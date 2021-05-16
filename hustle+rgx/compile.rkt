#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" a86/ast)

;; for testing purposes
(require "parse.rkt" "regex.rkt")

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define r8  'r8)  ; scratch in +, -
(define r9  'r9)  ; scratch in assert-type
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

(define global-s-to-l '())
(define regex-return-true (gensym))
(define regex-return-false (gensym))
;; type CEnv = [Listof Variable]

;; Expr -> Asm
(define (compile e)
  (let ((regex-assembly (compile-dfa-defines (get-dfas e))))
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        (Mov rbx rdi) ; recv heap pointer
        (compile-e e '())
        (Ret)
        (Label regex-return-true)
        (Mov rax val-true)
        (Ret)
        (Label regex-return-false)
        (Mov rax val-false)
        (Ret)
        regex-assembly))
)


;; helper function finds all dfas in the AST and store in list
(define (get-dfas e)
  (match e
    [(DFA _ _ _ _ _)    (list e)]
    [(Prim1 p e)        (get-dfas e)]
    [(Prim2 p e1 e2)    (append (get-dfas e1) (get-dfas e2))]
    [(Begin e1 e2)      (append (get-dfas e1) (get-dfas e2))]
    [(Let x e1 e2)      (append (get-dfas e1) (get-dfas e2))]
    [(If e1 e2 e3)      (append (get-dfas e1) (get-dfas e2) (get-dfas e3))]
    [_                  '()]))

;; compile each dfa into assembly
(define (compile-dfa-defines dfas)
  (match dfas
    ['() (seq)]
    [(cons dfa dfas)  
      (seq (compile-dfa-define dfa)
          (compile-dfa-defines dfas))]) 
)

(define (compile-dfa-define dfa)
  (match dfa
  [(DFA sigma states start accepts delta)
        (begin (set! global-s-to-l (append global-s-to-l (states-to-labels states)))
          (compile-dfa-states states start accepts delta)
        )])
)

(define (compile-dfa-states states start accepts delta)
  (match states
    ['()          (seq)]
    [(cons state l)   
      (seq (compile-dfa-state state start accepts delta)
           (compile-dfa-states l start accepts delta))]
  )
)

(define (compile-dfa-state state start accepts delta)
  (let ((l1 (gensym))
        (l2 (gensym)))
  (seq
              ;; String pointer found in rax
              ;; str length found in r9
              (Label (get-label state))

              ;; if end of string
              (Cmp r9 0)
              (Jne l1)
              (compile-dfa-state-final state accepts)
              
              ;; else if not end of string
              (Label l1)
              ;; load curr char into r8
              (Sub r9 1)
              (Add rax 8)
              (Mov r8 (Offset rax 0))
              (compile-dfa-state-immt-char (get-state-transitions state delta))
              (compile-dfa-state-immt-wild (get-state-transitions state delta))
  ))
)

(define (compile-dfa-state-final state accepts)
  (if (member state accepts) 
    ;; if the is final state, then jump to TRUE
    (seq        (Jmp regex-return-true))
    ;; else jump to FALSE
    (seq         (Jmp regex-return-false))
  )
)

(define (compile-dfa-state-immt-char state-transitions)
  ;; for each state, create jumps to other states given the transitions
  ;; if char, then jump
  ;; convert char to bits (imm->bits)
  (match state-transitions
    ['() (seq)]
    [(cons (list trans end) l)  
      (if (not (equal? trans (Wild)))
        (seq 
          (Cmp r8 (imm->bits trans))
          (Je (get-label end))
          (compile-dfa-state-immt-char l)
        )
        (compile-dfa-state-immt-char l)
      )]
  )
)

(define (compile-dfa-state-immt-wild state-transitions)
  ;; if wildcard, then jump as long as non-empty string
  ;; else, jump to false immediately
  (match state-transitions
    ['() (seq (Jmp regex-return-false))]
    [(cons (list trans end) l) 
      (if (equal? trans (Wild)) 
        (seq (Jmp (get-label end)))
        (compile-dfa-state-immt-wild l)
      )]
  )
)

;; helper function that returns all transitions from a specific state
(define (get-state-transitions state delta)
  (match delta
    ['() '()]
    [(cons (list start_state trans end_state) l)  
      (if (equal? state start_state) 
        (cons (list trans end_state) (get-state-transitions state l))
        (get-state-transitions state l)
      )]
  )
)

;; helper function that maps all states in dfa to a unique gensym label
(define (states-to-labels states) 
  (match states
    ['() '()]
    [(cons state l)
      (cons (list state (gensym)) (states-to-labels l))])
)

;; helper function to look-up label mapped to state
(define (get-label state)
  (foldl (lambda (v l) 
      (match v
      [(list s label)
        (if (equal? s state) label l)])) null global-s-to-l))

;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(String s)         (compile-string s)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)      (compile-begin e1 e2 c)]
    [(Let x e1 e2)      (compile-let x e1 e2 c)]
    [(DFA _ _ _ _ _)    (compile-dfa e)]))

;; returns effective address of the starting state of dfa
(define (compile-dfa dfa)
  (match dfa
  [(DFA _ _ start _ _)
    (seq (Lea rax (get-label start)))
    ])
)

;; DONE: Compile string
(define (compile-string s)
  ;; DONE: Replace with your solution
  (seq 
     (Mov rax (imm->bits (string-length s)))
     (Mov (Offset rbx 0) rax)
     (Mov rax rbx)
     (Or rax type-string)
     (Add rbx 8)
     (compile-string-aux s 0)
    )

)  

(define (compile-string-aux s i)
  (if (< i (string-length s)) 
        (seq  (Mov r8 (imm->bits (string-ref s i)))
              (Mov (Offset rbx 0) r8)
              (Add rbx 8)
              (compile-string-aux s (add1 i)))
        (seq))
)

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))       
    (seq (Mov rax (Offset rsp i)))))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (match p
    ['void      (seq (Mov rax val-void))]
    ['read-byte (seq (pad-stack c)
                     (Call 'read_byte)
                     (unpad-stack c))]
    ['peek-byte (seq (pad-stack c)
                     (Call 'peek_byte)
                     (unpad-stack c))]))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (match p
         ['add1
          (seq (assert-integer rax)
               (Add rax (imm->bits 1)))]
         ['sub1
          (seq (assert-integer rax)
               (Sub rax (imm->bits 1)))]         
         ['zero?
          (let ((l1 (gensym)))
            (seq (assert-integer rax)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['char?
          (let ((l1 (gensym)))
            (seq (And rax mask-char)
                 (Xor rax type-char)
                 (Cmp rax 0)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['char->integer
          (seq (assert-char rax)
               (Sar rax char-shift)
               (Sal rax int-shift))]
         ['integer->char
          (seq (assert-codepoint #f)
               (Sar rax int-shift)
               (Sal rax char-shift)
               (Xor rax type-char))]
         ['eof-object? (eq-imm val-eof)]
         ['write-byte
          (seq assert-byte
               (pad-stack c)
               (Mov rdi rax)
               (Call 'write_byte)
               (unpad-stack c)
               (Mov rax val-void))]
         ['box
          (seq (Mov (Offset rbx 0) rax)
               (Mov rax rbx)
               (Or rax type-box)
               (Add rbx 8))]
         ['unbox
          (seq (assert-box rax)
               (Xor rax type-box)
               (Mov rax (Offset rax 0)))]
         ['car
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 8)))]
         ['cdr
          (seq (assert-cons rax)
               (Xor rax type-cons)
               (Mov rax (Offset rax 0)))]
         ['empty? (eq-imm val-empty)]
         ;; DONE: string-length 
         ;; DONE: string?
         ['string?       
          (let ((l1 (gensym)))
            (seq (And rax ptr-mask)
                 (Cmp rax type-string)
                 (Mov rax val-true)
                 (Je l1)
                 (Mov rax val-false)
                 (Label l1)))]
         ['string-length 
            (seq    (assert-string rax)
                    (Xor rax type-string)
                    (Mov rax (Offset rax 0)))]
         )))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (match p
         ['+
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Add rax r8))]
         ['-
          (seq (Pop r8)
               (assert-integer r8)
               (assert-integer rax)
               (Sub r8 rax)
               (Mov rax r8))]
         ['eq?
          (let ((l (gensym)))
            (seq (Cmp rax (Offset rsp 0))
                 (Mov rax val-true)
                 (Je l)
                 (Mov rax val-false)
                 (Label l)
                 (Add rsp 8)))]
         ['cons
          (seq (Mov (Offset rbx 0) rax)
               (Pop rax)
               (Mov (Offset rbx 8) rax)
               (Mov rax rbx)
               (Or rax type-cons)
               (Add rbx 16))]
         ;; DONE: string-ref 
         ;; DONE: make-string
         ['string-ref  
            (seq   (Pop r8)
                    (assert-string r8)
                    (assert-integer rax)
                    (Cmp rax 0)
                    (Jl 'raise_error)
                    (Xor r8 type-string)
                    (Cmp rax (Offset r8 0))
                    (Je 'raise_error)
                    (Jg 'raise_error)
                    (Add r8 8)
                    (Sar rax 4)
                    (Sal rax 3)
                    (Add r8 rax)
                    (Mov rax (Offset r8 0))
                    )] 
         ['make-string 
          (let ((l1 (gensym))
                (l2 (gensym)))
            (seq    (Pop r8)
                    (assert-char rax)
                    (assert-integer r8)
                    (Cmp r8 0)
                    (Jl 'raise_error)
                    (Mov r9 rax)
                    (Mov (Offset rbx 0) r8)
                    (Mov rax rbx)
                    (Or rax type-string)
                    (Add rbx 8)
                    (Label l1)
                    (Cmp r8 0) 
                    (Je l2)
                    (Mov (Offset rbx 0) r9)
                    (Add rbx 8)
                    (Sub r8 1)
                    (Jmp l1)
                    (Label l2)))]
          ['regexp-match?
            ;; make sure stack is 16-byte aligned
            ;; https://www.cs.umd.edu/class/spring2021/cmsc430/Iniquity.html
            (if (even? (length c))
              ;Stack will be 16-byte aligned
              (seq
                  ;; string address stays in rax
                  (assert-string rax)
                  (Xor rax type-string)
                  ;; push the length of string onto r9
                  (Mov r9 (Offset rax 0))
                  (Sar r9 4)
                  ;; jump to regex, current address stored on stack using (Call) for (Ret)
                  (Pop r8)
                  (Call r8)
              )

              ;; TODO fix stack alignment

              ; stack will not be 16 byte aligned
              (seq 
                  (assert-string rax)
                  (Xor rax type-string)
                  (Mov r9 (Offset rax 0))
                  (Sar r9 4)
                  (Pop r8)
                  (Sub rsp 8)
                  (Call r8)
                  (Add rsp 8)
              )
            
            )
          ]
         )))

;; Imm -> Asm
(define (eq-imm imm)
  (let ((l1 (gensym)))
    (seq (Cmp rax imm)
         (Mov rax val-true)
         (Je l1)
         (Mov rax val-false)
         (Label l1))))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; CEnv -> Asm
;; Pad the stack to be aligned for a call
(define (pad-stack c)
  (match (even? (length c))
    [#t (seq (Sub rsp 8))]
    [#f (seq)]))

;; CEnv -> Asm
;; Undo the stack alignment after a call
(define (unpad-stack c)
  (match (even? (length c))
    [#t (seq (Add rsp 8))]
    [#f (seq)]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

(define (assert-type mask type)
  (λ (arg)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Jne 'raise_error))))

(define (type-pred mask type)
  (let ((l (gensym)))
    (seq (And rax mask)
         (Cmp rax type)
         (Mov rax (imm->bits #t))
         (Je l)
         (Mov rax (imm->bits #f))
         (Label l))))
         
(define assert-integer
  (assert-type mask-int type-int))
(define assert-char
  (assert-type mask-char type-char))
(define assert-box
  (assert-type ptr-mask type-box))
(define assert-cons
  (assert-type ptr-mask type-cons))
(define assert-string
  (assert-type ptr-mask type-string))

(define (assert-codepoint x)
  (let ((ok (gensym)))
    (seq (assert-integer rax)
         (Cmp rax (imm->bits 0))
         (Jl 'raise_error)
         (Cmp rax (imm->bits 1114111))
         (Jg 'raise_error)
         (Cmp rax (imm->bits 55295))
         (Jl ok)
         (Cmp rax (imm->bits 57344))
         (Jg ok)
         (Jmp 'raise_error)
         (Label ok))))
       
(define assert-byte
  (seq (assert-integer rax)
       (Cmp rax (imm->bits 0))
       (Jl 'raise_error)
       (Cmp rax (imm->bits 255))
       (Jg 'raise_error)))
       

