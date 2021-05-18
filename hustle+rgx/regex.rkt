#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Contains the logic to tokenize the regular expression, convert to an NFA,
;; and convert the NFA to a DFA

;; DFA defined in ast.rkt
;; (NFA Sigma States Start Final Transitions)
(struct NFA          (s q q0 f t) #:prefab)

;; type Regex = (Empty_String)
;;            | (Reg_Char Character)
;;            | (Union Regex Regex)
;;            | (Concat Regex Regex)
;;            | (Star Regex)
;;            | (Wild)
;;            | (Question Regex)
;;            | (Plus Regex)
;;            | (Quantifier Range Regex)
;;            | (Class list of Reg_Char)

(struct Empty_String ()           #:prefab)
(struct Reg_Char     (c)          #:prefab)
(struct Union        (rxp1 rxp2)  #:prefab)
(struct Concat       (rxp1 rxp2)  #:prefab)
(struct Star         (rxp)        #:prefab)
(struct Wild         ()           #:prefab)
(struct Question     (rxp)        #:prefab)
(struct Plus         (rxp)        #:prefab)
(struct Quantifier   (range rxp)  #:prefab)
(struct Range        (low high)   #:prefab)
(struct Class        (l)          #:prefab)

;; type Token = (Tok_Char Character)
;;            | (Tok_Epsilon)
;;            | (Tok_Union)
;;            | (Tok_Star)
;;            | (Tok_Wild)
;;            | (Tok_Plus)
;;            | (Tok_LParen)
;;            | (Tok_RParen)
;;            | (Tok_LCurly)
;;            | (Tok_RCurly)
;;            | (Tok_Comma)
;;            | (Tok_LBrace)
;;            | (Tok_RBrace)
;;            | (Tok_Hyphen)
;;            | (Tok_END)

(struct Tok_Char     (c)          #:prefab)
(struct Tok_Union    ()           #:prefab)
(struct Tok_Star     ()           #:prefab)
(struct Tok_Wild     ()           #:prefab)
(struct Tok_Question ()           #:prefab)
(struct Tok_Plus     ()           #:prefab)
(struct Tok_LParen   ()           #:prefab)
(struct Tok_RParen   ()           #:prefab)
(struct Tok_LCurly   ()           #:prefab)
(struct Tok_RCurly   ()           #:prefab)
(struct Tok_Comma    ()           #:prefab)
(struct Tok_LBrace   ()           #:prefab)
(struct Tok_RBrace   ()           #:prefab)
(struct Tok_Hyphen   ()           #:prefab)
(struct Tok_END      ()           #:prefab)

;; Basic pattern for tokenizing
(define (tokenize str)
  (let ((re_var (regexp "[A-Za-z0-9]")))
    (define (tok pos s)
      (if (>= pos (string-length s))
          (list (Tok_END)) ;; end of string
          (let ((c (substring s pos (add1 pos))))
            (cond
              [(regexp-match? re_var c)   (cons (Tok_Char (string-ref s pos)) (tok (add1 pos) s))]
              [(string=? c "|")           (cons (Tok_Union) (tok (add1 pos) s))]
              [(string=? c "*")           (cons (Tok_Star) (tok (add1 pos) s))]
              [(string=? c ".")           (cons (Tok_Wild) (tok (add1 pos) s))]
              [(string=? c "?")           (cons (Tok_Question) (tok (add1 pos) s))]
              [(string=? c "+")           (cons (Tok_Plus) (tok (add1 pos) s))]
              [(string=? c "(")           (cons (Tok_LParen) (tok (add1 pos) s))]
              [(string=? c ")")           (cons (Tok_RParen) (tok (add1 pos) s))]
              [(string=? c "{")           (cons (Tok_LCurly) (tok (add1 pos) s))]
              [(string=? c "}")           (cons (Tok_RCurly) (tok (add1 pos) s))]
              [(string=? c ",")           (cons (Tok_Comma) (tok (add1 pos) s))]
              [(string=? c "[")           (cons (Tok_LBrace) (tok (add1 pos) s))]
              [(string=? c "]")           (cons (Tok_RBrace) (tok (add1 pos) s))]
              [(string=? c "-")           (cons (Tok_Hyphen) (tok (add1 pos) s))]
              [(error "Tokenize error")]
            ))))
    (tok 0 str)))

;;   S -> A Tok_Union S | A
;;   A -> B A | B
;;   B -> C Tok_Star | C Tok_Question | C Tok_Plus | C Tok_LCurly D | C
;;   C -> Tok_Char | Tok_Wild | Tok_LParen S Tok_RParen | Tok_LBrace G Tok_RBrace
;;   D -> Tok_RCurly | E Tok_RCurly
;;   E -> F Tok_Comma F | Tok_Comma F | F
;;   F -> Tok_Char F | Tok_Char
;;   G -> H G | H
;;   H -> Tok_Char Tok_Hyphen TokChar | TokChar
;;   FIRST(S) = Tok_Char | Tok_Wild | Tok_LParen | Tok_LBrace
;;   FIRST(A) = Tok_Char | Tok_Wild | Tok_LParen | Tok_LBrace
;;   FIRST(B) = Tok_Char | Tok_Wild | Tok_LParen | Tok_LBrace
;;   FIRST(C) = Tok_Char | Tok_Wild | Tok_LParen | Tok_LBrace
;;   FIRST(D) = Tok_RCurly | Tok_Char | Tok_Comma
;;   FIRST(E) = Tok_Char | Tok_Comma
;;   FIRST(F) = Tok_Char
;;   FIRST(G) = Tok_Char
;;   FIRST(H) = Tok_Char

(define (parse-regexp tok-list)
  (define (lookahead toks)
    (match toks
      ['()    (error "lookahead error")]
      [(cons x xs) (values x xs)]))
  (define (parse_S l)
    (let*-values (((a1 l1) (parse_A l))
                 ((t n)   (lookahead l1)))
      (match t
        [(Tok_Union)
         (let-values (((a2 l2) (parse_S n)))
            (values (Union a1 a2) l2))]
        [_  (values a1 l1)])))
  (define (parse_A l)
    (let*-values (((a1 l1) (parse_B l))
                 ((t n)   (lookahead l1)))
      (match t
        [(Tok_Char c)
         (let-values (((a2 l2) (parse_A l1)))
            (values (Concat a1 a2) l2))]
        [(Tok_Wild)
         (let-values (((a2 l2) (parse_A l1)))
            (values (Concat a1 a2) l2))]
        [(Tok_LParen)
          (let-values (((a2 l2) (parse_A l1)))
            (values (Concat a1 a2) l2))]
        [(Tok_LBrace)
          (let-values (((a2 l2) (parse_A l1)))
            (values (Concat a1 a2) l2))]
        [_  (values a1 l1)])))
  (define (parse_B l)
    (let*-values (((a1 l1) (parse_C l))
                 ((t n)   (lookahead l1)))
      (match t
        [(Tok_Star) (values (Star a1) n)]
        [(Tok_Question) (values (Question a1) n)]
        [(Tok_Plus) (values (Plus a1) n)]
        [(Tok_LCurly)
          (let-values (((a2 l2) (parse_D n)))
            (values (Quantifier a2 a1) l2))]
        [_  (values a1 l1)])))
  (define (parse_C l)
    (let-values (((t n)   (lookahead l)))
      (match t
        [(Tok_Char c) (values (Reg_Char c) n)]
        [(Tok_Wild)   (values (Wild) n)]
        [(Tok_LParen)
         (let*-values (((a1 l1) (parse_S n))
                      ((t2 n2) (lookahead l1)))
          (match t2
            [(Tok_RParen)   (values a1 n2)]
            [_              (error "parse-C error 1")]))]
        [(Tok_LBrace)
         (let*-values (((a1 l1) (parse_G n))
                      ((t2 n2) (lookahead l1)))
          (match t2
            [(Tok_RBrace)   (values (Class a1) n2)]
            [_              (error "parse-C error 2")]))]
        [_  (error "parse-C error 3")])))
  (define (parse_D l)
    (let-values (((t n) (lookahead l)))
      (match t
        ;; unbounded on either end, same as *
        [(Tok_RCurly) (values (Range null null) n)]
        [(Tok_Char c)
          (let*-values (((a1 l1) (parse_E l))
                      ((t2 n2)  (lookahead l1)))
            (match t2
              [(Tok_RCurly) (values a1 n2)]
              [_             (error "parse-D error 1")]))]
        [(Tok_Comma)
          (let*-values (((a1 l1) (parse_E l))
                      ((t2 n2)  (lookahead l1)))
            (match t2
              [(Tok_RCurly) (values a1 n2)]
              [_             (error "parse-D error 1")]))]
        [_  (error "parse-D error 2")])))
  (define (parse_E l)
    (let-values (((t n) (lookahead l)))
      (match t
        [(Tok_Char _)
          (let*-values (((a1 l1) (parse_F l))
                       ((t2 n2) (lookahead l1)))
            (match t2
              [(Tok_Comma)
                (let-values (((t3 n3) (lookahead n2)))
                  (match t3
                    ;; both sides bounded
                    [(Tok_Char _)
                      (let-values (((a2 l2) (parse_F n2)))
                        (let ((low (num a1))
                              (high (num a2)))
                          (if (> low high)
                              (error "invalid range ordering")
                              (values (Range low high) l2))))]
                    ;; bounded on left side
                    [_ (values (Range (num a1) null) n2)]))]
              ;; exact num instances
              [_ (values (Range (num a1) (num a1)) l1)]))]
        [(Tok_Comma)
          (let-values (((t2 n2) (lookahead n)))
            (match t2
              ;; bounded on right side
              [(Tok_Char _)
                (let*-values (((a1 l1) (parse_F n)))
                  (values (Range null (num a1)) l1))]
              [_           (error "parse-E error 1")]))]
        [_  (error "parse-E error 2")])))
  (define (parse_F l)
    (let-values (((t n) (lookahead l)))
      (match t
        [(Tok_Char c)
          (let-values (((a1 l1) (parse_F n)))
            (values (string-append (make-string 1 c) a1) l1))]
        [_  (values "" l)])))
  (define (parse_G l)
    (let*-values (((a1 l1) (parse_H l))
                 ((t n)   (lookahead l1)))
      (match t
        [(Tok_Char c)
         (let-values (((a2 l2) (parse_G l1)))
            (values (set-union a1 a2) l2))]
        [_  (values a1 l1)])))
  (define (parse_H l)
    (let*-values (((t n) (lookahead l))
                 ((t2 n2) (lookahead n)))
      (match* (t t2)
        [((Tok_Char c) (Tok_Hyphen))
          (let-values (((t3 n3) (lookahead n2)))
            (match t3
              [(Tok_Char c2) (values (range-list c c2) n3)]
              [_             (error "parse-H error 1")]))]
        [((Tok_Char c) _)    (values (list c) n)]
        [(_ _) (error "parse-H error 2")])))
  ;; helper used in parse_E to convert to a number for the range
  (define (num str)
    (let ((number (string->number str)))
      ;; check it is a number
      (if number
          number
          (error "bad quantifier range value"))))
  ;; helper used in parse_H to convert a character range to a list of valid characters
  (define (range-list c1 c2)
    (define (gen-char x maximum)
      (if (> x maximum)
          '()
          (cons (integer->char x) (gen-char (add1 x) maximum))))
    ;; get the codepoints for the range
    (let ((cp1 (char->integer c1))
          (cp2 (char->integer c2)))
      (if (< cp2 cp1)
          (error "bad character class range")
          (gen-char cp1 cp2))))
  (let-values (((rxp toks) (parse_S tok-list)))
    (match toks
      [(list Tok_END)    rxp]
      [_            (error "parse-regex error")])))

(define (string-to-regexp str)
  (if (string=? str "")
      (Empty_String)
      (parse-regexp (tokenize str))))

(define (union list1 list2)
  (remove-duplicates (append list1 list2)))

(define (unwind-states start_states end_state)
  (foldl (lambda (v l) (cons (list v null end_state) l)) '() start_states))

(define (regexp-to-nfa regexp) 
  (match regexp 
    [(Empty_String)         
      (let ((x (gensym))) 
        (NFA '() (list x) x (list x) '())) ]
    [(Reg_Char c)             
      (let ((s0 (gensym)) (s1 (gensym))) 
        (NFA (list c) (list s0 s1) s0 (list s1) (list (list s0 c s1))))]
    [(Union rxp1 rxp2)      
      (let ((A (regexp-to-nfa rxp1))
            (B (regexp-to-nfa rxp2))
            (s0 (gensym)) 
            (s1 (gensym)))
        (match A
          [(NFA asigma astates astart aaccepts adelta) 
            (match B
              [(NFA bsigma bstates bstart baccepts bdelta) 
                  (NFA (union asigma bsigma)
                        (union (list s0 s1) (union astates bstates))
                        s0
                        (list s1)
                        (union adelta (union bdelta (union (list (list s0 null astart) (list s0 null bstart))  (union (unwind-states aaccepts s1) (unwind-states baccepts s1)))))
                        )])]))]
    [(Concat rxp1 rxp2)     
      (let ((A (regexp-to-nfa rxp1))
            (B (regexp-to-nfa rxp2)))
        (match A
          [(NFA asigma astates astart aaccepts adelta) 
            (match B
              [(NFA bsigma bstates bstart baccepts bdelta) 
                  (NFA (union asigma bsigma)
                        (union astates bstates)
                        astart
                        baccepts
                        (union adelta (union bdelta (unwind-states aaccepts bstart)))
                        )])]))]
    [(Star rxp)             
      (let ((A (regexp-to-nfa rxp))
            (s0 (gensym)) 
            (s1 (gensym)))
          (match A
            [(NFA asigma astates astart aaccepts adelta) 
                    (NFA asigma
                          (union astates (list s0 s1))
                          s0
                          (list s1)
                          (union adelta (union (list (list s0 null astart) (list s0 null s1) (list s1 null s0)) (unwind-states aaccepts s1)))
                          )]))]
    [(Wild)                 
      (let ((s0 (gensym)) (s1 (gensym))) 
        (NFA (list (Wild)) (list s0 s1) s0 (list s1) (list (list s0 (Wild) s1))))]
    [(Question rxp)         
      (let ((A (regexp-to-nfa rxp))
      (s0 (gensym)) 
      (s1 (gensym)))
          (match A
            [(NFA asigma astates astart aaccepts adelta) 
                    (NFA asigma
                          (union astates (list s0 s1))
                          s0
                          (list s1)
                          (union adelta (union (list (list s0 null astart) (list s0 null s1)) (unwind-states aaccepts s1)))
                          )]))]
    [(Plus rxp) (regexp-to-nfa (Concat rxp (Star rxp)))]
    [(Quantifier r rxp)
      (let ((A (regexp-to-nfa rxp))
            (s0 (gensym))
            (s1 (gensym)))
        (match r
        ;; (Range null null) == {}
        [(Range '() '())                              
          (match A
            [(NFA asigma astates astart aaccepts adelta) 
                (NFA asigma
                (union astates (list s0 s1))
                s0
                (list s1)
                (union adelta (union (list (list s0 null astart) (list s0 null s1) (list s1 null s0)) (unwind-states aaccepts s1)))
                )])]
        ;; (Range null int) == {,int}
        [(Range '() int) #:when(integer? int)                      (range-to-nfa rxp 0 int)]
        ;; (Range int null) == {int,}
        [(Range int '()) #:when(integer? int)                               
          (match (range-to-nfa rxp int int)
            [(NFA sigma states start accepts delta) 
              (match A
                [(NFA asigma astates astart aaccepts adelta)
                    (NFA (union asigma sigma)
                      (union astates (union states  (list s0 s1)))
                      start
                      (list s1)
                      (union delta (union adelta (union (unwind-states accepts s0) (union (list (list s0 null astart) (list s0 null s1) (list s1 null s0)) (unwind-states aaccepts s1)))))
                    )
                ]
              )
            ])]
        ;; (Range int1 int2) | (int1 != int2) == {int1,int2}
        [(Range int1 int2)                              (range-to-nfa rxp int1 int2)]
        ))]
    [(Class rxps)
      (let* ((s0 (gensym))
            (s1 (gensym))
            (trans (foldl (lambda (c l) (cons (list s0 c s1) l)) '() rxps)))
             
        (NFA rxps (list s0 s1) s0 (list s1) trans))]
    ))

(define (range-to-nfa rxp lower upper)
  (let ((nfa (regexp-to-nfa rxp))
        (s0 (gensym))
        (s1 (gensym)))
    (if (>= 0 upper)
      (NFA '() (list s0) s0 (list s0) '())

      (let ((prev (range-to-nfa rxp (- lower 1) (- upper 1))))
        (match nfa
          [(NFA sigma states start accepts delta)
            (match prev
              [(NFA psigma pstates pstart paccepts pdelta)
                (if (>= 0 lower)
              
                (NFA (union sigma psigma)
                     (union states pstates)
                     start
                     (union (list start) (union accepts paccepts))
                     (union delta (union pdelta (unwind-states accepts pstart)))
                )
                
                (NFA (union sigma psigma)
                     (union states pstates)
                     start
                     paccepts
                     (union delta (union pdelta (unwind-states accepts pstart))))
                )])])))))

(define (eq-trans? t v)
  (match* (t v)
    [((Wild) x) #:when (not (equal? x null)) #t] ;; special case bc struct as transition
    ;; When moving from one state to another, also include if state = "WILD"
    [(_ _) (eq? t v)]))

(define (move nfa qs v)
  (define (reach state delta)
    (define (from trans)
      (match trans
        [(list start t dest)
         #:when (and (eq? start state) (eq-trans? t v))
         (list dest)]
        [_ '()]))
    (foldl (lambda (trans l) (union l (from trans))) '() delta))
  (match nfa
    [(NFA _ _ _ _ delta)
     (foldl (lambda (state l) (union l (reach state delta))) '() qs)]))

(define (e-closure nfa qs)
  (define (helper r rprime)
    (if (set=? (set r) (set rprime))
        rprime
        (helper rprime (union rprime (move nfa rprime null)))))
  (helper '() qs))

(define (new-states nfa qs)
  (match nfa
    [(NFA sigma _ _ _ _)
     (foldl (lambda (v l) (append l (list (e-closure nfa (move nfa qs v))))) '() sigma)]))

(define (new-trans nfa qs)
  (match nfa
    [(NFA sigma _ _ _ _)
     (map (lambda (v dest) (list qs v dest)) sigma (new-states nfa qs))]))

(define (new-finals nfa qs)
  (match nfa
    [(NFA _ _ _ accepts _)
     (if (null? (set-intersect accepts qs))
         '()
         (list qs))]))
 
(define (nfa-to-dfa-step nfa dfa work)
  (match dfa 
    [(DFA sigma states start accepts delta)
      (match work
        ['() dfa]
        [(cons h t)
          (let* ((new_states (remove* (list '()) (new-states nfa h)))
                (new_trans
                  (foldl (lambda (trans l)
                          (match trans
                            [(list start v dest)
                            (if (eq? dest '()) l (cons trans l))])) '() (new-trans nfa h)))
                (new_finals (new-finals nfa h))
                (new_dfa (DFA sigma (union states new_states) start (union accepts new_finals) (union delta new_trans))))
          (nfa-to-dfa-step nfa new_dfa (union t (remove* states new_states))))])]))

(define (nfa-to-dfa nfa) 
  (match nfa
    [(NFA sigma states start accepts delta) 
     (let* ((new_start (e-closure nfa (list start)))
            (dfa (DFA sigma (list new_start) new_start '() '())))
        (nfa-to-dfa-step nfa dfa (list new_start)))]))

(define (brzozowski-minimization dfa)
  (nfa-to-dfa (brzozowski-step  (nfa-to-dfa (brzozowski-step  dfa))))
)

(define (brzozowski-step dfa)
   ;; Reverse all edges, make initial state accept state, and accept states inital in NFA 
  (let ((s0 (gensym)))
    (match dfa
      [(DFA sigma states start accepts delta)
      (let* ((state-to-label (map (lambda (x) (list x (gensym))) states))
           (new_states (map (lambda (x) (match x [(list state label) label])) state-to-label))
           (new_start (get-label start state-to-label))
           (new_accepts  (map (lambda (x) (get-label x state-to-label)) accepts))
           (new_delta (map (lambda (x)  (match x
                  [(list start trans end) 
                    (list (get-label start state-to-label) trans (get-label end state-to-label))])) delta)))
        (NFA  sigma
              (union new_states (list s0))
              s0
              (list new_start)
              (map (lambda (x) (reverse x)) (union new_delta (unwind-states new_accepts s0))))
              )
      ])))

;; helper function to look-up label mapped to state
(define (get-label state labels)
  (foldl (lambda (v l) 
      (match v
      [(list s label)
        (if (equal? s state) label l)])) null labels))