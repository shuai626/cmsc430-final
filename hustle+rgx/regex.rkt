#lang racket
(provide (all-defined-out))
(require "ast.rkt")

;; Contains the logic to tokenize the regular expression, convert to an NFA,
;; and convert the NFA to a DFA

;; Add to this as needed hehe :)

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

(struct Empty_String ()           #:prefab)
(struct Reg_Char     (c)          #:prefab)
(struct Union        (rxp1 rxp2)  #:prefab)
(struct Concat       (rxp1 rxp2)  #:prefab)
(struct Star         (rxp)        #:prefab)
(struct Wild         ()           #:prefab)
(struct Question     (rxp)        #:prefab)

;; type Token = (Tok_Char Character)
;;            | (Tok_Epsilon)
;;            | (Tok_Union)
;;            | (Tok_Star)
;;            | (Tok_Wild)
;;            | (Tok_LParen)
;;            | (Tok_RParen)
;;            | (Tok_END)

(struct Tok_Char     (c)          #:prefab)
(struct Tok_Union    ()           #:prefab)
(struct Tok_Star     ()           #:prefab)
(struct Tok_Wild     ()           #:prefab)
(struct Tok_Question ()           #:prefab)
(struct Tok_LParen   ()           #:prefab)
(struct Tok_RParen   ()           #:prefab)
(struct Tok_END      ()           #:prefab)

;; Basic pattern for tokenizing
(define (tokenize str)
  (let ((re_var (regexp "[a-z]")))
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
              [(string=? c "(")           (cons (Tok_LParen) (tok (add1 pos) s))]
              [(string=? c ")")           (cons (Tok_RParen) (tok (add1 pos) s))]
              [(error "Tokenize error")]
            ))))
    (tok 0 str)))

;;   S -> A Tok_Union S | A
;;   A -> B A | B
;;   B -> C Tok_Star | C Tok_Question | C
;;   C -> Tok_Char | Tok_Wild | Tok_LParen S Tok_RParen
;;   FIRST(S) = Tok_Char | Tok_Wild | Tok_LParen
;;   FIRST(A) = Tok_Char | Tok_Wild | Tok_LParen
;;   FIRST(B) = Tok_Char | Tok_Wild | Tok_LParen
;;   FIRST(C) = Tok_Char | Tok_Wild | Tok_LParen

(define (parse-regexp tok-list)
  (define (lookahead toks)
    (match toks
      ['()    (error "lookahead error")]
      [(cons x xs) (values x xs)]
    )
  )
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
        [_  (values a1 l1)])))
  (define (parse_B l)
    (let*-values (((a1 l1) (parse_C l))
                 ((t n)   (lookahead l1)))
      (match t
        [(Tok_Star) (values (Star a1) n)]
        [(Tok_Question) (values (Question a1) n)]
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
        [_  (error "parse-C error 2")])))
  (let-values (((rxp toks) (parse_S tok-list)))
    (match toks
      [(list Tok_END)    rxp]
      [_            (error "parse-regex error")])))

(define (string-to-regexp str)
  (if (string=? str "")
      (Empty_String)
      (parse-regexp (tokenize str))))
