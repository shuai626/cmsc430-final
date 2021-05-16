#lang racket
(provide parse)
(require "ast.rkt"
         "regex.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (String s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]           
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse e))]

    ;; Added special condition to parse Regex
    [(list (? (op? '(regex-match-exact?)) p2) (? string? e1) (? string? e2)) (parse-regex-match e1 e2)]

    ;; Added special condition to parse substr Regex
    ;; TODO add parenthesis around string if not already there
    [(list (? (op? '(regex-match?)) p2) (? string? e1) (? string? e2)) 
        (cond
          [(equal? (string-length e1) 0)      (parse-regex-match (string-append ".*" e1 ".*") e2)]
          [(equal? (string-ref e1 0) #\( )    (parse-regex-match (string-append ".*" e1 ".*") e2)]
          [else                               (parse-regex-match (string-append ".*(" e1 ").*") e2)]
        )]

    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse e1) (parse e2))]
    [(list 'begin e1 e2)
     (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse e1) (parse e2))]
    [_ (error "Parse error" s)]))

;; Added function to parse regex-match
(define (parse-regex-match regex str) 
  (Prim2 'regex-match? (nfa-to-dfa (regexp-to-nfa (string-to-regexp regex))) (String str))
)

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer box unbox empty? car cdr
         string? string-length
         ))
(define op2
  '(+ - eq? cons
      string-ref make-string
      ))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
