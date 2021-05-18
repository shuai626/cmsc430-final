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

    ;; Parse regex for an exact match
    [(list (? (op? '(regexp-match-exact?)) p2) (? string? e1) e2) (parse-regex-match e1 e2)]

    ;; Parse regex for substring matching, preprocess for ^ and $
    [(list (? (op? '(regexp-match?)) p2) (? string? e1) e2)
        (if (equal? (string-length e1) 0)
          (parse-regex-match (string-append ".*" e1 ".*") e2)
          (let ((anchor1 (string-ref e1 0))
                (anchor2 (string-ref e1 (sub1 (string-length e1)))))
            (cond
              [(and (equal? anchor1 #\^) (equal? anchor2 #\$)) ;; anchored at both
               (parse-regex-match (substring e1 1 (sub1 (string-length e1))) e2)]
              [(equal? anchor1 #\^) ;; anchored only at beginning
               (if (string=? (substring e1 1) "")
                   (parse-regex-match ".*" e2)
                   (parse-regex-match (string-append "(" (substring e1 1) ").*") e2))]
              [(equal? anchor2 #\$) ;; anchored only at end
               (if (string=? (substring e1 0 (sub1 (string-length e1))) "")
                   (parse-regex-match ".*" e2)
                   (parse-regex-match (string-append ".*(" (substring e1 0 (sub1 (string-length e1))) ")") e2))]
              [else
               (parse-regex-match (string-append ".*(" e1 ").*") e2)])))]

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
  (cond
  [(string? str)      (Prim2 'regexp-match? (brzozowski-minimization (nfa-to-dfa (regexp-to-nfa (string-to-regexp regex)))) (String str))]
  [(symbol? str)      (Prim2 'regexp-match? (brzozowski-minimization (nfa-to-dfa (regexp-to-nfa (string-to-regexp regex)))) (Var str))]
  [else (error "Regex parse error")]
  )
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
