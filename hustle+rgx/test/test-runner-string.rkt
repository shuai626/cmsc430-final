#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run) 
  (check-equal? (run "") "")
  (check-equal? (run " ") " ")
  (check-equal? (run "\"") "\"")
  (check-equal? (run "FFFF") "FFFF")
  (check-equal? (run "FGE") "FGE")
  
  (check-equal? (run '(string? "F ")) #t)
  (check-equal? (run '(string? "")) #t)
  (check-equal? (run '(string? " ")) #t)
  (check-equal? (run '(string? "\"")) #t)
  (check-equal? (run '(string? #f)) #f)

  (check-equal? (run '(string-length "")) 0)
  (check-equal? (run '(string-length "F")) 1)
  (check-equal? (run '(string-length " ")) 1)
  (check-equal? (run '(string-length "F ")) 2)
  (check-equal? (run '(string-length "\"")) 1)
  (check-equal? (run '(string-length "FF")) 2)
  (check-equal? (run '(string-length 0)) 'err)

  (check-equal? (run '(string-ref 0 0)) 'err)
  (check-equal? (run '(string-ref "" -1)) 'err)
  (check-equal? (run '(string-ref "" 1)) 'err)
  (check-equal? (run '(string-ref "" 0)) 'err)
  (check-equal? (run '(string-ref " " 0)) #\space)
  (check-equal? (run '(string-ref "F" -1)) 'err)
  (check-equal? (run '(string-ref "F" 2)) 'err)
  (check-equal? (run '(string-ref "F" 1)) 'err)
  (check-equal? (run '(string-ref "F" 0)) #\F)
  (check-equal? (run '(string-ref "FGE" -1)) 'err)
  (check-equal? (run '(string-ref "FGE" 0)) #\F)
  (check-equal? (run '(string-ref "FGE" 1)) #\G)
  (check-equal? (run '(string-ref "FGE" 2)) #\E)
  (check-equal? (run '(string-ref "FGE" 3)) 'err)

  (check-equal? (run '(make-string -1 #\f)) 'err)
  (check-equal? (run '(make-string 0 1)) 'err)
  (check-equal? (run '(make-string 0 #\f)) "")
  (check-equal? (run '(make-string 3 #\f)) "fff")
  (check-equal? (run '(make-string 3 #\")) "\"\"\"")

  (check-equal? (run '(string? (make-string 3 #\"))) #t)
  (check-equal? (run '(string? (string-length "F"))) #f)
  (check-equal? (run '(string-ref "FGE" (- (string-length "FGE") 1))) #\E)
  (check-equal? (run '(make-string (string-length "FGE") (string-ref "FGE" (- (string-length "FGE") 1)))) "EEE")


  ;; Public string test
  (check-equal?
   (run
    '(let ((x "FFF"))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42)))
   #\F)
   

  ;; Public string test
  (check-equal?
   (run
    '(let ((x (make-string 3 #\f)))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42)))
   #\f)
)

(define (test-runner-io run)
  (check-equal?
   (run
    '(let ((x (make-string 3 (integer->char (read-byte)))))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42))
    "L")
   (cons #\L ""))
)
