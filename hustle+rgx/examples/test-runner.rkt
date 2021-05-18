#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run) 
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
  (check-equal? (run '(add1 (add1 7))) 9)
  (check-equal? (run '(add1 (sub1 7))) 7)

  ;; Con examples
  (check-equal? (run '(if (zero? 0) 1 2)) 1)
  (check-equal? (run '(if (zero? 1) 1 2)) 2)
  (check-equal? (run '(if (zero? -7) 1 2)) 2)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) 1 2)
                          7))
                2)
  (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                          (if (zero? 1) 1 2)
                          7))
                7)

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run (if #t 1 2)) 1)
  (check-equal? (run (if #f 1 2)) 2)
  (check-equal? (run (if 0 1 2)) 1)
  (check-equal? (run '(if #t 3 4)) 3)
  (check-equal? (run '(if #f 3 4)) 4)
  (check-equal? (run '(if  0 3 4)) 3)
  (check-equal? (run '(zero? 4)) #f)
  (check-equal? (run '(zero? 0)) #t)  
  ;; Dodger examples
  (check-equal? (run #\a) #\a)
  (check-equal? (run #\b) #\b)
  (check-equal? (run '(char? #\a)) #t)
  (check-equal? (run '(char? #t)) #f)  
  (check-equal? (run '(char? 8)) #f) 
  (check-equal? (run '(char->integer #\a)) (char->integer #\a))
  (check-equal? (run '(integer->char 955)) #\λ)  
  ;; Extort examples
  (check-equal? (run '(add1 #f)) 'err)
  (check-equal? (run '(sub1 #f)) 'err)
  (check-equal? (run '(zero? #f)) 'err)
  (check-equal? (run '(char->integer #f)) 'err)
  (check-equal? (run '(integer->char #f)) 'err)
  (check-equal? (run '(integer->char -1)) 'err)
  (check-equal? (run '(write-byte #f)) 'err)
  (check-equal? (run '(write-byte -1)) 'err)
  (check-equal? (run '(write-byte 256)) 'err)
  ;; Fraud examples
  (check-equal? (run '(let ((x 7)) x)) 7)
  (check-equal? (run '(let ((x 7)) 2)) 2)
  (check-equal? (run '(let ((x 7)) (add1 x))) 8)
  (check-equal? (run '(let ((x (add1 7))) x)) 8)
  (check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
  (check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
  (check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

  (check-equal? (run '(let ((x 0))
                        (if (zero? x) 7 8)))
                7)
  (check-equal? (run '(let ((x 1))
                        (add1 (if (zero? x) 7 8))))
                9)
  (check-equal? (run '(+ 3 4)) 7)
  (check-equal? (run '(- 3 4)) -1)
  (check-equal? (run '(+ (+ 2 1) 4)) 7)
  (check-equal? (run '(+ (+ 2 1) (+ 2 2))) 7)
  (check-equal? (run '(let ((x (+ 1 2)))
                        (let ((z (- 4 x)))
                          (+ (+ x x) z))))
                7)
  ;; Hustle examples  
  (check-equal? (run ''()) '())  
  (check-equal? (run '(box 1)) (box 1))
  (check-equal? (run '(cons 1 2)) (cons 1 2))
  (check-equal? (run '(unbox (box 1))) 1)
  (check-equal? (run '(car (cons 1 2))) 1)
  (check-equal? (run '(cdr (cons 1 2))) 2)
  (check-equal? (run '(cons 1 '())) (list 1))
  (check-equal? (run '(let ((x (cons 1 2)))
                        (begin (cdr x)
                               (car x))))
                1)
  (check-equal? (run '(let ((x (cons 1 2)))
                        (let ((y (box 3)))
                          (unbox y))))
                3)

  ;; Hustle string examples
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

  (check-equal?
   (run
    '(let ((x "FFF"))
      (if (string? x)
          (string-ref x
                      (- (string-length x) 1))
          42)))
   #\F)
   
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
  ;; Evildoer examples
  (check-equal? (run 7 "") (cons 7 ""))
  (check-equal? (run '(write-byte 97) "") (cons (void) "a"))
  (check-equal? (run '(read-byte) "a") (cons 97 ""))
  (check-equal? (run '(begin (write-byte 97) (read-byte)) "b")
                (cons 98 "a"))
  (check-equal? (run '(read-byte) "") (cons eof ""))
  (check-equal? (run '(eof-object? (read-byte)) "") (cons #t ""))
  (check-equal? (run '(eof-object? (read-byte)) "a") (cons #f ""))
  (check-equal? (run '(begin (write-byte 97) (write-byte 98)) "")
                (cons (void) "ab"))

  (check-equal? (run '(peek-byte) "ab") (cons 97 ""))
  (check-equal? (run '(begin (peek-byte) (read-byte)) "ab") (cons 97 ""))
  ;; Extort examples
  (check-equal? (run '(write-byte #t) "") (cons 'err ""))

  ;; Fraud examples
  (check-equal? (run '(let ((x 97)) (write-byte x)) "") (cons (void) "a"))
  (check-equal? (run '(let ((x 97))
                        (begin (write-byte x)
                               x))
                     "")
                (cons 97 "a"))
  (check-equal? (run '(let ((x 97)) (begin (read-byte) x)) "b")
                (cons 97 ""))
  (check-equal? (run '(let ((x 97)) (begin (peek-byte) x)) "b")
                (cons 97 ""))

  ;; Hustle examples
  (check-equal? (run '(let ((x 1))
                        (begin (write-byte 97)
                               1))
                     "")
                (cons 1 "a"))

  (check-equal? (run '(let ((x 1))
                        (let ((y 2))
                          (begin (write-byte 97)
                                 1)))
                     "")
                (cons 1 "a"))

  (check-equal? (run '(let ((x (cons 1 2)))
                        (begin (write-byte 97)
                               (car x)))
                     "")
                (cons 1 "a"))

  ;; Hustle string examples
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
