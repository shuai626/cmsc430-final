#lang racket
(provide test-runner)
(require rackunit)

;; Contains Hustle+Rgx examples

(define (test-runner run) 
  ;; regexp-match-exact? examples
  (check-equal? (run '(regexp-match-exact? "" "a")) #f)
  (check-equal? (run '(regexp-match-exact? "" "")) #t)

  (check-equal? (run '(regexp-match-exact? "." "a")) #t)
  (check-equal? (run '(regexp-match-exact? "." "b")) #t)
  (check-equal? (run '(regexp-match-exact? "." "bb")) #f)

  (check-equal? (run '(regexp-match-exact? "a" "a")) #t)
  (check-equal? (run '(regexp-match-exact? "a" "b")) #f)

  (check-equal? (run '(regexp-match-exact? "a|b" "a")) #t)
  (check-equal? (run '(regexp-match-exact? "a|b" "b")) #t)
  (check-equal? (run '(regexp-match-exact? "a|b" "c")) #f)

  (check-equal? (run '(regexp-match-exact? "a|b|c" "a")) #t)
  (check-equal? (run '(regexp-match-exact? "a|b|c" "b")) #t)
  (check-equal? (run '(regexp-match-exact? "a|b|c" "c")) #t)
  (check-equal? (run '(regexp-match-exact? "a|b|c" "d")) #f)

  (check-equal? (run '(regexp-match-exact? "abcaa" "abcaa")) #t)
  (check-equal? (run '(regexp-match-exact? "abcaa" "abca")) #f)

  (check-equal? (run '(regexp-match-exact? "a*" "a")) #t)
  (check-equal? (run '(regexp-match-exact? "a*" "aaaaa")) #t)
  (check-equal? (run '(regexp-match-exact? "a*" "aaaaaaaaab")) #f)
  (check-equal? (run '(regexp-match-exact? "a*b" "aaaaaaaaab")) #t)

  (check-equal? (run '(regexp-match-exact? "a?" "")) #t)
  (check-equal? (run '(regexp-match-exact? "a?" "a")) #t)
  (check-equal? (run '(regexp-match-exact? "a?" "b")) #f)
  
  (check-equal? (run '(regexp-match-exact? "(.a|bc)*" "bcbcbcda")) #t)
  (check-equal? (run '(regexp-match-exact? "(.ce|bc)" "ace")) #t)
  (check-equal? (run '(regexp-match-exact? "(.ce|bc)" "bc")) #t)

  (check-equal? (run '(regexp-match-exact? "(a|b)*" "abbaaabbababa")) #t)

  ;; regex-p-match? examples
  (check-equal? (run '(regexp-match? "" "a")) #t)
  (check-equal? (run '(regexp-match? "" "")) #t)

  (check-equal? (run '(regexp-match? "." "a")) #t)
  (check-equal? (run '(regexp-match? "." "b")) #t)
  (check-equal? (run '(regexp-match? "." "bb")) #t)

  (check-equal? (run '(regexp-match? "a" "a")) #t)
  (check-equal? (run '(regexp-match? "a" "paw")) #t)
  (check-equal? (run '(regexp-match? "a" "b")) #f)

  (check-equal? (run '(regexp-match? "a|b" "a")) #t)
  (check-equal? (run '(regexp-match? "a|b" "bad")) #t)
  (check-equal? (run '(regexp-match? "a|b" "b")) #t)
  (check-equal? (run '(regexp-match? "a|b" "bbbbbba")) #t)
  (check-equal? (run '(regexp-match? "a|b" "ddddddda")) #t)
  (check-equal? (run '(regexp-match? "a|b" "pad")) #t)
  (check-equal? (run '(regexp-match? "a|b" "c")) #f)

  (check-equal? (run '(regexp-match? "abcaa" "abcaa")) #t)
  (check-equal? (run '(regexp-match? "abcaa" "bbbabcaabbb")) #t)
  (check-equal? (run '(regexp-match? "abcaa" "abca")) #f)

  (check-equal? (run '(regexp-match? "a*" "a")) #t)
  (check-equal? (run '(regexp-match? "a*" "")) #t)
  (check-equal? (run '(regexp-match? "a*" "aaaaa")) #t)
  (check-equal? (run '(regexp-match? "a*" "aaaaaaaaab")) #t)
  (check-equal? (run '(regexp-match? "a*b" "b")) #t)
  (check-equal? (run '(regexp-match? "a*b" "aaaaaaaaab")) #t)
  (check-equal? (run '(regexp-match? "a*b" "ddddddddb")) #t)
  (check-equal? (run '(regexp-match? "a*b" "dddddddd")) #f)

  (check-equal? (run '(regexp-match? "a?" "")) #t)
  (check-equal? (run '(regexp-match? "a?" "a")) #t)
  (check-equal? (run '(regexp-match? "a?" "bbbabbb")) #t)
  (check-equal? (run '(regexp-match? "a?" "b")) #t)
  
  (check-equal? (run '(regexp-match? "a+" "a")) #t)
  (check-equal? (run '(regexp-match? "a+" "")) #f)
  (check-equal? (run '(regexp-match? "a+" "aaaaa")) #t)
  (check-equal? (run '(regexp-match? "a+" "aaaaaaaaab")) #t)
  (check-equal? (run '(regexp-match? "a+b" "b")) #f)
  (check-equal? (run '(regexp-match? "a+b" "aaaaaaaaab")) #t)

  (check-equal? (run '(regexp-match? "(.a|bc)*" "bcbcbcda")) #t)
  (check-equal? (run '(regexp-match? "(.ce|bc)" "ace")) #t)
  (check-equal? (run '(regexp-match? "(.ce|bc)" "bc")) #t)

  (check-equal? (run '(regexp-match? "(a|b)*" "abbaaabbababa")) #t)

  ;; examples with let binding for query strings
  (check-equal? (run '(let ((x "OKAY123")) (regexp-match? "KAY" x))) #t)
  (check-equal? (run '(let ((x "OKAY123")) (let ((y "OKAY123")) (regexp-match? "KAY" x)))) #t)
  ;; example with let binding AND multiple regex queries
  (check-equal? (run '(let ((x "OKAY123")) (let ((y "OKAY123"))
                        (if (regexp-match? "KAY" x) (regexp-match? "false" y) #t)))) #f)

  ;; string anchoring examples
  (check-equal? (run '(regexp-match? "^$" "")) #t)
  (check-equal? (run '(regexp-match? "^" "ace")) #t)
  (check-equal? (run '(regexp-match? "$" "bc")) #t)
  (check-equal? (run '(regexp-match? "^hello$" "hello")) #t)
  (check-equal? (run '(regexp-match? "^hello$" "helloooo")) #f)
  (check-equal? (run '(regexp-match? "^hello" "hellothere")) #t)
  (check-equal? (run '(regexp-match? "^.*xyz" "xyz430")) #t)
  (check-equal? (run '(regexp-match? "^.*xyz" "abcdefghixyz123")) #t)
  (check-equal? (run '(regexp-match? "(ab|c)*hello$" "cmsc430abacbacchello")) #t)

  ;; character class examples
  (check-equal? (run '(regexp-match? "[a-z]+hello$" "randomlettershahahello")) #t)
  (check-equal? (run '(regexp-match? "[0-9]+[B-F]+[g-hC-H2-3]*$" "012394CCCDgF2")) #t)
  (check-equal? (run '(regexp-match? "[0-9]+[B-F]+[g-hC-H2-3]*$" "012394CCCDgA2")) #f)
  (check-equal? (run '(regexp-match? "[0-9]+[B-F]+[g-hC-H2-3]*$" "012394CCCDaF2")) #f)
  (check-equal? (run '(regexp-match? "[0-9]+[B-F]+[g-hC-H2-3]*$" "012394CCADgF2")) #f)
  (check-equal? (run '(regexp-match? "[0-9]+[B-F]+[g-hC-H2-3]*$" "012394CCCDgF4")) #f)
  (check-equal? (run '(regexp-match? "[a-z]+hello$" "randomlettershahaZhello")) #f)
  (check-equal? (run '(regexp-match? "^cracked[a-sWAJ]+" "crackedasWAJgWb")) #t)
  (check-equal? (run '(regexp-match? "^cracked[Wa-sAJ]+" "crackedasWAJgWb")) #t)
  (check-equal? (run '(regexp-match? "cracked[WAJ]+$" "woohoocrackedJAW")) #t)

  ;; range quantifier examples
  (check-equal? (run '(regexp-match-exact? "(a|b){5}" "aabab")) #t)
  (check-equal? (run '(regexp-match-exact? "(a|b){5}" "aabaab")) #f)
  (check-equal? (run '(regexp-match-exact? "(a|b){5}" "aaab")) #f)
  (check-equal? (run '(regexp-match-exact? "(a|b){5}" "aaabc")) #f)
  (check-equal? (run '(regexp-match-exact? "[abc]{,5}" "")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{,5}" "abc")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{,5}" "aabbc")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{,5}" "aabbd")) #f)
  (check-equal? (run '(regexp-match-exact? "[abc]{,5}" "aabbcc")) #f)
  (check-equal? (run '(regexp-match-exact? "[abc]{2,}" "ab")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{2,}" "aaaaabbbbbbbccccccc")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{2,}" "a")) #f)
  (check-equal? (run '(regexp-match-exact? "[abc]{}" "")) #t)
  (check-equal? (run '(regexp-match-exact? "[abc]{}" "aaaaaaaabbbbbbbbcccccc")) #t)
  (check-equal? (run '(regexp-match-exact? "a{5,7}" "aaaaa")) #t)
  (check-equal? (run '(regexp-match-exact? "a{5,7}" "aaaaaa")) #t)
  (check-equal? (run '(regexp-match-exact? "a{5,7}" "aaaaaaa")) #t)
  (check-equal? (run '(regexp-match-exact? "a{5,7}" "aaaa")) #f)
  (check-equal? (run '(regexp-match-exact? "a{5,7}" "aaaaaaaaaaaa")) #f)
)
