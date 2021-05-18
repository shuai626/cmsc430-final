#lang racket
(require "test-runner-regex.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(test-runner    (λ (e) (unload/free (asm-interp (compile (parse e))))))
