;; evaluator for simple expressions.
;; Possible starting point for first interpreter assignment.
;;                  
;; Claude Anderson.  Last modified April, 2014
;; Modified by Wesley Van Pelt and Maxim Zaman.

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "env.ss")
    (load "datatypes.ss")
    (load "syntax-expand.ss")
    (load "parse.ss")
    (load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier!

