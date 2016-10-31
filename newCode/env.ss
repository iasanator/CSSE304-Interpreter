;; All stuff for environments

(define (empty-env) (empty-env-record))

(define (extend-env syms vals env) (extended-env-record syms vals env))

(define (extend-env-recursively proc-names ids bodies old-env)
    (recursively-extended-env-record proc-names ids bodies old-env))

(define (list-find-position sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los))

(define (list-index pred ls)
    (cond [(null? ls) #f]
          [(pred (car ls)) 0]
          [else (let ((list-index-r (list-index pred (cdr ls))))
                  (if (number? list-index-r)
                      (+ 1 list-index-r)
                      #f))]))

(define (apply-env env sym succeed fail)
  (cases environment env
         (empty-env-record    () (fail))
         (extended-env-record (syms vals env)
                              (let ((pos (list-find-position sym syms)))
                                (if (number? pos)
                                    (succeed (list-ref vals pos))
                                    (apply-env env sym succeed fail))))
         (recursively-extended-env-record (procnames idss bodiess old-env)
                                          (let ([pos (list-find-position sym procnames)])
                                            (if (number? pos)
                                                (closure (list-ref idss pos) (list-ref bodiess pos) env)
                                                (apply-env old-env sym succeed fail))))))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record (syms (list-of symbol?))
                       (vals (list-of (lambda (x) #t)))
                       (env environment?)]
  [recursively-extended-env-record (proc-names (list-of symbol?))
                                   (syms (list-of (list-of symbol?)))
                                   (vals (list-of (list-of expression?)))
                                   (env environment?)])
