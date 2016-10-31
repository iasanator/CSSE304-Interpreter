;; Core part of the interpreter

(define (top-level-eval form) (eval-exp form (empty-env)))

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
    (lambda (exp env)
      (cases expression exp
             [lit-exp    (datum) datum]
             [var-exp    (id) (apply-env env
                                         id
                                         identity-proc
                                         (lambda () (apply-env global-env
                                                               id
                                                               identity-proc
                                                               (lambda () (error 'apply-env "variable ~s is not bound" id)))))]
             [member-exp (var vals) (member (eval-exp var env) (eval-exp vals env))]
             [app-exp    (rator rands)
                         (let ([proc-value (eval-exp rator env)]
                               [args (eval-rands rands env)])
                           (apply-proc proc-value args env))]
             [if-exp     (test-exp then-exp else-exp)
                         (if (eval-exp test-exp env)
                             (eval-exp then-exp env)
                             (eval-exp else-exp env))]
             [oneif-exp  (test-exp then-exp)
                         (if (eval-exp test-exp env)
                             (eval-exp then-exp env))]
             [quote-exp  (quotee) quotee]
             [letrec-exp (proc-names ids bodies letrec-body)
                         (eval-bodies letrec-body
                                      (extend-env-recursively
                                       proc-names ids bodies env))]
             [lambda-exp (vars-exp body-exp)
                         (closure vars-exp body-exp env)]
             [begin-exp  (bodies)
                         (eval-bodies bodies env)]
             [while-exp  (test bods)
                         (letrec ([while (lambda ()
                                           (if (eval-exp test env)
                                               (begin (eval-bodies bods env) (while))))])
                           (while))]
             [else       (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

(define (eval-rands rands env) (in-order-map (lambda (exp) (eval-exp exp env)) rands))

(define (eval-bodies bodies env)
  (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin (eval-exp (car bodies) env) (eval-bodies (cdr bodies) env))))

(define (in-order-map proc ls)
  (if (null? ls) '()
      (cons (proc (car ls)) (in-order-map proc (cdr ls)))))

(define (apply-proc proc-value args env)
  (cases proc-val proc-value
         [prim-proc (op) (apply-prim-proc op args env)]
         [closure   (vars bodies env) (apply-closure vars args bodies env)]
         [else (eopl:error 'apply-proc "Attempt to apply bad procedure: ~s" proc-value)]))

(define (apply-closure vars args bodies env) (eval-bodies bodies (extend-env vars args env)))

(define (select-and-apply-closure args env closures)
  (if (equal? (length args) (length (cadar closures)))
      (apply-closure (cadar closures) args (list (caddar closures)) env)
      (select-and-apply-closure args env (cdr closures))))

(define (match-args? args closure) #t))

(define *prim-proc-names* '(+ - * add1 sub1 cons =
                            / zero? car cdr list null? eq? equal? atom? length list->vector list?
                            pair? procedure? vector->list vector? make-vector vector-ref vector? number?
                            symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar
                            quotient cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr > < >= <= != not map apply vector member?))

(define init-env (extend-env *prim-proc-names* (map prim-proc *prim-proc-names*) (empty-env)))
(define global-env init-env)

(define (apply-prim-proc prim-proc args env)
  (case prim-proc
    [(+)            (apply + args)]
    [(-)            (apply - args)]
    [(*)            (apply * args)]
    [(/)            (/ (car args) (cadr args))]
    [(=)            (= (car args) (cadr args))]
    [(>)            (apply > args)]
    [(<)            (apply < args)]
    [(>=)           (apply >= args)]
    [(<=)           (apply <- args)]
    [(!=)           (not (apply = args))]
    [(car)          (car (car args))]
    [(cdr)          (cdr (car args))]
    [(eq?)          (eq? (car args) (cadr args))]
    [(not)          (apply not args)]
    [(map)          (map (lambda (x) (apply-proc (car args) (list x) (empty-env))) (cadr args))]
    [(add1)         (+ (car args) 1)]
    [(assq)         (assq (car args) (cdr args))]
    [(caar)         (caar (car args))]
    [(cadr)         (cadr (car args))]
    [(cdar)         (cdar (car args))]
    [(cddr)         (cddr (car args))]
    [(cons)         (cons (car args) (cadr args))]
    [(list)         (apply list args)]
    [(sub1)         (- (car args) 1)]
    [(apply)        (apply-proc (car args) (cadr args) env)]
    [(atom?)        (atom (car args))]
    [(caaar)        (caaar (car args))]
    [(caadr)        (caadr (car args))]
    [(cadar)        (cadar (car args))]
    [(caddr)        (caddr (car args))]
    [(cdaar)        (cdaar (car args))]
    [(cdadr)        (cdadr (car args))]
    [(cddar)        (cddar (car args))]
    [(cdddr)        (cdddr (car args))]
    [(list?)        (list? (car args))]
    [(null?)        (null? (car args))]
    [(pair?)        (pair? (car args))]
    [(zero?)        (zero? (car args))]
    [(equal?)       (equal? (car args) (cadr args))]
    [(length)       (length (car args))]
    [(member)       (car args) (cadr args)]
    [(vector)       (apply vector args)]
    [(display)      (display (car args))]
    [(newline)      (newline)]
    [(number?)      (number? (car args))]
    [(symbol?)      (symbol? (car args))]
    [(vector?)      (vector? (car args))]
    [(set-car!)     (set-car! (car args) (cadr args))]
    [(set-cdr!)     (set-cdr! (car args) (cadr args))]
    [(quotient)     (quotient (car args) (cadr args))]
    [(procedure?)   (proc-val? (car args))]
    [(vector-ref)   (vector-ref (car args) (cadr args))]
    [(make-vector)  (make-vector (car args) (cadr args))]
    [(vector-set!)  (vector-set! (car args) (cadr args) (caddr args))]
    [(list->vector) (list->vector (car args))]
    [(vector->list) (vector->list (car args))]
    [else (error 'apply-prim-proc "Bad primitive procedure name: ~s" prim-proc)]))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      (eopl:pretty-print answer) (newline)
      (rep))))

(define (eval-one-exp x) (top-level-eval (syntax-expand (parse-exp x))))
