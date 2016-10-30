(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form (empty-env))))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
  (lambda (exp env)                                                                             ;<------ Add env to this procedure
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id) ; look up its value.
        (apply-env env
          id
          identity-proc ; procedure to call if var is in env
          (lambda () ; procedure to call if var is not in env
            (apply-env global-env ; was init-env
              id
              identity-proc
              (lambda ()
                (error 'apply-env
                  "variable ~s is not bound"
                  id)))))]
      [member-exp (var vals)
        (member (eval-exp var env) (eval-exp vals env))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args env))]
      [if-exp (test-exp then-exp else-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env)
            (eval-exp else-exp env))]
      [oneif-exp (test-exp then-exp)
        (if (eval-exp test-exp env)
            (eval-exp then-exp env))]
      [quote-exp (quotee)
        quotee]
      [letrec-exp (proc-names ids bodies letrec-body)
        (eval-bodies letrec-body
          (extend-env-recursively
            proc-names ids bodies env))]
      [let-exp (syms exprs bodies)
        (let ([extended-env
                (extend-env syms
                  (map (lambda (e)(eval-exp e env))
                    exprs)
                  env)])
      (eval-bodies bodies env))]
      [lambda-exp (vars-exp body-exp)
        (closure vars-exp body-exp env)]
      [case-lambda-exp (exps)
        (case-closure (map
                        (lambda (ex)
                          (cases expression ex
                            [lambda-exp (vars-exp body-exp)
                              (closure vars-exp body-exp env)]
                            [else ex]))
                        exps))]
      [begin-exp (bodies)
        (eval-bodies bodies env)]
      [while-exp (test bods)
        (letrec ([while
                   (lambda ()
                     (if (eval-exp test env)
                         (begin (eval-bodies bods env) (while))))])
          (while))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (in-order-map (lambda (exp) (eval-exp exp env)) rands)))

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
        (eval-exp (car bodies) env)
        (begin (eval-exp (car bodies) env) (eval-bodies (cdr bodies) env)))))

(define in-order-map
  (lambda (proc ls)
    (if (null? ls)
        '()
          (cons (proc (car ls)) (in-order-map proc (cdr ls))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args env)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args env)]
			; You will add other cases
      [closure (vars bodies env)
        (apply-closure vars args bodies env)]
      [case-closure (closures)
        (select-and-apply-closure args env closures)]
      [else (eopl:error 'apply-proc
                   "Attempt to apply bad procedure: ~s"
                    proc-value)])))

(define apply-closure
  (lambda (vars args bodies env)
    (eval-bodies bodies (extend-env vars args env))))

(define select-and-apply-closure
  (lambda (args env closures)
    (if (equal? (length args) (length (cadar closures)))
        (apply-closure (cadar closures) args (list (caddar closures)) env)
        (select-and-apply-closure args env (cdr closures))
        )))

(define match-args?
  (lambda (args closure)
    #t))


(define *prim-proc-names* '(+ - * add1 sub1 cons =
                            / zero? car cdr list null? eq? equal? atom? length list->vector list?
                            pair? procedure? vector->list vector? make-vector vector-ref vector? number?
                            symbol? set-car! set-cdr! vector-set! display newline caar cadr cdar
                            quotient cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr > < >= <= != not map apply vector member?))

(define init-env         ; for now, our initial global environment only contains
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
    (map prim-proc
          *prim-proc-names*)
     (empty-env)))

(define global-env
  init-env)

; Usually an interpreter must define each
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args env)
    (case prim-proc
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (/ (car args) (cadr args))]
      [(quotient) (quotient (car args) (cadr args))]
      [(add1) (+ (car args) 1)]
      [(sub1) (- (car args) 1)]
      [(cons) (cons (car args) (cadr args))]
      [(=) (= (car args) (cadr args))]
      [(zero?) (zero? (car args))]
      [(car) (car (car args))]
      [(cdr) (cdr (car args))]
      [(list) (apply list args)]
      [(null?) (null? (car args))]
      [(assq) (assq (car args) (cdr args))]
      [(eq?) (eq? (car args) (cadr args))]
      [(equal?) (equal? (car args) (cadr args))]
      [(atom?) (atom (car args))]
      [(length) (length (car args))]
      [(list->vector) (list->vector (car args))]
      [(list?) (list? (car args))]
      [(pair?) (pair? (car args))]
      [(procedure?) (proc-val? (car args))]
      [(vector->list) (vector->list (car args))]
      [(vector?) (vector? (car args))]
      [(make-vector) (make-vector (car args) (cadr args))]
      [(vector-ref) (vector-ref (car args) (cadr args))]
      [(vector?) (vector? (car args))]
      [(number?) (number? (car args))]
      [(symbol?) (symbol? (car args))]
      [(set-car!) (set-car! (car args) (cadr args))]
      [(set-cdr!) (set-cdr! (car args) (cadr args))]
      [(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
      [(display) (display (car args))]
      [(newline) (newline)]
      [(>) (apply > args)]
      [(<) (apply < args)]
      [(>=) (apply >= args)]
      [(<=) (apply <- args)]
      [(!=) (not (apply = args))]
      [(caar) (caar (car args))]
      [(cadr) (cadr (car args))]
      [(cdar) (cdar (car args))]
      [(cddr) (cddr (car args))]
      [(caaar) (caaar (car args))]
      [(caadr) (caadr (car args))]
      [(cadar) (cadar (car args))]
      [(caddr) (caddr (car args))]
      [(cdaar) (cdaar (car args))]
      [(cdadr) (cdadr (car args))]
      [(cddar) (cddar (car args))]
      [(cdddr) (cdddr (car args))]
      [(not) (apply not args)]
      [(apply) (apply-proc (car args) (cadr args) env)]
      [(member) (car args) (cadr args)]
      [(map) (map (lambda (x) (apply-proc (car args) (list x) (empty-env))) (cadr args))]
      [(vector) (apply vector args)]
      [else (error 'apply-prim-proc
            "Bad primitive procedure name: ~s"
            prim-proc)])))

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
           (empty-env-record ()
                             (fail))
           (extended-env-record (syms vals env)
                                (let ((pos (list-find-position sym syms)))
                                  (if (number? pos)
                                      (succeed (list-ref vals pos))
                                      (apply-env env sym succeed fail))))
           (recursively-extended-env-record
            (procnames idss bodiess old-env)
            (let ([pos
                   (list-find-position sym procnames)])
              (if (number? pos)
                  (closure (list-ref idss pos)
                           (list-ref bodiess pos)
                           env)
                  (apply-env old-env sym succeed fail)))))))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
