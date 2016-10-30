(load "C:/Users/iassona/Desktop/chez-init.ss");

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define letrec-name-check 
  (lambda (ls)
    (if (list? ls)
        (if (symbol? (car ls))
            (car ls)
            (eopl:error 'parse-exp "letrec needs names ~s" 'datum))
        (eopl:error 'parse-exp "improper list for letrec ~s" 'datum))))

(define letrec-body-check
  (lambda (ls)
    (if (and (not (null? (cdr ls))) (null? (cddr ls)))
        (cadr ls)
        (eopl:error 'parse-exp "incorrect number of args ~s" 'datum))))

(define let*-name-check 
  (lambda (ls)
    (if (list? ls)
        (if (symbol? (car ls))
            (car ls)
            (eopl:error 'parse-exp "let* needs names ~s" 'datum))
        (eopl:error 'parse-exp "improper list for let* ~s" 'datum))))

(define let-name-check 
  (lambda (ls)
    (if (list? ls)
        (if (symbol? (car ls))
            (car ls)
            (eopl:error 'parse-exp "let needs names ~s" 'datum))
        (eopl:error 'parse-exp "improper list for let ~s" 'datum))))


(define parse-exp         
  (lambda (datum)
    (cond
     [(symbol? datum) 
      (var-exp datum)]
     [(or (number? datum) (vector? datum) (boolean? datum) (string? datum))
      (lit-exp datum)]
     [(pair? datum)
      (cond
        [(not (list? datum))
          (eopl:error 'parse-exp "application: ~s is not a proper list" datum)]
        [(equal? (1st datum) 'lambda)
         (cond
           [(< (length datum) 3)
            (eopl:error 'parse-exp "lambda expression missing body" datum)]
           [else (lambda-exp (letrec ([lambda-help (lambda (ls)
                                               (cond
                                                 [(or (symbol? ls) (null? ls))
                                                  (if (list? (cadr datum))
                                                      (cadr datum)
                                                      (fix-improp (cadr datum)))]
                                                 [(symbol? (car ls))
                                                  (lambda-help (cdr ls))]
                                                 [else
                                                   (eopl:error 'parse-exp "formals must be symbols:" (cadr datum))]))])
                                   (lambda-help (cadr datum)))
             (map parse-exp (cddr datum)))])]
        [(equal? (1st datum) 'case-lambda)
         (cond
           [(< (length datum) 2)
            (eopl:error 'parse-exp "case-lambda expression missing body" datum)]
           [else (case-lambda-exp (letrec ([case-lambda-help 
                                             (lambda (ls)
                                               (cond
                                                 [(null? ls)
                                                 '()]
                                                 [(list? (car ls))
                                                  (cons (lambda-exp (caar ls) (parse-exp (cadar ls))) (case-lambda-help (cdr ls)))]
                                                 [else
                                                   (eopl:error 'parse-exp "formals must be symbols:" (cadr datum))]))])
                                    (case-lambda-help (cdr datum))))])]
        [(equal? (1st datum) 'set!)
         (set!-exp (cadr datum)
           (if (and (not(null? (cddr datum))) (null? (cdddr datum)))
               (parse-exp (caddr datum))
               (cond
                 [(null? (cddr datum))
                  (eopl:error 'parse-exp "not enough expressions for set!: ~s" 'datum)]
                 [(not (null? (cdddr datum)))
                  (eopl:error 'parse-exp "too many arguments for set!: ~s" 'datum)])))]
        [(equal? (1st datum) 'if)
         (cond
           [(< (length datum) 2)
               (eopl:error 'parse-exp "if statement needs condition: ~s" datum)]
           [(< (length datum) 3)
               (eopl:error 'parse-exp "if statement needs truth statement: ~s" datum)]
           [(< (length datum) 4)
            (oneif-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))]
           [else (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))])]
        [(equal? (1st datum) 'letrec)
         (letrec-exp 
           (cond
             [(list? (cadr datum))
              (map letrec-name-check (cadr datum))]
             [else (eopl:error 'parse-exp "incorrect number of elements needed for letrec: ~s" 'datum)])
             
             (map cadr (map parse-exp (map letrec-body-check (cadr datum))))
             (map caddr (map parse-exp (map letrec-body-check (cadr datum))))
             
             (if (null? (cddr datum))
                 (eopl:error 'parse-exp "body of letrec missing ~s" 'datum)
                 (map parse-exp (cddr datum))))]

        
        [(equal? (1st datum) 'let*)
        (let*-exp (cond 
                    [(list? (cadr datum))
                      (map let*-name-check (cadr datum))]
             [else (eopl:error 'parse-exp "incorrect number of elements needed for let*: ~s" 'datum)])
             
             (map parse-exp (map letrec-body-check (cadr datum)))
             
             (if (null? (cddr datum))
                 (eopl:error 'parse-exp "body of let* is missing ~s" 'datum)
                 (map parse-exp (cddr datum))))]
                     
        [(equal? (1st datum) 'let)
         (if (>= (length datum) 4)
             (named-let-exp (cadr datum) 
               (cond 
                 [(list? (caddr datum))
                 (map let-name-check (caddr datum))]
                [else (eopl:error 'parse-exp "incorrect number of elements needed for let: ~s" 'datum)])
               (map parse-exp (map letrec-body-check (caddr datum)))
              
              (if (null? (cdddr datum))
                 (eopl:error 'parse-exp "body of let is missing ~s" 'datum)
                 (map parse-exp (cdddr datum))))
         (let-exp (cond 
                    [(list? (cadr datum))
                      (map let-name-check (cadr datum))]
             [else (eopl:error 'parse-exp "incorrect number of elements needed for let: ~s" 'datum)])
             
             (map parse-exp (map letrec-body-check (cadr datum)))
             
             (if (null? (cddr datum))
                 (eopl:error 'parse-exp "body of let is missing ~s" 'datum)
                 (map parse-exp (cddr datum)))))]
        
        [(equal? (1st datum) 'quote)
         (quote-exp (2nd datum))]
        [(equal? (1st datum) 'begin)
        (begin-exp (map parse-exp (cdr datum)))]
        [(equal? (1st datum) 'case)
         (case-exp (parse-exp (2nd datum)) (in-order-map parse-exp (in-order-map car (cddr datum))) (in-order-map parse-exp (in-order-map cadr (cddr datum))))]
        [(equal? (1st datum) 'cond)
         (cond-exp (in-order-map parse-exp (in-order-map car (cdr datum))) (in-order-map parse-exp (in-order-map cadr (cdr datum))))]
        [(equal? (1st datum) 'while)
         (while-exp (parse-exp (cadr datum)) (in-order-map parse-exp (cddr datum)))]
        [else (if (list? datum)
                  (if ((list-of number?) datum)
                      (quote-exp datum)
                      (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum))))
                      (eopl:error 'parse-exp "apps must be a list ~s" 'datum))])]
      
      [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define fix-improp
  (lambda (ls)
    (if (not (pair? ls))
        (list ls)
        (cons (car ls) (fix-improp (cdr ls))))))
      
      
(define-datatype expression expression?
  (null-exp)
  (app-exp
    (rator list?)
    (rands list?))
  (var-exp
    (id symbol?))
  (lit-exp
    (val (lambda (x) #t)))
  (lambda-exp
    (vars (lambda (ls) (or (symbol? ls) (null? ls) ((list-of symbol?) ls))))
    (body list?))
  (if-exp
    (condition expression?)
    (true (lambda (ls) (or (boolean? ls) (expression? ls))))
    (false (lambda (ls) (or (null? ls) (boolean? ls) (expression? ls) (symbol? ls) (void? ls)))))
  (oneif-exp
    (condition expression?)
    (true expression?))
  (quote-exp
    (quotee (lambda (x) #t)))
  (begin-exp
    (rest (list-of expression?)))
  (letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodiess (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?)))
  (let*-exp
    (names list?)
    (inners list?)
    (body list?))
  (let-exp
    (names list?)
    (inners list?)
    (body list?))
  (set!-exp
    (var symbol?)
    (val expression?))
  (cond-exp
    (test list?)
    (body list?))
  (case-exp
    (var list?)
    (tests list?)
    (bodies list?))
  (named-let-exp
    (name symbol?)
    (vars list?)
    (vals list?)
    (body list?))
  (member-exp
    (var list?)
    (rest list?))
  (while-exp
    (test list?)
    (args list?))
  (case-lambda-exp
    (exps (list-of expression?))))

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?))
  (recursively-extended-env-record
    (proc-names (list-of symbol?))
    (syms (list-of (list-of symbol?)))
    (vals (list-of (list-of expression?)))
    (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
    (var (lambda (ls) (or  (symbol? ls)  (list? ls))))
    (body list?)
    (env  environment?)]
  [case-closure
    (closures list?)])

; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (proc-names ids bodies old-env)
    (recursively-extended-env-record
      proc-names ids bodies old-env)))

(define iota
  (lambda (len)
    (letrec [(helper 
               (lambda (len num ans)
                 (cond
                   [(zero? len)
                     ans]
                   [else 
                     (helper (- len 1) (+ 1 num) (append ans (list num)))])))]
      (helper len 0 '() ))))
                     
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

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



(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [null-exp () '()]
      [var-exp (var) var]
      [lit-exp (val) val]
      [lambda-exp 
        (vars body)
        (append (list 'lambda vars) (map unparse-exp body))]
      [set!-exp
        (var val)
        (list 'set! var (unparse-exp val))]
      [if-exp
        (condition true false)
        (if (null? false)
            (list 'if
              (unparse-exp condition)
              (unparse-exp true))
            (list 'if 
              (unparse-exp condition)
              (unparse-exp true)
              (unparse-exp false)))]
      [oneif-exp
        (condition true)
        (list 'if
          (unparse-exp condition)
          (unparse-exp true))]
      [begin-exp
        (rest)
        (cons 'begin (map unparse-exp rest))]
      [quote-exp
        (quotee)
        (list (quote quote) val)]
      [app-exp
        (vars)
        (map unparse-exp vars)]
      [let-exp
        (names inners body)
        (append (cons 'let (list(let-unparser names inners))) (map unparse-exp body))]
      [let*-exp
        (names inners body)
        (append (cons 'let* (list (let-unparser names inners))) (map unparse-exp body))]
      [letrec-exp
        (names inners body)
        (append (cons 'letrec (list (let-unparser names inners))) (map unparse-exp body))]
      )))

(define let-unparser
  (lambda (vars bodies)
    (if (null? vars)
        '()
        (cons
          (list (car vars) (unparse-exp (car bodies)))
          (let-unparser (cdr vars) (cdr bodies))))))



; top-level-eval evaluates a form in the global environment

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
      [(/) (/ (1st args) (2nd args))]
      [(quotient) (quotient (1st args) (2nd args))]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(zero?) (zero? (1st args))]
      [(car) (car (1st args))]
      [(cdr) (cdr (1st args))]
      [(list) (apply list args)]
      [(null?) (null? (1st args))]
      [(assq) (assq (1st args) (cdr args))]
      [(eq?) (eq? (1st args) (2nd args))]
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (proc-val? (1st args))]
      [(vector->list) (vector->list (1st args))]
      [(vector?) (vector? (1st args))]
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(>) (apply > args)]
      [(<) (apply < args)]
      [(>=) (apply >= args)]
      [(<=) (apply <- args)]
      [(!=) (not (apply = args))]
      [(caar) (caar (1st args))]
      [(cadr) (cadr (1st args))]
      [(cdar) (cdar (1st args))]
      [(cddr) (cddr (1st args))]
      [(caaar) (caaar (1st args))]
      [(caadr) (caadr (1st args))]
      [(cadar) (cadar (1st args))]
      [(caddr) (caddr (1st args))]
      [(cdaar) (cdaar (1st args))]
      [(cdadr) (cdadr (1st args))]
      [(cddar) (cddar (1st args))]
      [(cdddr) (cdddr (1st args))]
      [(not) (apply not args)]
      [(apply) (apply-proc (1st args) (2nd args) env)]
      [(member) (1st args) (2nd args)]
      [(map) (map (lambda (x) (apply-proc (car args) (list x) (empty-env))) (cadr args))]
      [(vector) (apply vector args)]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))


(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [null-exp () '()]
      [var-exp (var) exp]
      [lit-exp (val) exp]
      [lambda-exp (vars body)
        (lambda-exp vars (map syntax-expand body))]
      [member-exp (var vals)
        (member var vals)]
      [set!-exp (var val)
        (set!-exp var (syntax-expand val))]
      [if-exp (condition true false)
        (if-exp
          (syntax-expand condition)
          (syntax-expand true)
          (syntax-expand false))]
      [named-let-exp (name vars vals bodies)
        (letrec-exp (list name) (list vars) (list bodies) (list (app-exp (var-exp name) vals)))];(map (lambda (val) (lit-exp val)) vals))))]
                                 

      [oneif-exp (condition true)
        (oneif-exp
          (syntax-expand condition)
          (syntax-expand true))]
      [begin-exp
        (rest)
        (begin-exp (map syntax-expand rest))]
      [quote-exp
        (quotee)
        exp]
      [app-exp
        (rator rands)
        (cond
          [(equal? rator (var-exp 'or))
           (letrec [(or-expand
                      (lambda (ls)
                        (cond
                          [(null? ls) (lit-exp #f)]
                          [else (if-exp (syntax-expand (car ls))
                                  (syntax-expand (car ls))
                                  (or-expand (cdr ls)))])))]
             (or-expand rands))]
          [(equal? rator (var-exp 'and))
           (letrec [(and-expand
                      (lambda (ls)
                        (cond
                          [(null? ls) (lit-exp #t)]
                          [else (if-exp (syntax-expand (car ls))
                                  (and-expand (cdr ls))
                                  (lit-exp #f))])))]
             (and-expand rands))]
          [else
            (app-exp (syntax-expand rator) (map syntax-expand rands))])]
      [let-exp
        (names inners bodies) 
        (app-exp (lambda-exp names (map syntax-expand bodies)) (map syntax-expand inners))]
      [let*-exp
        (names inners bodies) 
        (app-exp (lambda-exp names (map syntax-expand bodies)) (map syntax-expand inners))]
      [case-exp (var tests bodies)
        (letrec [(case-expand
                   (lambda (vals testls bodls)
                     (cond
                       [(null? testls)
                        ]
                       [(equal? 'else (cadar testls))
                        (syntax-expand (car bodls))]
                       [else (if-exp (member-exp vals (car testls))
                                (car bodls)
                                (case-expand vals (cdr testls) (cdr bodls)))])))]
          (case-expand var tests bodies))]
      [while-exp (test args)
        (while-exp (syntax-expand test) (in-order-map syntax-expand args))]
      [cond-exp (tests bodies)
        (letrec [(cond-expand
                   (lambda (testls bodls)
                     (cond
                       [(null? testls)
                        (lit-exp (void))]
                       [(equal? 'else (cadar testls))
                        (syntax-expand (car bodls))]
                       [else (if-exp (syntax-expand (car testls)) 
                               (syntax-expand (car bodls)) 
                               (cond-expand (cdr testls) (cdr bodls)))])))]
          (cond-expand tests bodies))]
      [else exp])))



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
