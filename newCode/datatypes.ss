(define-datatype expression expression?
  [null-exp]
  [app-exp (rator list?)
           (rands list?)]
  [var-exp (id symbol?)]
  [lit-exp (val (lambda (x) #t))]
  [lambda-exp (vars (lambda (ls) (or (symbol? ls) (null? ls) ((list-of symbol?) ls))))
              (body list?)]
  [if-exp  
    (condition expression?)
    (true (lambda (ls) (or (boolean? ls) (expression? ls))))
    (false (lambda (ls) (or (null? ls) (boolean? ls) (expression? ls) (symbol? ls) (void? ls))))]
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


(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (var (lambda (ls) (or  (symbol? ls)  (list? ls))))
   (body list?)
   (env  environment?)]
  [case-closure
   (closures list?)])

