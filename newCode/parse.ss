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
        [(equal? (car datum) 'lambda)
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
        [(equal? (car datum) 'case-lambda)
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
        [(equal? (car datum) 'set!)
         (set!-exp (cadr datum)
           (if (and (not(null? (cddr datum))) (null? (cdddr datum)))
               (parse-exp (caddr datum))
               (cond
                 [(null? (cddr datum))
                  (eopl:error 'parse-exp "not enough expressions for set!: ~s" 'datum)]
                 [(not (null? (cdddr datum)))
                  (eopl:error 'parse-exp "too many arguments for set!: ~s" 'datum)])))]
        [(equal? (car datum) 'if)
         (cond
           [(< (length datum) 2)
               (eopl:error 'parse-exp "if statement needs condition: ~s" datum)]
           [(< (length datum) 3)
               (eopl:error 'parse-exp "if statement needs truth statement: ~s" datum)]
           [(< (length datum) 4)
            (oneif-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))]
           [else (if-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))])]
        [(equal? (car datum) 'letrec)
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


        [(equal? (car datum) 'let*)
        (let*-exp (cond
                    [(list? (cadr datum))
                      (map let*-name-check (cadr datum))]
             [else (eopl:error 'parse-exp "incorrect number of elements needed for let*: ~s" 'datum)])

             (map parse-exp (map letrec-body-check (cadr datum)))

             (if (null? (cddr datum))
                 (eopl:error 'parse-exp "body of let* is missing ~s" 'datum)
                 (map parse-exp (cddr datum))))]

        [(equal? (car datum) 'let)
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

        [(equal? (car datum) 'quote)
         (quote-exp (cadr datum))]
        [(equal? (car datum) 'begin)
        (begin-exp (map parse-exp (cdr datum)))]
        [(equal? (car datum) 'case)
         (case-exp (parse-exp (cadr datum)) (in-order-map parse-exp (in-order-map car (cddr datum))) (in-order-map parse-exp (in-order-map cadr (cddr datum))))]
        [(equal? (car datum) 'cond)
         (cond-exp (in-order-map parse-exp (in-order-map car (cdr datum))) (in-order-map parse-exp (in-order-map cadr (cdr datum))))]
        [(equal? (car datum) 'while)
         (while-exp (parse-exp (cadr datum)) (in-order-map parse-exp (cddr datum)))]
        [else (if (list? datum)
                  (if ((list-of number?) datum)
                      (quote-exp datum)
                      (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum))))
                      (eopl:error 'parse-exp "apps must be a list ~s" 'datum))])]

     [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))
