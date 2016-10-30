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

