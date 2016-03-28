#lang racket
(require htdp/testing)

;; Part A: More on quoting
;; Exercise A.1.a
;; -> 'x

;; Exercise A.1.b
;; -> '((x x) 5 (cons '(x x) x))

;; Exercise A.1.c
;; -> '((y z) 1 2)

;; Exercise A.1.d
;; -> '(x 1 y (2 3) 2 3 z (4 5 6) 4 5 6)

;; Exercise A.2.1
'(cons a b)

;; Exercise A.2.2
(list '+ '(* a 3) '(* b 2))

;; Exercise A.3.a
;; There is no quote for the +, so + means the addition operator for numbers.
;; 5 is a number, but ''5 is a symbol. There is a contract violation.

;; Exercise A.3.b
;; The + here still means an addition operator for numbers. 1 is a number.
;; However, '(+ 2 3) is a list containing the symbols, +, 2, and 3.
;; There is a contract violation.

;; Exercise A.3.c
;; The "," is equivalent to an unquote: ,(+ 2 3) -> (unquote (+ 2 3)).
;; However, unquote only works if it is used within a [quasi]quote.
;; Within a quote, it states that the following should be taken literally.
;; The full expression (+ ,(+ 2 3) 1) is not a quote, however - it is
;; a literal addition operation between 1 and ,(+ 2 3), thus there is
;; nothing to unquote from.

;; Part B: Mutation
;; Exercise B.1.1
;; 5 is not a variable, and set! is used to redefine a variable that has
;; already been defined.

;; Exercise B.1.2
;; Similar reason as above: (cons 3 4) is not a variable/identifier.

;; Exercise B.1.3
;; Here, a is a variable defined to be 42. However, 'a is a quoted "a".
;; In other words, 'a is not referring to the variable a, but rather
;; a symbol "a". 

;; Exercise B.1.4
;; (symbol) is not an identifier - it does not directly refer to the 42
;; in (define a 42).

;; Exercise B.1.5
;; (car a) is not an identifier; rather, "a" is. set! is unable to change
;; the value of the car of '(1 . 2).

;; Exercise B.2
(define (make-monitored f)
  (let mf ([count 0])
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) count)
            ((eq? input 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1))
                         (f input)))))))

;(define (square x) (* x x))
;(define squarem (make-monitored square))
;
;(check-expect (squarem 'how-many-calls?) 0)
;(check-expect (squarem 10) 100)
;(check-expect (squarem 'how-many-calls?) 1)
;(check-expect (squarem 42) 1764)
;(check-expect (squarem 'how-many-calls?) 2)
;(check-expect 
;  (begin
;    (squarem 'reset-count)
;    (squarem 'how-many-calls?)) 0)

;; Exercise B.3
(define f
  (let ([y 1])
    (lambda (x)
      (if (= y 1)
          (begin (set! y x)
                 y)
          y))))

;; Part C: Tagged Data
(define (get-tag m) (car m))
(define (get-value m) (cdr m))
(define grams-per-slug 14593.903203)
(define slugs-per-gram (/ 1 grams-per-slug))
;; Exercise C.1.a
(define (make-gram mass)
  (cons 'gram mass))
(define (make-slug mass)
  (cons 'slug mass))

;; Exercise C.1.b
(define (gram? m)
  (and (pair? m) (eq? 'gram (get-tag m))))
(define (slug? m)
  (and (pair? m) (eq? 'slug (get-tag m))))

;; Exercise C.1.c
(define (gram-add a b)
  (make-gram (+ (get-value a) (get-value b))))
(define (slug-add a b)
  (make-slug (+ (get-value a) (get-value b))))

;; Exercise C.1.d
(define (slug-to-gram mass)
  (make-gram (* grams-per-slug (get-value mass))))
(define (gram-to-slug mass)
  (make-slug (* slugs-per-gram (get-value mass))))
(define (get-gram mass)
  (cond ((gram? mass) mass)
        ((slug? mass) (slug-to-gram mass))
        (else
         (error "get-gram requires a mass, but given" mass))))
(define (get-slug mass)
  (cond ((slug? mass) mass)
        ((gram? mass) (gram-to-slug mass))
        (else
         (error "get-slug requires a mass, but given" mass))))

;; Exercise C.1.e
(define (mass-add a b)
  (cond ((and (gram? a) (gram? b))
         (gram-add a b))
        ((and (slug? a) (slug? b))
         (slug-add a b))
        ((and (gram? a) (slug? b))
         (slug-add (gram-to-slug a) b))
        ((and (slug? a) (gram? b))
         (slug-add a (gram-to-slug b)))
        (else (error "mass-add given incompatible units:" a b))))

;; Exercise C.1.f
(define (mass? a)
  (or (gram? a) (slug? a)))

;; Exercise C.2.a
;; -> '(gram . 43)

;; Exercise C.2.b
;; -> '(slug . 1.1370435292176577)

;; Exercise C.2.c
;; -> '(slug . 4)

;; Exercise C.2.d
;; -> error: incompatible units

;; Part D: Message Passing
(define (make-gram2 x)
  (define as-slug (* x (/ 1 14593.903203)))
  (lambda (op . args)
    (cond
      ((eq? op 'get-mass) x)
      ;; Exercise D.1.a
      ((eq? op 'get-slug) (make-gram2 as-slug))
      ;; Exercise D.1.b
      ((eq? op 'get-gram) x)
      ;; Exercise D.1.c
      ((eq? op 'unit-type) 'gram)
      ;; Exercise D.1.d
      ((eq? op 'compatible)
       (let ((other (car args)))
         (number? (x 'add other))))
      ;; Exercise D.1.e
      ((eq? op 'add)
       (let ((other (car args)))
         (cond
           ((eq? (other 'unit-type) 'gram)
            (make-gram2 (+ x (other 'get-mass))))
           ((eq? (other 'unit-type) 'slug)
            (make-gram2 (+ x (* (other 'get-mass) as-slug))))
           (else (error "unknown type: "
                        (other 'unit-type)))))))))

;; Exercise D.2.1
;; make-product: mp-expr mp-expr -> mp-expr
;; Define a product as a message-passing object.
(define (make-product exp1 exp2)
  (cond ((exp1 'zero?) (make-number 0)) ;; exp * 0 --> 0
        ((exp2 'zero?) (make-number 0))
        ((and (exp1 'number?) (= (exp1 'value) 1)) exp2) ;; exp * 1 --> exp
        ((and (exp2 'number?) (= (exp2 'value) 1)) exp1)
        ((and (exp1 'number?) (exp2 'number?))
         (make-number (* (exp1 'value) (exp2 'value)))) ;; num * num --> num
        (else  ;; create a new message-passing object representing the product
         (lambda (m . args)
           (cond ((eq? m 'derive) 
                  (if (and (= (length args) 1)
                           (symbol? (car args)))
                      (let ((variable (car args)))
                        ;; derivative of a product
                        (make-sum (make-product (exp1 'derive variable) exp2)
                                  (make-product exp1 (exp2 'derive variable))))
                      (error "derive needs a variable argument")))
                 ((eq? m 'print) (list '* (exp1 'print) (exp2 'print)))
                 ((eq? m 'zero?) #f)
                 ((eq? m 'number?) #f)
                 ((eq? m 'value) 
                  (error
                   "should not ask for the value of a product expression"))
                 ((eq? m 'evaluate) 
                  (if (and (= (length args) 2)
                           (symbol? (car args))
                           (number? (cadr args)))
                      (let ((variable (car args))
                            (number   (cadr args)))
                        (let ((exp1-eval (exp1 'evaluate variable number))
                              (exp2-eval (exp2 'evaluate variable number)))
                          (make-product exp1-eval exp2-eval)))
                      (error "evaluate needs a variable symbol and a number")))
                 (else (error "unknown message: " m)))))))

;; Exercise D.2.2
(define (differentiate function var)
  (function 'derive var))

;; Exercise D.2.3.a
;; -> [no output]

;; Exercise D.2.3.b
;; -> [no output]

;; Exercise D.2.3.c
;; -> '(+
;;  (+ (* x (* x y)) (* x (+ (* x y) (* x y))))
;;  (* 3 (+ (* x (* y y)) (* x (* y y)))))

;; Exercise D.2.3.d
;; -> '(+
;;  (* 3 (* 3 (* 3 y)))
;;  (+ (* 3 (* 3 (* 3 (* y y)))) (+ (* y y) 2)))

;; Exercise D.2.3.e
;; -> 558

;; Exercise D.2.3.f
;; -> 396
;


;;; make-number: number -> mp-number
;;; Define a number as a message-passing object. 
;;; "mp-number" in the contract means "message-passing number".
;;; "value" is just an ordinary Scheme number.
;(define (make-number value)
;  (lambda (m . args) ;; remember dotted tail notation!
;    (cond ((eq? m 'derive) (make-number 0))  ; derivative of a number is 0
;          ((eq? m 'print) value)
;          ((eq? m 'evaluate) 
;           (make-number value)) ; must evaluate to a message-passing object
;          ((eq? m 'zero?) (= value 0))
;          ((eq? m 'number?) #t)
;          ((eq? m 'value) value)
;          (else (error "unknown message: " m)))))
;
;;; make-variable: symbol -> mp-variable
;;; Define a variable as a message-passing object.
;;; "mp-variable" in the contract means "message-passing variable".
;;; "varname" is a Scheme symbol.
;(define (make-variable varname)         
;  (lambda (m . args)
;    (cond ((eq? m 'derive) 
;           (if (and (= (length args) 1) ;; length returns the length of a list
;                    (symbol? (car args)))
;               (if (eq? (car args) varname)
;                   (make-number 1)
;                   (make-number 0))
;               (error "derive needs a variable argument")))
;          ((eq? m 'print) varname)
;          ((eq? m 'zero?) #f)
;          ((eq? m 'number?) #f)
;          ((eq? m 'value) 
;           (error "should not be asking for the value of a variable"))
;          ((eq? m 'evaluate) 
;           (if (and (= (length args) 2)
;                    (symbol? (car args))
;                    (number? (cadr args)))  ;; n.b. (cadr args) is same as (car (cdr args))
;               (if (eq? varname (car args)) 
;                   (make-number (cadr args))
;                   (make-variable varname))
;               (error "evaluate needs a variable symbol and a number")))
;          (else (error "unknown message: " m)))))
;
;;; make-sum: mp-expr mp-expr -> mp-expr
;;; Define a sum as a message-passing object.
;;; "mp-expr" in the contract means "message-passing expression",
;;; i.e. a message-passing object representing an algebraic expression.
;(define (make-sum exp1 exp2)
;  (cond ((exp1 'zero?) exp2) ;; exp + 0 --> exp
;        ((exp2 'zero?) exp1)
;        ((and (exp1 'number?) (exp2 'number?))
;         (make-number (+ (exp1 'value) (exp2 'value)))) ;; num + num --> num
;        (else  ;; create a new message-passing object representing the sum
;         (lambda (m . args)
;           (cond ((eq? m 'derive) 
;                  (if (and (= (length args) 1)
;                           (symbol? (car args)))
;                      (let ((variable (car args)))
;                        ;; derivative of a sum is the sum of the derivatives
;                        ;; of the parts of the sum
;                        (make-sum (exp1 'derive variable) 
;                                  (exp2 'derive variable)))
;                      (error "derive needs a variable argument")))
;                 ((eq? m 'print) (list '+ (exp1 'print) (exp2 'print)))
;                 ((eq? m 'zero?) #f)
;                 ((eq? m 'number?) #f)
;                 ((eq? m 'value) 
;                  (error "should not be asking for the value of a sum expression"))
;                 ((eq? m 'evaluate) 
;                  (if (and (= (length args) 2)
;                           (symbol? (car args))
;                           (number? (cadr args)))
;                      (let ((variable (car args))
;                            (number   (cadr args)))
;                        (let ((exp1-eval (exp1 'evaluate variable number))
;                              (exp2-eval (exp2 'evaluate variable number)))
;                          (make-sum exp1-eval exp2-eval)))
;                      (error "evaluate needs a variable symbol and a number")))
;                 (else (error "unknown message: " m)))))))
;
;;; evaluate: mp-expr symbol number -> mp-expr
;;; Evaluate a message-passing expression with a number
;;; substituted for a variable.
;(define (evaluate expression variable value)
;  (expression 'evaluate variable value))
;
;;; print: mp-expr -> (list-of symbols OR symbol OR number)
;;; Return the expression as its representation as a Scheme list,
;;; or as a symbol or a number if possible.
;(define (print expression)
;  (expression 'print))
;
;;; We ask you to define differentiate below.
;
;


(generate-report)