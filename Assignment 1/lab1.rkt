#lang racket

;; Part A: Basic Exercises
;; Exercise A.1
;; 10
;; -> 10

;; (+ 5 3 4)
;; -> 12

;; (- 9 1)
;; -> 8

;; (/ 6 2)
;; -> 3

;; (+ (* 2 4) (- 4 6))
;; -> 6

;; (define a 3)
;; ->

;; (define b (+ a 1))
;; ->

;; (+ a b (* a b))
;; -> 19

;; (= a b)
;; -> #f

;; (if (and (> b a) (< b (* a b)))
;;    b
;;    a)
;; -> 4

;; (cond ((= a 4) 6)
;;      ((= b 4) (+ 6 7 a))
;;      (else 25))
;; -> 16

;; (+ 2 (if (> b a) b a))
;; -> 6

;; (* (cond ((> a b) a)
;;         ((< a b) b)
;;         (else -1))
;;   (+ a 1))
;; -> 16

;; Exercise A.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5)))) (* 3 (- 6 2) (- 2 7)))

;; Exercise A.3
(define (sum-of-squares-of-two-largest x y z)
  (if (and (< x y) (< x z))
      (+ (expt y 2) (expt z 2))
      (if (and (< y x) (< y z))
          (+ (expt x 2) (expt z 2))
          (+ (expt x 2) (expt y 2)))))

;; Exercise A.4
;; The function takes inputs, a and b. If b is a positive number, then it
;; is added to a. If b is 0 or negative, then it is subtracted from a. In
;; other words, the function adds the absolute value of b to a.
;; What (if (> b 0) + -) does: The if statement checks whether b is > 0
;; or not. If positive, the statement returns the operator "+". If
;; negative or zero, the statement returns the operator "-". This operator
;; is then applied to the values a and b. This statement basically functions
;; as the operator for adding the absolute value of b.


;; Part B: Evaluation
;; Exercise B.1
;; In an applicative order evaluation, the interpreter hangs. In a normal
;; order evaluation, the interpreter would return 0. In the normal order
;; evaluation, the procedure first tests if the first argument = 0. Since
;; in (test 0 (p)) it does equal 0, the interpreter returns 0 immediately
;; as according to the definition of test. In the applicative order
;; evaluation, the interpreter attempts to evaluate the inputs of test.
;; Though 0 evaluates as 0, it cannot evaluate (p), and thus it hangs.

;; Exercise B.2
;; The interpreter hangs. Unlike the built in if, new-if is a function.
;; In Scheme, each input of a function is evaluated first (applicative
;; order evaluation). Unforunately for Eva's case, this means evaluating
;; (good-enough? guess x), guess, and (sqrt-iter (improve guess x) x).
;; Of particular interest is the final input. This sqrt-iter call is
;; calling the function from within its definition. Due to this, the
;; interpreter runs another iteration of sqrt-iter, thus calling itself
;; again.

;; Exercise B.3
;; (if (= 2 0) 5 (inc (+ (dec 2) 5)))
;; Evaluate special form "if" (if (= 2 0) ...)
;; -> Evaluate (= 2 0) -> #f -> (if #f ...)
;; Evaluate the false case
;; Evaluate (inc (+ (dec 2) 5))
;; Evaluate 5 -> 5
;; Evaluate (dec 2) -> dec is built in -> 1
;; Evaluate (inc (+ 1 5))
;; Evaluate special form "if" (if (= 1 0) ...)
;; -> Evaluate (= 1 0) -> #f -> (if #f ...)
;; Evaluate the false case
;; Evaluate (inc (inc (+ (dec 1) 5))
;; Evaluate 5 -> 5
;; Evaluate (dec 1) -> dec is built in -> 0
;; Evaluate (inc (inc (+ 0 5)))
;; Evaluate special form "if" (if (= 0 0) ...)
;; -> Evaluate (= 0 0) -> #t -> (if #t ...)
;; Evaluate the true case -> 5
;; Evaluate (inc (inc 5))
;; Evaluate inc 5 -> inc is built in -> 6
;; Evaluate (inc 6)
;; Evaluate inc 6 -> inc is built in -> 7
;; Evaluate 7 -> Return 7
;; This is a recursive process.

;; (if (= 2 0) b (+ (dec a) (inc b))
;; Evaluate special form "if" (if (= 2 0) ...)
;; -> Evaluate (= 2 0) -> #f -> (if #f ...)
;; Evaluate the false case
;; Evaluate (+ (dec 2) (inc 5))
;; Evaluate (dec 2) -> dec is built in -> 1
;; Evaluate (inc 5) -> inc is built in -> 6
;; Evaluate (+ 1 6)
;; Evaluate special form "if" (if (= 1 0) ...)
;; -> Evaluate (= 1 0) -> #f -> (if #f ...)
;; Evaluate the false case
;; Evaluate (+ (dec 1) (inc 6))
;; Evaluate (dec 1) -> dec is built in -> 0
;; Evaluate (inc 6) -> inc is built in -> 7
;; Evaluate (+ 0 7)
;; Evaluate special form "if" (if (= 0 0) ...)
;; -> Evaluate (= 0 0) -> #t -> (if #t ...)
;; Evaluate the true case
;; Evaluate 7 -> Return 7
;; This is an iterative process.

;; Part C: Recursion
;; Exercise C.1
;; This function computes the factorial of the input number,
;; which for a number n is equal to n * (n-1) * ... * 1.
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Exercise C.1.a
(define (e-term n)
  (/ 1 (factorial n)))

;; Exercise C.1.b
(define (e-approximation n)
  (if (= n -1)
      0.0
      (+ (e-term n) (e-approximation (- n 1)))))

;; Exercise C.1.c
;; (exp 1) -> 2.718281828459045
;; (e-approximation 100) -> 2.7182818284590455

;; Exercise C.2
(define (even? n)
  (if (= n 0)
      #t
      (odd? (- n 1))))
(define (odd? n)
  (if (= n 0)
      #f
      (even? (- n 1))))

;; Exercise C.3
(define (f-recursive n) ;; Computation of f by means of recursive process
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n) ;; Computation of f by means of iterative process
  (define (f-helper last next current max)
    (if (= 3 max)
        (+ current (* 2 next) (* 3 last))
        (f-helper next current (+ current (* 2 next) (* 3 last)) (- max 1))))
  (if (< n 3)
      n
      (f-helper 0 1 2 n)))

;; Exercise C.4
(define (pascal row column)
  (cond ((< row 3) 1)
        ((< column 2) 1)
        ((= row column) 1)
        (else 
         (+ (pascal (- row 1) (- column 1)) (pascal (- row 1) column)))))
         
;; Exercise C.5
;; The "end case" in this procedure is the case in which n = 0 (>(if (= n 0) ...)).
;; However, at the end of the procedure on the false case for the if statement, the
;; n is divided by 2. Division of two integers which have no common factor results
;; in a rational number - it is not truncated such that n approximately = 0 after
;; enough loops. Because of this, the n doesn't ever actually = 0, and the procedure
;; runs indefinitely. To fix this issue, one can "cap" the accuracy of the result and
;; the duration of the procedure by setting n = 0 to a very small value instead
;; (ex. n = 0.000001).