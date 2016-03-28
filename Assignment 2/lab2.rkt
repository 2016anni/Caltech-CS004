#lang racket
(require htdp/testing)

;; Part A: Orders of growth
;; Exercise A.1
;; Space complexity: O(n).
;; Time complexity has to do with how long each of the operations within the program takes to
;; perform. Space complexity has to do with how much memory the program requires; e.g. how many
;; pending operations there are.

;; Exercise A.2.a
;; 5 times.

;; Exercise A.2.b
;; Time complexity: sine is a recursive function for inputs greater than 3. The function calls
;; p, which is a function that is purely composed of basic operations (subtraction,
;; multiplication). p also calls cube, which is also a basic operation (multiplication). These two
;; functions are of constant time, so only the recursion in sine determines the order of growth.
;; The number of times is dependent on how many times the input is divided by 3. Thus, the time
;; complexity is log-base-3(a), which is also log(a).
;; O(log(a))
;;
;; Space complexity: Due to Scheme's applicative-order evaluation, at the end of the if statement,
;; the (/ angle 3.0) is computed first, then (sine (/ angle 3.0)) is computed before the
;; function p is called. The function p is put "on hold" for as many times the input is divided
;; by 3. As such, the space complexity is O(log(a)).

;; Exercise A.3
(define (even? n)
  (= (remainder n 2) 0))
(define (iter-expt b n)
  (iter-expt-helper b n 1))
(define (iter-expt-helper b n a)
  (cond ((= n 0) 1)
        ((= n 1) (* a b))
        ((even? n) (iter-expt-helper (* b b) (/ n 2) a))
        (else (iter-expt-helper b (- n 1) (* a b)))))

(check-expect (iter-expt 2 0) 1)
(check-expect (iter-expt 2 1) 2)
(check-expect (iter-expt 2 2) 4)
(check-expect (iter-expt 2 3) 8)
(check-expect (iter-expt 2 4) 16)

;; Exercise A.4
(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))
(define (recur-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (+ (recur-mult a (halve b)) (recur-mult a (halve b))))
        (else (+ (double a) (- b 1)))))

(check-expect (recur-mult 2 0) 0)
(check-expect (recur-mult 2 1) 2)
(check-expect (recur-mult 2 2) 4)
(check-expect (recur-mult 2 3) 6)
(check-expect (recur-mult 2 4) 8)

;; Exercise A.5
(define (iter-mult a b)
  (iter-mult-helper a b 0))
(define (iter-mult-helper a b total)
  (cond ((= b 0) 0)
        ((= b 1) (+ a total))
        ((even? b) (iter-mult-helper (double a) (halve b) total))
        (else (iter-mult-helper a (- b 1) (+ total a)))))

(check-expect (recur-mult 2 0) 0)
(check-expect (recur-mult 2 1) 2)
(check-expect (recur-mult 2 2) 4)
(check-expect (recur-mult 2 3) 6)
(check-expect (recur-mult 2 4) 8)

;; Exercise A.6
;; Time complexity: foo is a recursive function. It is called for as long as n > 1. Thus, foo
;; is called every time n is divided to 2. f is constant time, and the addition of the two foo
;; functions is constant time. Thus, the time complexity is O(log-base-2(n)).
;; O(log(n))
;;
;; Space complexity: for each recursion of the foo function, an addition is put on hold. The
;; (/ n 2) in the functions, as well as the call of f are constant space. foo is called
;; log-base-2 times, thus the space complexity follows suit.
;; O(log(n))

;; Part B: Evaluation
;; Exercise B.1.1
((lambda (x y) (* x (+ 2 y))) 20 (expt 2 4))

;; Exercise B.1.2
((lambda (a b c) (sqrt (- (* b b) (* 4.0 a c)))) 1 2 3)

;; Exercise B.1.3
((lambda (x y z) (* x y z)) 1 2 3)

;; Exercise B.1.4
((lambda (x y z) (* x y z)) 3 3 3)

;; Exercise B.2
;; We first desugar all lets into lambdas
;; (let ((x (* 2 10)) (y (+ 3 4))... -> Evaluate (* 2 10) -> 20
;; substitute 20 for x in (* x y z)
;; -> ((lambda x (* x y z)) 20)
;; Substitute (+ 3 4) for y in (* x y z) -> Evaluate (+ 3 4) -> 7
;; -> ((lambda (x y) (* x y z)) 20 7)
;; (let ((y 14))... -> substitute 14 for y in (* x y z)
;; -> ((lambda (x y) (* x y z)) 20 14)
;; (let ((z 22))... -> substitute 22 for z in (* x y z)
;; -> ((lambda (x y z) (* x y z)) 20 14 22)
;; (* x y z) -> (* 20 14 22)
;; Evaluate (* 20 14 22) -> 6160

;; Exercise B.3
;; When desugared into lambda, the following is constructed:
;; ((lambda (x y z) (+ x y z)) 10 (* x 2) (+ y 3))
;; When 10, (* x 2), and (+ y 3) are substituted into the body, the x, as the error states,
;; does not refer to anything.
;;
;; nested let solution:
(let ((x 10))
  (let ((y (* x 2)))
    (let ((z (+ y 3)))
      (+ x y z))))
;;
;; let* solution:
(let* ((x 10)
       (y (* x 2))
       (z (+ y 3)))
  (+ x y z))

;; Part C: Higher-order procedures
;; Exercise C.1
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (square n) (* n n))
(define (step1 n) (+ n 1))
(check-expect (sum square 10 step1 0) 0)
(check-expect (sum square 4 step1 4) 16)
(check-expect (sum square 0 step1 10) 385)

;; Exercise C.2.a
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
(define (factorial-iter b)
  (product-iter values 1 step1 b))

;; Exercise C.2.b
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))
(define (factorial-rec b)
  (product-rec values 1 step1 b))

(check-expect (factorial-rec  0)  1)
(check-expect (factorial-iter 0)  1)
(check-expect (factorial-rec  10) 3628800)
(check-expect (factorial-iter 10) 3628800)

;; Exercise C.3.a
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise C.3.b
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

;; For sum, set "combiner = +" and "null-value = 0".
;; For product, set "combiner = *" and "null-value = 1".
(check-expect (accumulate-iter * 1 values 1 step1 4) 24)
(check-expect (accumulate-rec * 1 values 1 step1 4) 24)
(check-expect (accumulate-iter + 0 values 1 step1 4) 10)
(check-expect (accumulate-rec + 0 values 1 step1 4) 10)

;; Exercise C.4
(define (compose f g)
  (lambda (x) (f (g x))))
 
;; (define (square n) (* n n)) ***Commented out because already defined previously***
(define (inc n) (+ n 1))
(check-expect ((compose square inc) 6) 49)
(check-expect ((compose inc square) 6) 37)

;; Exercise C.5
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose (repeated f (- n 1)) f)))

;; (define (square n) (* n n)) ***Commented out because already defined previously***
(check-expect ((repeated square 0) 6) 6)
(check-expect ((repeated square 1) 6) 36)
(check-expect ((repeated square 2) 6) 1296)

;; Exercise C.6
(define (smooth f dx)
  (lambda (x)(/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (nsmoothed f dx n)
  (define (smooth-dx f)
    (smooth f dx))
  ((repeated smooth-dx n) f))

(generate-report)