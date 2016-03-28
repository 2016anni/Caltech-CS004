#lang typed/racket

;; Part D: Typed Racket
;; Exercise D.1.1
;; This function computes the factorial of the input number,
;; which for a number n is equal to n * (n-1) * ... * 1.
(: factorial (Integer -> Integer))
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Exercise D.1.2
(: e-term (Integer -> Real))
(define (e-term n)
  (/ 1 (factorial n)))

;; Exercise D.1.3
(: e-approximation (Integer -> Real))
(define (e-approximation n)
  (if (= n -1)
      0.0
      (+ (e-term n) (e-approximation (- n 1)))))

;; Exercise D.2
(: ifib (Integer -> Integer))
(define (ifib n)
  (iter 1 0 n))
(: iter (Integer Integer Integer -> Integer))
(define (iter fnext f cnt)
  (if (= cnt 0)
      f
      (iter (+ fnext f) fnext (- cnt 1))))

;; Exercise D.3
(: power-of-two? (Real -> Boolean))
(define (power-of-two? n)
  (if (= n 1)
      #t
      (if (< n 1)
          #f
          (power-of-two? (/ n 2)))))

;; Exercise D.4
(: ipower-of-two? (Integer -> Boolean))
(define (ipower-of-two? n)
  (cond [(= n 1) #t]
        [(not (= (remainder n 2) 0)) #f]
        [(= (quotient n 2) 1) #t]
        [else (ipower-of-two? (quotient n 2))]))

;; I retract my previous statement. ipower-of-two? and
;; power-of-two? both have time complexity O(log(n)) for
;; "n" containing factors of 2. However, ipower-of-two?
;; has a separate case for numbers that do not contain any
;; factor of 2 (remainder != 0 -> #f). Because of this
;; case, for such cases, it has a time complexity of O(1);
;; the function returns a value much earlier.