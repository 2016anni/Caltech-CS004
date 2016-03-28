#lang typed/racket

;; Part D: Typed Racket
;; Exercise D.1
(: ilen (All (A) ((Listof A) -> Integer)))
(define (ilen lst)
  (: iter (All (A) ((Listof A) Integer -> Integer)))
  (define (iter current total)
    (if (null? current)
        total
        (iter (cdr current) (+ total 1))))
  (iter lst 0))

;; Exercise D.2
(: sum ((Integer -> Integer) Integer (Integer -> Integer) Integer -> Integer))
(define (sum term a next b)
  (: iter (Integer Integer -> Integer))
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise D.3
(: compose (All (A) ((A -> A) (A -> A) -> (A -> A))))
(define (compose f g)
  (lambda (x) (f (g x))))

(: repeated (All (A) ((A -> A) Integer -> (A -> A))))
(define (repeated f n)
  (if (= n 0)
      (lambda (x) x)
      (compose (repeated f (- n 1)) f)))

;; Tests
(: string-double (String -> String))
(define (string-double s) (string-append s s))
((repeated string-double 3) "ha")  
(string=? ((repeated string-double 3) "ha") "hahahahahahahaha")

(: double (Integer -> Integer))
(define (double n) (* n 2))
((repeated double 4) 10)
(equal? ((repeated double 4) 10) 160)