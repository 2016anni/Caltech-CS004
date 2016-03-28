#lang racket
(require htdp/testing)

;; Part B: Streams
;; Exercise B.1
(define-syntax delay  
  (syntax-rules () 
    ((delay expr) (lambda () expr))))

(define (force x) (x))
(define the-empty-stream (list))
(define stream-null? null?)

(define-syntax cons-stream  
  (syntax-rules () 
    ((cons-stream x y) 
     (cons x (delay y)))))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;; stream-map: Procedure Stream -> Stream
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; Exercise B.2
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))
(define (take n s)
  (if (= n 0)
      (list)
      (cons (stream-car s) (take (- n 1) (stream-cdr s)))))

;; mul-streams: Stream Stream -> Stream
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; factorials: Stream Stream -> Stream
(define factorials (cons-stream 1 (mul-streams factorials integers)))


;; Exercise B.3
(define (average x y)
  (/ (+ x y) 2.0))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; stream-limit: Stream Number -> Number
(define (stream-limit s1 tol)
  (let ([s2 (stream-cdr s1)])
    (if (< (abs (- (stream-car s2) (stream-car s1))) tol)
        (stream-car s2)
        (stream-limit s2 tol))))

(define (new-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(check-expect (take 11 factorials) 
  '(1 1 2 6 24 120 720 5040 40320 362880 3628800))
(check-within (new-sqrt 2.0 0.00000000001) (sqrt 2.0) 0.00000000001)

;; Part C: Continuation-passing Style
;; Exercise C.1
;; fact-cps: integer (function integer -> integer) -> integer
(define (fact-cps n k)
  (if (= n 0)
      (k 1)
      (fact-cps (- n 1) (lambda (c) (k (* n c))))))
;; Ben is incorrect. This function still requires the same amount of space.
;; If special cases (such as stopping when multiplying by 0) were added, some space
;; could be saved.

;; Exercise C.2
;; fibonacci: integer -> integer
(define (fibonacci n) (fib-cps n (lambda (x) x)))

(define (fib-cps n k)
  (if (< n 2)
      (k n)
      (fib-cps (- n 1)
        (lambda (c)
          (fib-cps (- n 2) (k (+ (- n 1) c)))))))

(generate-report)