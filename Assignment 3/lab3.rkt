#lang racket
(require htdp/testing)

;; Part A: Abstraction and cons
;; Exercise A.1
;; make-point: Number Number -> (Pairof Number Number)
;; x-point: Point -> Number
;; y-point: Point -> Number
(define (make-point x-coord y-coord) (cons x-coord y-coord))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

;; make-segment: Point Point -> (Pairof Point Point)
;; start-segment: Segment -> Point
;; end-segment: Segment -> Point
(define (make-segment point1 point2) (cons point1 point2))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;; midpoint-segment: Segment -> Point
(define (midpoint-segment segment)
  (make-point
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

;; segment-length: Segment -> Number
(define (segment-length segment)
  (sqrt (+ (expt (- (x-point (end-segment segment)) (x-point (start-segment segment))) 2)
           (expt (- (y-point (end-segment segment)) (y-point (start-segment segment))) 2))))

;; print-point: Point -> String
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;(define p1 (make-point 0.0 0.0))
;(define p2 (make-point 10.0 0.0))
;(define p3 (make-point 10.0 10.0))
;(define s1 (make-segment p1 p2))
;(define s2 (make-segment p2 p3))
;(define s3 (make-segment p3 p1))
;(check-within (segment-length s1) 10.0 0.00001)
;(check-within (segment-length s2) 10.0 0.00001)
;(check-within (segment-length s3) 14.14213 0.00001)
;(check-within (x-point (midpoint-segment s3)) 5.0 0.00001)
;(check-within (y-point (midpoint-segment s3)) 5.0 0.00001)

;; Exercise A.2
;;; make-rectangle: Point Point -> (Pairof Point Point)
;;; Given a pair of points, lower-left and upper-right
;(define (make-rectangle upper-right lower-left) (cons upper-right lower-left))
;;; rectangle-lower-segment: Rectangle -> Segment
;;; rectangle-upper-segment: Rectangle -> Segment
;;; rectangle-left-segment: Rectangle -> Segment
;;; rectangle-right-segment: Rectangle -> Segment
;(define (rectangle-lower-segment rectangle) 
;  (make-segment (cdr rectangle) (make-point (x-point (car rectangle))
;                                            (y-point (cdr rectangle)))))
;(define (rectangle-upper-segment rectangle)
;  (make-segment (car rectangle) (make-point (x-point (cdr rectangle))
;                                            (y-point (car rectangle)))))
;(define (rectangle-left-segment rectangle)
;  (make-segment (cdr rectangle) (make-point (x-point (cdr rectangle))
;                                            (y-point (car rectangle)))))
;(define (rectangle-right-segment rectangle)
;  (make-segment (car rectangle) (make-point (x-point (car rectangle))
;                                            (y-point (cdr rectangle)))))

;; make-rectangle: Number Number Number Number -> (Pairof Point Point)
;; Given 4 numbers, lower & upper x, lower & upper y
(define (make-rectangle lowerx lowery upperx uppery)
  (vector lowerx lowery upperx uppery))
;; rectangle-lower-segment: Rectangle -> Segment
;; rectangle-upper-segment: Rectangle -> Segment
;; rectangle-left-segment: Rectangle -> Segment
;; rectangle-right-segment: Rectangle -> Segment
(define (rectangle-lower-segment rectangle)
  (make-segment (make-point (vector-ref rectangle 0) (vector-ref rectangle 1))
                (make-point (vector-ref rectangle 2) (vector-ref rectangle 1))))
(define (rectangle-upper-segment rectangle)
  (make-segment (make-point (vector-ref rectangle 0) (vector-ref rectangle 3))
                (make-point (vector-ref rectangle 2) (vector-ref rectangle 3))))
(define (rectangle-left-segment rectangle)
  (make-segment (make-point (vector-ref rectangle 0) (vector-ref rectangle 1))
                (make-point (vector-ref rectangle 0) (vector-ref rectangle 3))))
(define (rectangle-right-segment rectangle)
  (make-segment (make-point (vector-ref rectangle 2) (vector-ref rectangle 1))
                (make-point (vector-ref rectangle 2) (vector-ref rectangle 3))))

;; rectangle-perimeter: Rectangle -> Number
(define (rectangle-perimeter rectangle)
  (+ (segment-length (rectangle-lower-segment rectangle))
     (segment-length (rectangle-upper-segment rectangle))
     (segment-length (rectangle-left-segment rectangle))
     (segment-length (rectangle-right-segment rectangle))))

;; rectangle-area: Rectangle -> Number
(define (rectangle-area rectangle)
  (* (segment-length (rectangle-left-segment rectangle))
     (segment-length (rectangle-lower-segment rectangle))))

;(define p1 (make-point 0.0  2.0))
;(define p2 (make-point 5.0 10.0))
;(define r (make-rectangle p1 p2))             ;; first representation
;(define r (make-rectangle 0.0 2.0 5.0 10.0))  ;; second representation: lx, ly, ux, uy
;
;(check-within (rectangle-area r)      40.0 0.0001)
;(check-within (rectangle-perimeter r) 26.0 0.0001)

;; Exercise A.3
(define (new-cons x y)
  (lambda (m) (m x y)))

(define (new-car z)
  (z (lambda (p q) p)))

(define (new-cdr z)
  (z (lambda (p q) q)))
;; Substitution model for (new-cdr (new-cons 1 2))
;; Evaluate (new-cons 1 2) -> the body of new-cons is retrieved:
;; (lambda (m) (m x y))
;; The parameters x and y are substituted: (lambda (m) (m 1 2))
;; The body of new-cdr is retrieved:
;; (z (lambda (p q) q))
;; The parameter z is substituted: ((lambda (m) (m 1 2)) (lambda (p q) q))
;; (lambda (p q) q) is sbustituted into m: (lambda (lambda (p q) q) (lambda (p q) q 1 2))
;; Values 1 2 are substituted into p q
;; lambda (1 2) 2 -> 2 is returned

;; Exercise A.4
;; new-cons-2: Integer Integer -> Integer
(define (new-cons-2 a b)
  (* (expt 2 a) (expt 3 b)))

;; new-car-2: Integer -> Integer
(define (new-car-2 a)
  (new-car-2-helper a 0))
;; new-car-2-helper: Integer Integer -> Integer
(define (new-car-2-helper num count)
  (if (not (= (remainder num 2) 0))
      count
      (new-car-2-helper (/ num 2) (+ count 1))))
;; new-cdr-2: Integer -> Integer
(define (new-cdr-2 a)
  (new-cdr-2-helper a 0))
;; new-cdr-2-helper: Integer Integer -> Integer
(define (new-cdr-2-helper num count)
  (if (not (= (remainder num 3) 0))
      count
      (new-cdr-2-helper (/ num 3) (+ count 1))))

;(define alpha (new-cons-2 5 7))
;(check-expect (new-car-2 alpha) 5)
;(check-expect (new-cdr-2 alpha) 7)

;; Exercise A.5
(define uzero (list))
;; uzero?: Unary -> Boolean
(define (uzero? u) (null? u))
;; usucc: Unary -> Unary
(define (usucc u) (cons 1 u))

;; uprev: Unary -> Unary
(define (uprev u)
  (cdr u))

;; integer-to-unary: Integer -> Unary
;; integer-to-unary-helper: Integer Unary -> Unary
(define (integer-to-unary num)
  (integer-to-unary-helper num (list)))
(define (integer-to-unary-helper num list)
  (if (= num 0)
      list
      (integer-to-unary-helper (- num 1) (usucc list))))

;; unary-to-integer: Unary -> Integer
;; unary-to-integer-helper: Integer Unary -> Integer
(define (unary-to-integer list)
  (unary-to-integer-helper 0 list))
(define (unary-to-integer-helper num list)
  (if (uzero? list)
      num
      (unary-to-integer-helper (+ num 1) (uprev list))))

;; unary-add: Unary Unary -> Unary
(define (unary-add list1 list2)
  (integer-to-unary (+ (unary-to-integer list1) (unary-to-integer list2))))

;; To change the representation from a list of 1s to empty lists, simply
;; redefine usucc: (define (usucc u) (cons (list) u))

;(check-expect (uprev '(1)) '())
;(check-expect (uprev '(1 1 1 1 1)) '(1 1 1 1))
;(check-expect (integer-to-unary 0) '())
;(check-expect (integer-to-unary 1) '(1))
;(check-expect (integer-to-unary 10) '(1 1 1 1 1 1 1 1 1 1))
;(check-expect (unary-to-integer '(1 1 1 1 1 1 1 1 1)) 9)
;(check-expect (unary-to-integer '(1 1 1)) 3)
;(check-expect (unary-to-integer '(1)) 1)
;(check-expect (unary-to-integer '()) 0)
;(check-expect (unary-add '(1 1 1) '()) '(1 1 1))
;(check-expect (unary-add '() '(1 1 1)) '(1 1 1))
;(check-expect (unary-add '(1 1) '(1 1 1)) '(1 1 1 1 1))
;(check-expect 
; (unary-to-integer
;  (unary-add 
;   (integer-to-unary 1001)
;   (integer-to-unary 65535)))
; 66536)

;; Exercise A.6
(define zero (lambda (s) (lambda (z) z)))

(define (add-1 n)
  (lambda (s) (lambda (z) (s ((n s) z)))))

(define one (lambda (s) (lambda (z) (s z))))
(define two (lambda (s) (lambda (z) (s (s z)))))
(define three (lambda (s) (lambda (z) (s (s (s z))))))
(define four (lambda (s) (lambda (z) (s (s (s (s z)))))))
(define five (lambda (s) (lambda (z) (s (s (s (s (s z))))))))
(define six (lambda (s) (lambda (z) (s (s (s (s (s (s z)))))))))
(define seven (lambda (s) (lambda (z) (s (s (s (s (s (s (s z))))))))))
(define eight (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s z)))))))))))
(define nine (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s (s z))))))))))))
(define ten (lambda (s) (lambda (z) (s (s (s (s (s (s (s (s (s (s z)))))))))))))

(define (Church-to-integer s)
  ((s (lambda (z) (+ z 1))) 0))

(define (add a b)
  (lambda (s) (lambda (z) ((a s) ((b s) z)))))

;(check-expect (Church-to-integer zero)   0)
;(check-expect (Church-to-integer one)    1)
;(check-expect (Church-to-integer two)    2)
;(check-expect (Church-to-integer three)  3)
;(check-expect (Church-to-integer four)   4)
;(check-expect (Church-to-integer five)   5)
;(check-expect (Church-to-integer six)    6)
;(check-expect (Church-to-integer seven)  7)
;(check-expect (Church-to-integer eight)  8)
;(check-expect (Church-to-integer nine)   9)
;(check-expect (Church-to-integer ten)   10)
;
;(check-expect (Church-to-integer (add zero three)) 3)
;(check-expect (Church-to-integer (add one  three)) 4)
;(check-expect (Church-to-integer (add two  three)) 5)
;(check-expect (Church-to-integer (add three zero)) 3)
;(check-expect (Church-to-integer (add three one))  4)
;(check-expect (Church-to-integer (add three two))  5)

;; Part B: Working with lists
;; Exercise B.1
;; last-pair: (Listof Any) -> (Listof Any)
(define (last-pair listinput)
  (cond ((null? listinput) (error "list must be non-empty!"))
        ((> (length listinput) 1) (last-pair (cdr listinput)))
        (else listinput)))

(check-error (last-pair (list)) "list must be non-empty!")
(check-expect (last-pair (list 4 2 51 1)) (list 1))
(check-expect (last-pair (list 6)) (list 6))
(check-expect (last-pair (list 1 2 "huh" 5 "hmm")) (list "hmm"))
 
;; Exercise B.2
;; reverse: (Listof Any) -> (listof Any)
;; reverse-helper: (Listof Any) Integer -> (listof Any)
(define (reverse listinput)
  (if (null? listinput)
      (list)
      (append (reverse (cdr listinput)) (list (car listinput)))))

(check-expect (reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
(check-expect (reverse (list 1)) (list 1))

;; Exercise B.3
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (expt (car items) 2) (square-list (cdr items)))))
;
;(define (square-list2 items)
;  (map (lambda (x) (expt x 2)) items))

;; Exercise B.4
;; The list is in reverse order because the appending at the tail-recursion occurs
;; at the front of the list (cons answer (...)) -> '(answer (...)) rather than
;; '((...) answer).
;;
;; By reversing the order, Louis creates a non-list pair. Instead of the expected list,
;; he is getting: '(answer . (...)).
;; 
;; Louis' solution could be fixed by:
;; > ...(cons answer (square ... -> ...(append answer (list (square (car things))))...
;; (replace cons with append, call function list)
;; This is not efficient; because append traverses the list to add at the end, the
;; time complexity is quadratic.

;; Exercise B.5
;; (append x y)
;; > '(1 2 3 4 5 6)
;; (cons x y)
;; > '((1 2 3) 4 5 6)
;; (list x y)
;; > '((1 2 3) (4 5 6))

;; Exercise B.6.1
;; count-negative: (Listof Number) -> Number
;; count-negative-helper: (Listof Number) Number -> Number
(define (count-negative input)
  (count-negative-helper input 0))
(define (count-negative-helper input total)
  (if (null? input)
      total
      (if (< (car input) 0)
          (count-negative-helper (cdr input) (+ total 1))
          (count-negative-helper (cdr input) total))))

(check-expect (count-negative (list -2 -1 0 1)) 2)
(check-expect (count-negative (list)) 0)
(check-expect (count-negative (list 1 2 3)) 0)

;; Exercise B.6.2
;; powers-of-n: Integer -> (Listof Integer)
(define (powers-of-n n)
  (if (= n 0)
      (list)
      (append (powers-of-n (- n 1)) (list (expt 2 (- n 1))))))

(check-expect (powers-of-n 0) (list))
(check-expect (powers-of-n 1) (list 1))
(check-expect (powers-of-n 4) (list 1 2 4 8))

;; Exercise B.6.3
;; prefix-sum: (List of Number) -> (List of Number)
;; prefix-sum-helper: (List of Number) Number -> (List of Number)
(define (prefix-sum input)
  (prefix-sum-helper input 0 (list)))
(define (prefix-sum-helper input total final-list)
  (if (null? input)
      final-list
      (prefix-sum-helper (cdr input) 
                         (+ total (car input)) 
                         (append final-list (list (+ total (car input)))))))

(check-expect (prefix-sum (list)) (list))
(check-expect (prefix-sum (list 1 2 3)) (list 1 3 6))

;; Exercise B.7.1
;(define (find-max-y a-list)
;  (define (find-max-y-helper current-list current-max-y)
;    (let ((max-y (get-y (car current-list))))
;      (cond ((null? current-list) current-max-y)
;            ((< max-y current-max-y)
;             (find-max-y-helper (cdr current-list) current-max-y))
;            (else (find-max-y-helper (cdr current-list)
;                                     max-y)))))
;  (find-max-y-helper a-list 0))
;; The code does not run slower because (cdr current-list) occurs in two
;; different cond paths; therefore for each loop it is actually only
;; calculated once.

;; Exercise B.7.2
;(define (find-x-for-max-y a-list)
;  (define (find-x-for-max-y-helper current-list current-max-y current-max-x)
;    (let ((max-y (get-y (car current-list)))
;          (max-x (get-x (car current-list)))
;      (cond ((null? current-list) current-max-x)
;            ((< max-y current-max-y)
;             (find-x-for-max-y-helper (cdr current-list) current-max-y current-max-x))
;            (else (find-x-for-max-y-helper (cdr current-list)
;                                     max-y max-x))))))
;  (find-x-for-max-y-helper a-list 0 0))

;; Exercise B.8
;; deep-reverse: (Listof All) -> (Listof All)
;; deep-reverse-helper: (Listof All) -> (Listof All)
(define (deep-reverse input)
  (define (deep-reverse-helper input output)
    (if (null? input)
        output
        (if (list? (car input))
            (deep-reverse-helper (cdr input) (cons (deep-reverse-helper (car input) (list)) output))
            (deep-reverse-helper (cdr input) (cons (car input) output)))))
  (deep-reverse-helper input (list)))

;(check-expect (deep-reverse (list)) (list))
;(check-expect (deep-reverse (list 1 2 3 4 5)) (list 5 4 3 2 1))
;(check-expect 
;  (deep-reverse (list (list 1 2) (list 3 4) 5)) 
;  (list 5 (list 4 3) (list 2 1)))
;(check-expect 
;  (deep-reverse 
;    (list (list (list 1 2) (list 3 4)) 
;          (list (list 5 6) (list 7 8))))
;    (list (list (list 8 7) (list 6 5))
;          (list (list 4 3) (list 2 1))))
  
(generate-report)