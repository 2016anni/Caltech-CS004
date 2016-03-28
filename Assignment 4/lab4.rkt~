#lang racket
(require htdp/testing)

;; Part A: Lists
;; make-mobile: Length Length -> (Listof Length)
(define (make-mobile left right)
  (list left right))
;; make-branch: Number (Number or Mobile) -> (Listof Number (Number or Mobile))
(define (make-branch length structure)
  (list length structure))

;; Exercise A.1.a
;; left-branch: Mobile -> Mobile
;; right-branch: Mobile -> Mobile
;; branch-length: Branch -> Length
;; branch-structure: Branch -> (Weight or Mobile)
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
;; Exercise A.1.b
;; submobile-weight: Branch -> Weight
;; total-weight: Mobile -> Weight
;; We need a separate function to account for a branch having a mobile on one arm instead
(define (submobile-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (total-weight mobile)
  (+ (submobile-weight (left-branch mobile)) (submobile-weight (right-branch mobile))))
;; Exercise A.1.c
;; branch-torque: Branch -> Torque
;; balanced?: Mobile -> Boolean
;; submobile-balanced?: Branch -> Boolean
;; We follow a similar procedure as determining the branch weights by making a separate
;; function.
(define (branch-torque branch)
  (* (branch-length branch) (submobile-weight branch)))
(define (submobile-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      true))
(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
       (submobile-balanced? (left-branch mobile))
       (submobile-balanced? (right-branch mobile))))
;; Exercise A.1.d
;(define (right-branch mobile)
;  (cdr mobile))
;(define (branch-structure branch)
;  (cdr branch))
;; We simply delete the "car" in front of the cdr, as using cons instead of list will
;; simply make it so that the cdr returns values instead of more lists.

;(define m0
;  (make-mobile (make-branch 1 1) (make-branch 1 1)))
;
;(define m1
;  (make-mobile
;    (make-branch 3 4) 
;    (make-branch 4 
;      (make-mobile
;         (make-branch 1 2) 
;         (make-branch 2 1)))))
;
;(define m2
;  (make-mobile
;    (make-branch 1 400)
;    (make-branch 10
;      (make-mobile
;        (make-branch 100 1)
;        (make-branch 1 200)))))
;
;(check-expect (total-weight m0) 2)
;(check-expect (balanced? m0) #t)
;(check-expect (total-weight m1) 7)
;(check-expect (balanced? m1) #t)
;(check-expect (total-weight m2) 601)
;(check-expect (balanced? m2) #f)

;; Exercise A.2
;; square-tree: (Listof Numbers) -> (Listof Numbers)
;;; Directly
;(define (square-tree tree)
;  (cond ((equal? (list) tree) (list))
;        ((not (list? tree)) (expt tree 2))
;        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))
;;; map and recursion
;(define (square-tree tree)
;  (map (lambda (sub-tree) (if (list? sub-tree)
;                              (square-tree sub-tree)
;                              (expt sub-tree 2)))
;       tree))

;; Exercise A.3
;; tree-map: Proc Tree -> Tree
;; square-tree: Tree -> Tree
(define (tree-map proc tree)
  (cond ((equal? (list) tree) (list))
        ((not (list? tree)) (proc tree))
        (else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree))))))

;; square: Number -> Number
(define (square x)
  (expt x 2))

(define (square-tree tree) (tree-map square tree))

;(define nil (list))
;(define a-tree (list 10 (list 20 (list 42 nil 12) nil (list 13 nil)) nil (list 1 2 3))) 
;(check-expect (square-tree nil) nil)
;(check-expect (square-tree (list 10)) (list 100)) 
;(check-expect (square-tree a-tree)
;  (list 100 (list 400 (list 1764 nil 144) nil (list 169 nil)) nil (list 1 4 9)))

;; Exercise A.4
;; subsets: List -> (Listof Lists)
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
;; The filled in function concatenates a specific element in s with x.
;; What "subsets" does is list all the subsets of the set that contains
;; some specific element and the complement of that set.

(check-expect (subsets (list)) (list (list)))
(check-expect (subsets (quote (1))) (quote (() (1))))
(check-expect (subsets (quote (1 2 3))) (quote (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))))

;; Exercise A.5
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (new-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (new-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(check-expect (new-map square (list)) (list))
(check-expect (new-map square (list 1 2 3 4 5)) (list 1 4 9 16 25))
(check-expect (new-append (list) (list)) (list))
(check-expect (new-append (list 1 2 3) (list)) (list 1 2 3))
(check-expect (new-append (list) (list 4 5 6)) (list 4 5 6))
(check-expect (new-append (list 1 2 3) (list 4 5 6)) (list 1 2 3 4 5 6))
(check-expect (new-length (list)) 0)
(check-expect (new-length (list 1 2 3 4 5)) 5)

;; Exercise A.6
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(check-expect (accumulate-n + 0 '(())) '())
(check-expect (accumulate-n * 1 '((1 2 3 4 5) (1 1 2 6 24)))
  '(1 2 6 24 120))
(check-expect (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  '(22 26 30))

;; Exercise A.7
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (vector) (dot-product vector v)) m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (vector) (matrix-*-vector cols vector)) m)))

(check-expect (dot-product '() '()) 0)
(check-expect (dot-product '(1 2 3) '(4 5 6)) 32)
(check-expect (matrix-*-vector '((1 0) (0 1)) '(10 20)) '(10 20))
(check-expect (matrix-*-vector '((1 2) (3 4)) '(-2 3)) '(4 6))
(check-expect (transpose '((1 2) (3 4))) '((1 3) (2 4)))
(check-expect (transpose '((1 2 3) (4 5 6))) '((1 4) (2 5) (3 6)))
(check-expect (matrix-*-matrix '((1 0) (0 1)) '((1 2) (3 4))) '((1 2) (3 4)))
(check-expect (matrix-*-matrix '((1 2) (3 4)) '((1 2) (3 4))) '((7 10) (15 22)))
(check-expect (matrix-*-matrix '((1 2 3) (4 5 6)) '((1 2) (3 4) (5 6))) 
  '((22 28) (49 64)))

;; Part B: Structural and Generative recursion
;; Exercise B.1
;; quicksort: (Listof Numbers) -> (Listof Numbers)
(define (quicksort input)
  (if (null? input)
      input
      (let ([pivot (car input)])
        (append (quicksort (filter (lambda (x) (< x pivot)) (cdr input)))
                (list pivot)
                (quicksort (filter (lambda (x) (>= x pivot)) (cdr input)))))))

(check-expect (quicksort '()) '())
(check-expect (quicksort '(1)) '(1))
(check-expect (quicksort '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (quicksort '(5 4 3 2 1 1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))

;; Exercise B.2
;; Quicksort is generative because it recurses on inputs that are newly generated by
;; the function (in this case it recurses on the newly generated inputs, [list of
;; elements < pivot] and [list of elements <= pivot].

;; Exercise B.3
;; The entire point of merge-sort is to break a list down to unit sizes, then begin
;; comparing adjacent units before merging appropriately. If the base case of list
;; length 1 is removed, then the "unit" of the list effectively becomes 0/infinitely
;; small. We have that base case to tell our function when to stop breaking down our
;; list (i.e. break it down once the length of the unit lists is 1).

;; Exercise B.4
;; Helper procedures:
;; insert-in-order: List List -> List
(define (insert-in-order new-result a-list) 
  (if (or (null? a-list) (goes-before? new-result (car a-list)))
      (cons new-result a-list)
      (cons (car a-list) 
         (insert-in-order new-result (cdr a-list)))))

(define (goes-before? r1 r2) (< r1 r2))  ; ascending order

;; The insertion sort function itself:
;; insertion-sort: List -> List
(define (insertion-sort a-list)
  (if (null? a-list)
      a-list
      (insert-in-order (car a-list) (insertion-sort (cdr a-list)))))
;; Structural recursion; no new information is created. Rather, the sort strictly
;; only uses information provided by the input list.

;; Part C: Quoting
;; Exercise C.1
;; ' is shorthand for quote. Thus, (car ''abracadabra) -> (car (quote (quote abracadabra))).
;; (quote (quote abracadabra)) -> '(quote abracadabra), where "quote" and "abracadabra" are
;; two symbols. The car of this list is, thus, simply "quote".

;; Exercise C.2

;;;; CODE FROM CHAPTER 2 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;; so that they
;;;  are easy to find, and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; SECTION 2.3.2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))

;(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; exponentiation?: Equation -> Boolean
;; base: Equation -> Number
;; exponent: Equation -> Number
;; make-exponentiation: Number Number -> Equation
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp)) (expt base exp))
        (else (list '** base exp))))

(check-expect (deriv 10 'x) 0)
(check-expect (deriv 'x 'x) 1)
(check-expect (deriv '(** y 1) 'x) 0)
(check-expect (deriv '(** x 1) 'x) 1)
(check-expect (deriv '(** x 2) 'x) '(* 2 x))
(check-expect (deriv '(** (* 3 x) 3) 'x) '(* (* 3 (** (* 3 x) 2)) 3))
(check-expect (deriv '(+ (* 4 (** x 3)) (* 6 (** x 2))) 'x)
  '(+ (* 4 (* 3 (** x 2))) (* 6 (* 2 x))))

(generate-report)