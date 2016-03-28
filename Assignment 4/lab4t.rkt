#lang typed/racket

(struct: Mobile ([left : Branch] [right : Branch]))
(struct: Branch ([length : Number] [structure : (U Number Mobile)]))

(: submobile-weight (Branch -> Number))
(define (submobile-weight branch)
  (define var (Branch-structure branch))
  (if (number? var)
      var
      (total-weight var)))

(: total-weight (Mobile -> Number))
(define (total-weight mobile)
  (+ (submobile-weight (Mobile-left mobile)) (submobile-weight (Mobile-right mobile))))

(: branch-torque (Branch -> Number))
(define (branch-torque branch)
  (* (Branch-length branch) (submobile-weight branch)))

(: submobile-balanced? (Branch -> Boolean))
(define (submobile-balanced? branch)
  (if (pair? (Branch-structure branch))
      (balanced? (Branch-structure branch))
      true))

(: balanced? (Mobile -> Boolean))
(define (balanced? mobile)
  (and (= (branch-torque (Mobile-left mobile)) (branch-torque (Mobile-right mobile)))
       (submobile-balanced? (Mobile-left mobile))
       (submobile-balanced? (Mobile-right mobile))))



(: m0 Mobile)
(define m0
  (Mobile (Branch 1 1) (Branch 1 1)))

(: m1 Mobile)
(define m1
  (Mobile
    (Branch 3 4) 
    (Branch 4 
      (Mobile
         (Branch 1 2) 
         (Branch 2 1)))))

(: m2 Mobile)
(define m2
  (Mobile
    (Branch 1 400)
    (Branch 10
      (Mobile
        (Branch 100 1)
        (Branch 1 200)))))

(and
  (= (total-weight m0) 2)
  (balanced? m0)
  (= (total-weight m1) 7)
  (balanced? m1)
  (= (total-weight m2) 601)
  (not(balanced? m2)))