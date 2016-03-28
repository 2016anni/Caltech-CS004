#lang racket
(require scheme/mpair)
(provide (all-defined-out))

;; Part A: The game of Lights Out
;; Exercise A.1
(define (make-light)
    (let ([type 'light]
          [state 'off]
          [neighbors (list)])
      (lambda (op . args)
        (cond
          [(eq? op 'get-type) type]
          [(eq? op 'get-state) state]
          [(eq? op 'set-state!)
           (if (or (eq? (car args) 'on) (eq? (car args) 'off))
               (set! state (car args))
               (error "Input must be 'on or 'off!"))]
          [(eq? op 'toggle!)
           (if (eq? state 'on)
               (set! state 'off)
               (set! state 'on))]
          [(eq? op 'add-neighbor!) 
           (if (eq? ((car args) 'get-type) 'light)
               (set! neighbors (append neighbors (list (car args))))
               (error "Input must be a light object!"))]
          [(eq? op 'update!)
           (if (eq? state 'on)
               (set! state 'off)
               (set! state 'on))
           (for-each (lambda (x) (x 'toggle!)) neighbors)]
          [else (error "Must provide valid message!")]))))

;; Exercise A.2
(define (make-list-board)
    (let ([board (list (list (make-light)
                             (make-light)
                             (make-light)
                             (make-light)
                             (make-light))
                       (list (make-light)
                             (make-light)
                             (make-light)
                             (make-light)
                             (make-light))
                       (list (make-light)
                             (make-light)
                             (make-light)
                             (make-light)
                             (make-light))
                       (list (make-light)
                             (make-light)
                             (make-light)
                             (make-light)
                             (make-light))
                       (list (make-light)
                             (make-light)
                             (make-light)
                             (make-light)
                             (make-light)))])
      (lambda (op . args)
        (cond
          [(eq? op 'get)
           (if (or (or (> (car args) 4) (< (car args) 0))
                   (or (> (cadr args) 4) (< (cadr args) 0)))
               (error "Arguments must be between 0 and 4!")
               (list-ref (list-ref board (car args)) (cadr args)))]
          [else (error "Must provide valid message!")]))))

(define (make-vector-board)
    (let ([board (vector (vector (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light))
                         (vector (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light))
                         (vector (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light))
                         (vector (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light))
                         (vector (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)
                                 (make-light)))])
      (lambda (op . args)
        (cond
          [(eq? op 'get)
           (if (or (or (> (car args) 4) (< (car args) 0))
                   (or (> (cadr args) 4) (< (cadr args) 0)))
               (error "Arguments must be between 0 and 4!")
               (vector-ref (vector-ref board (car args)) (cadr args)))]
          [else (error "Must provide valid message!")]))))

;; Exercise A.3
(define (make-game board-maker)
  ;;
  ;; Helper functions:
  ;;
 
  ;; Check if the list of lists has 5 lists and 5 elements per list
  (define (check lst row)
    (if (= (length lst) 5)
        (if (< row 5)
            (if (= (length (list-ref lst row)) 5)
                (check lst (+ row 1))
                #f)
            #t)
        #f))
  
  ;; Initialize the board given a list of lists of on/off values 
  ;; by setting the corresponding lights to those values.
  (define (initialize! board init)
    (define (init-helper row col)
      (cond [(>= row 5) null]
            [(>= col 5) (init-helper (+ row 1) 0)]
            [else
             ((board 'get row col) 'set-state! (list-ref (list-ref init row) col))
             (init-helper row (+ col 1))]))
    (init-helper 0 0))
  
  ;; Connect all orthogonally adjacent lights to each other.
  (define (connect! board)
    (define (con-helper row col)
      (cond [(>= row 5) null]
            [(>= col 5) (con-helper (+ row 1) 0)]
            [else
             (when (>= (- row 1) 0) ((board 'get row col)
                                  'add-neighbor!
                                  (board 'get (- row 1) col)))
             (when (<= (+ row 1) 4) ((board 'get row col)
                                  'add-neighbor!
                                  (board 'get (+ row 1) col)))
             (when (>= (- col 1) 0) ((board 'get row col)
                                  'add-neighbor!
                                  (board 'get row (- col 1))))
             (when (<= (+ col 1) 4) ((board 'get row col)
                                  'add-neighbor!
                                  (board 'get row (+ col 1))))
             (con-helper row (+ col 1))]))
    (con-helper 0 0))
    
  ;; Print the board representation to the terminal.
  (define (print board)
    (define (iter row col)
      (cond ((>= row 5) (newline))
            ((>= col 5) (newline) (iter (+ row 1) 0))
            (else 
              (if (eq? ((board 'get row col) 'get-state) 'on)
                  (display "0")
                  (display "."))
              (iter row (+ col 1)))))
    (iter 0 0))

  (let ([board (board-maker)])
    (connect! board)
    (lambda (op . args)
      (cond
        [(eq? op 'init!)
         (if (check (car args) 0)
             (initialize! board (car args))
             (error "List of lists has invalid number of elements!"))]
        [(eq? op 'peek)
         (if (or (or (> (car args) 4) (< (car args) 0))
                 (or (> (cadr args) 4) (< (cadr args) 0)))
             (error "Arguments must be between 0 and 4!")
             (board 'get (car args) (cadr args)))]
        [(eq? op 'play!)
         (if (or (or (> (car args) 4) (< (car args) 0))
                 (or (> (cadr args) 4) (< (cadr args) 0)))
             (error "Arguments must be between 0 and 4!")
             ((board 'get (car args) (cadr args)) 'update!))]
        [(eq? op 'print)
         (print board)]
        [else (error "Must provide valid message!")]))))

;(define (play-game board-maker init)
;  (define (valid-read? lst)
;    (and (list? lst) 
;         (= (length lst) 2)
;         (let ([row (car lst)]
;               [col (cadr lst)])
;           (and (integer? row) (integer? col)
;                (valid-location? row col)))))
;  
;  (define (run game)
;    (display "Enter move (row col): ")
;    (let ([vals (read)])
;      (cond [(eq? vals 'quit) (void)]
;            [(not (valid-read? vals))
;             (display "Invalid input; please try again!")
;             (newline)
;             (run game)]
;            [else
;             (let ([row (car vals)]
;                   [col (cadr vals)])
;               (game 'play! row col)
;               (newline)
;               (game 'print)
;               (run game))])))
;  
;  (let ([game (make-game board-maker)])
;    (game 'init! init)
;    (newline)
;    (game 'print)
;    (run game)))