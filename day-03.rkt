#lang racket

(require rackunit)

; A matrix is represented as a pair a list of numbers and a
; vector. The list of numbers gives the length of each dimension. The
; vector contains the data. The length of the second vector is the
; product of the numbers in the list.
(define (make-matrix dims data)
  (cons dims data))

; Calculate the offset multipliers for each dimension in d
(define (dims-calc-offset-multipliers acc d)
  (cond
    [(null? acc) (dims-calc-offset-multipliers (cons 1 acc) d)]
    [(null? (cdr d)) (reverse acc)]
    [else (dims-calc-offset-multipliers (cons (eval (cons '* (cons (car d) acc))) acc) (cdr d))]))

(check-equal? (dims-calc-offset-multipliers '() '(55)) '(1))
(check-equal? (dims-calc-offset-multipliers '() '(2 5)) '(1 2))
(check-equal? (dims-calc-offset-multipliers '() '(2 5 4)) '(1 2 10))

; Select the value from m at position s, using 1-indexes (no
; 0-indexing due to maths).
(define (matrix-ref m s)
  ; check that s is a valid selector
  (if
   (eval
    (cons
     'and
     (map
      (λ (dim selected) (>= dim selected))
      (car m)
      s)))
   (vector-ref
    (cdr m)
    (eval
     (cons
      '+
      (map
       (λ (selected offmult)
         (* (- selected 1) offmult))
       s
       (dims-calc-offset-multipliers '() (car m))))))
   (error "selector is out of range")))

(check-equal? (matrix-ref (make-matrix '(2 2) #(A B C D)) '(1 2)) 'C)

; Helper for matrix-rev-ref. Acc accumulates to a
; selector. rev-offmult is the reversed list of offset multiples and
; pos is the position to calculate the selector for.
(define (rec-rev-ref acc rev-offmult pos)
  (cond
    [(null? rev-offmult) acc]
    [else (rec-rev-ref
           (cons (+ 1 (quotient pos (car rev-offmult))) acc)
           (cdr rev-offmult)
           (remainder pos (car rev-offmult)))]))

(check-equal? (rec-rev-ref '() '(2 1) 3) '(2 2))
(check-equal? (rec-rev-ref '() '(2 1) 4) '(1 3))
(check-equal? (rec-rev-ref '() '(2 1) 5) '(2 3))

; Find the selector that refers to pos in the data vector of m.
(define (matrix-rev-ref m pos)
  ; check for pos being smaller than length data vector
  (if
   (< pos (vector-length (cdr m)))
   (rec-rev-ref '() (reverse (dims-calc-offset-multipliers '() (car m))) pos)
   (error "pos is too large")))

(check-equal? (matrix-rev-ref (make-matrix '(2 2) #(A B C D)) 2) '(1 2))

; find the numbers in the input string. Returns a list of pairs, of
; which the car of the pair is a pair giving the location, and the cdr
; is the actual number.
(define (find-numbers inp regex)
(map
 (λ (range)
   (cons
    range
    (substring inp (car range) (cdr range))))
 (regexp-match-positions* regex inp)))

; finds the selectors for all locations in ll. Returns a list of
; pairs, with selectors as the car of the pair and location as the cdr
; of the pair.
(define (find-selectors m acc pos ll)
  (cond
    [(null? ll) acc]
    [(> (caar ll) pos) (find-selectors m acc (caar ll) ll)]
    [(< pos (cdar ll)) (find-selectors m (cons (cons (matrix-rev-ref m pos) (car ll)) acc) (+ 1 pos) ll)]
    [else (find-selectors m acc pos (cdr ll))]))

; find the surrounding selectors of a 2d selector
(define (2d-neighbours s)
  (list
   (list (- (car s) 1) (- (cadr s) 1))
   (list (car s) (- (cadr s) 1))
   (list (+ (car s) 1) (- (cadr s) 1))
   (list (- (car s) 1) (cadr s))
   (list (+ (car s) 1) (cadr s))
   (list (- (car s) 1) (+ (cadr s) 1))
   (list (car s) (+ (cadr s) 1))
   (list (+ (car s) 1) (+ (cadr s) 1))))

(define (find-matches numbers acc selectors)
  (cond
    [(null? selectors) acc]
    [(dict-has-key? numbers (car selectors))
     (find-matches numbers (cons (dict-ref numbers (car selectors)) acc) (cdr selectors))]
    [else (find-matches numbers acc (cdr selectors))]))

(define in (open-input-file "day-03-input.txt"))
(define in-string (regexp-replace* "\r?\n" (port->string in) ""))
(define in-matrix (cons (list 140 140) (list->vector (string->list in-string))))
(define in-symbols (find-numbers in-string "[^0-9.]"))
(define in-numbers (find-numbers in-string "[0-9]+"))
(define number-selectors (find-selectors in-matrix '() 0 (map car in-numbers)))
(define symbol-selectors
  (append* (map 2d-neighbours (map car (find-selectors in-matrix '() 0 (map car in-symbols))))))

; Part 1 answer
(eval (cons '+ (map string->number (find-matches in-numbers '() (remove-duplicates (find-matches number-selectors '() symbol-selectors))))))

; Part 2
(define in-gears (find-numbers in-string "[*]"))

(eval
 (cons
  '+
  (map
   (λ (z)
     (eval (cons '* (map string->number (find-matches in-numbers '() z)))))
   (filter
    (λ (y) (eq? (length y) 2))
    (map
     (λ (x)
       (remove-duplicates
        (find-matches
         number-selectors
         '()
         (2d-neighbours
          (caar
           (find-selectors in-matrix '() 0 (list(car x))))))))
     in-gears)))))
