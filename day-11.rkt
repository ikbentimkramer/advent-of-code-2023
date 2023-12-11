#lang racket

; So for this puzzle (part1) we need to do a couple of things:
;
;  1.  Find the empty rows and columns
;  2.  Find the locations of the galaxies
;  3.  Pair each galaxy with another galaxy (only once!)
;  4.  Find out how many empty rows and columns are between each galaxy pair
;  5.  Find the distance between the galaxy pairs and add the number of
;      empty rows and columns.

; To help us accomplish this, we record the sequential position of
; each galaxy while parsing. This can be divided by the row length to
; get the row number. The remainder will be the column number.
(define in-test (open-input-file "day-11-input-test.txt"))
(define in (open-input-file "day-11-input.txt"))

(define (parse cnt acc in)
  (cond
    [(eof-object? (peek-char in)) (reverse acc)]
    [else (local [(define char (read-char in))]
            (cond
              [(eq? char #\#) (parse (add1 cnt) (cons cnt acc) in)]
              [(eq? char #\.) (parse (add1 cnt) (cons 0 acc) in)]
              [else (parse cnt acc in)]))]))

(define parselist (parse 0 '() in))

(define rowlength 140)
(define collength (/ (length parselist) rowlength))

(define (rev-pair p)
  (cons (cdr p) (car p)))

(define galaxy-locations
  (map
   (λ (y) (rev-pair ; quotient is the column, we want that second, not first
           (call-with-values
           (λ () (quotient/remainder y rowlength))
           cons)))
     (filter (λ (x) (not (eq? x 0))) parselist)))

; Find combinations of galaxies
(define (combine-galaxies acc g1 g2)
  (cond
    [(null? g1) acc]
    [(null? g2) (combine-galaxies acc (cdr g1) (cdr g1))]
    [(equal? (car g1) (car g2)) (combine-galaxies acc g1 (cdr g2))]
    [else (combine-galaxies (cons (cons (car g1) (car g2)) acc) g1 (cdr g2))]))

(define galaxy-combis (combine-galaxies '() galaxy-locations galaxy-locations))

; To figure out which rows are empty, we can do matrix multiplication
; with a row-matrix and a column-matrix containing only 1's. The
; result will tell us if the column or row is empty when the value is
; 0 for that row/column.

(require math/matrix)

(define matr
  (list->matrix rowlength collength parselist))

(define empty-cols
  (list->set
   (indexes-of
    (matrix->list (matrix* (->row-matrix (make-list rowlength 1)) matr))
    0)))

(define empty-rows
  (list->set
   (indexes-of
    (matrix->list (matrix* matr (->col-matrix (make-list collength 1))))
    0)))

; gc are galaxy-combinations. factor is how many rows an empty row actually is
(define (solve factor acc gc)
  (cond
    [(null? gc) acc]
    [else
     (solve
      factor
      (cons
       (+
        (abs (- (cadar gc) (caaar gc))) ; distance over x
        (abs (- (cddar gc) (cdaar gc))) ; distance over y
        (*
         (- factor 1) ; extra empty-col distance
         (set-count
          (set-intersect
           (list->set (inclusive-range (min (cadar gc) (caaar gc)) (max (cadar gc) (caaar gc))))
           empty-cols)))
        (*
         (- factor 1) ; extra empty-row distance
         (set-count
          (set-intersect
           (list->set (inclusive-range (min (cddar gc) (cdaar gc)) (max (cddar gc) (cdaar gc))))
           empty-rows))))
       acc)
      (cdr gc))]))

(define part1-solution (apply + (solve 2 '() galaxy-combis)))

; Part 2 - Basically the same thing, add a factor

(define part2-solution (apply + (solve 1000000 '() galaxy-combis)))
