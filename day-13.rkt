#lang racket

; inputs
(define in-test (open-input-file "day-13-input.txt"))

; Each lava field has an identifier (number), rowlength (number),
; collength (number) and poslist (list of pairs, row first,
; column second).
(struct lava-field (id rowlength collength poslist))

; Helper to skip chars
(define (skip-char in)
  (local [(define c (read-char in))]
    in))

; Parse a single row
(define (parse-row id rl cl acc res-acc in)
  (local [(define c (read-char in))]
    (cond
      [(eq? c #\.) (parse-row id (add1 rl) cl acc res-acc in)]
      [(eq? c #\#) (parse-row id (add1 rl) cl (cons (cons rl cl) acc) res-acc in)]
      [(and (eq? c #\return) (eq? (peek-char in) #\newline))
       (parse id rl (add1 cl) acc res-acc (skip-char in))]
      [else (error "Unexpected character in parse-row")])))

; Parse an input document
(define (parse id rl cl acc res-acc in)
  (cond
    [(> id 100) (error "infinite loop?")]
    [(eof-object? (peek-char in)) (cons (lava-field id rl cl acc) res-acc)]
    [(or (eq? (peek-char in) #\.) (eq? (peek-char in) #\#))
     (parse-row id 0 cl acc res-acc in)]
    [(eq? (peek-char in) #\return)
     (parse id rl cl acc res-acc (skip-char in))]
    [(eq? (peek-char in) #\newline)
     (parse (add1 id) 0 0 '() (cons (lava-field id rl cl acc) res-acc) (skip-char in))]
    [else (error "Unexpected character in parse")]))

; Store the parse in parsetree
(define parsetree (parse 0 0 0 '() '() in-test))

; subtract two positions from eachother
(define (subtract-pos left right)
  (cons (- (car left) (car right)) (- (cdr left) (cdr right))))

(define (add-pos left right)
    (cons (+ (car left) (car right)) (+ (cdr left) (cdr right))))

; flip a position on the horizontal axis
(define (flip-horizontal pos)
  (cons (car pos) (* (cdr pos) -1)))

; flip a position on the vertical axis
(define (flip-vertical pos)
  (cons (* (car pos) -1) (cdr pos)))

; convert a pair of numbers to exact numbers
(define (inexact-pair->exact-pair pair)
  (cons
   (inexact->exact (car pair))
   (inexact->exact (cdr pair))))

; transpose a pair
(define (pair-transpose p)
  (cons (cdr p) (car p)))

; transpose a lava-field
(define (lava-field-transpose lf)
  (lava-field
   (lava-field-id lf)
   (lava-field-collength lf)
   (lava-field-rowlength lf)
   (map pair-transpose (lava-field-poslist lf))))

; generate a list of lines to mirror over
(define (make-lines-from-lava-field lf)
  (map (λ (x) (cons x 0)) (range 0.5 (sub1 (lava-field-rowlength lf)))))

; generate a list of linenumbers with which we calculate the required sum
(define (make-line-numbers-from-lava-field lf)
    (range 1 (lava-field-rowlength lf)))

; check if reflection happens
(define (reflects? line current-lava-field)
  (local
      [(define lfp (lava-field-poslist current-lava-field))
       (define lfp-upper (lava-field-rowlength current-lava-field))]
    (subset?
     (filter
      (λ (y) (and (>= (car y) 0) (< (car y) lfp-upper)))
      (map
       (λ (x)
         (inexact-pair->exact-pair
          (add-pos
           (flip-vertical
            (subtract-pos x line))
           line)))
       lfp))
     lfp)))

; vertical reflections
(define verticals
  (flatten
   (map
    (λ (lf)
      (foldl
       (λ (x y acc) (cond [x (cons y acc)] [else acc]))
       '()
       (map
        (λ (line) (reflects? line lf))
        (make-lines-from-lava-field lf))
       (make-line-numbers-from-lava-field lf)))
    parsetree)))

; horizontal reflections. Includes score multiplication.
(define horizontals
  (flatten
   (map
    (λ (lf)
      (foldl
       (λ (x y acc) (cond [x (cons (* 100 y) acc)] [else acc]))
       '()
       (map
        (λ (line) (reflects? line lf))
        (make-lines-from-lava-field lf))
       (make-line-numbers-from-lava-field lf)))
    (map lava-field-transpose parsetree))))

(define part-1-solution (+ (apply + verticals) (apply + horizontals)))

; check if all items in sub, minus one, is in s
(define (subset-minus1? sub s)
  (local [(define hs (list->set s))]
    (eq?
     1
     (apply
      +
      (map
       (λ (x)
         (cond
           [(set-member? hs x) 0]
           [else 1]))
       sub)))))

(define (part-2-reflects? line current-lava-field)
  (local
      [(define lfp (lava-field-poslist current-lava-field))
       (define lfp-upper (lava-field-rowlength current-lava-field))]
    (subset-minus1?
     (filter
      (λ (y) (and (>= (car y) 0) (< (car y) lfp-upper)))
      (map
       (λ (x)
         (inexact-pair->exact-pair
          (add-pos
           (flip-vertical
            (subtract-pos x line))
           line)))
       lfp))
     lfp)))

; vertical reflections
(define part-2-verticals
  (flatten
   (map
    (λ (lf)
      (foldl
       (λ (x y acc) (cond [x (cons y acc)] [else acc]))
       '()
       (map
        (λ (line) (part-2-reflects? line lf))
        (make-lines-from-lava-field lf))
       (make-line-numbers-from-lava-field lf)))
    parsetree)))

; horizontal reflections. Includes score multiplication.
(define part-2-horizontals
  (flatten
   (map
    (λ (lf)
      (foldl
       (λ (x y acc) (cond [x (cons (* 100 y) acc)] [else acc]))
       '()
       (map
        (λ (line) (part-2-reflects? line lf))
        (make-lines-from-lava-field lf))
       (make-line-numbers-from-lava-field lf)))
    (map lava-field-transpose parsetree))))

(define part-2-solution (+ (apply + part-2-verticals) (apply + part-2-horizontals)))
