#lang racket

(require rackunit)

; Input is simple today, so i hand-coded it to a simple file. The file
; provides the variable in, which is a list of pairs of numbers. The
; first number is the time, the second the distance. Also provides
; in-pt2, as the input for part 2.
(require "day-06-input.rkt")

; This require provides in-test. Similar to in (above) but with
; smaller numbers.
(require "day-06-input-test.rkt")

; To solve this puzzle, we find the zeroes to the following equation:
;
;   press_duration * (time - press_duration) - distance = 0
;
; This simplifies to:
;
;   - press_duration ^ 2 + time * press_duration - distance = 0
;
; The formula that solves this is:
;
;   - ( b (+/-) (sqrt D) ) / ( 2 * a )
;
; With a = -1, b = time, c = - distance and:
;
;   D = b ^ 2 - 4 * a * c

(define (D a b c) (- (* b b) (* 4 a c)))
(check-equal? (D 3 2 -1) 16)

; Positive case
(define (solver-p a b c d) (/ (+ (- b) (sqrt d)) (* 2 a)))

; Negative case
(define (solver-n a b c d) (/ (- (- b) (sqrt d)) (* 2 a)))

; Solves the question to the nearest included integer. Returns null if
; no way to win, otherwise a pair of lowerbound upperbound. Upperbound
; is included in the range.
(define (solver a b c)
  (local [(define d (D a b c))]
    (cond
      [(< d 0) '()]
      [(eq? d 0) (cons (ceiling (solver-p a b c d)) (ceiling (solver-p a b c d)))]
      [(> d 0) (cons (ceiling (solver-p a b c d)) (floor (solver-n a b c d)))])))

(define (part1 ls)
  (map
     (λ (row)
       (local [(define res (solver -1 (car row) (- (+ (cdr row) 0.00001))))]
         (cond
           [(null? res) 1]
           [else (- (+ (cdr res) 1) (car res))])))
     ls))

(define solution-part1 (eval (cons '* (part1 in))))

;;  PART 2

; The difficulty with this part is that square roots of really large
; integers tend to be wrong.

(part1 in-pt2)
