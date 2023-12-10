#lang racket

; Test input today is kind of disappointing, because it does not have
; the same length as the actual input sequences.

(define in (open-input-file "day-09-input.txt"))

(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    [(parse
      (cons
       (map (λ (x) (string->number (string-trim x))) (string-split (read-line in) " "))
       acc)
      in)]))

(define parsetree (parse '() in))

; lagrange basis, given that x = 0, 1, ... k + 1 is the number of nodes
(define (lagrange-basis-simple k j x)
  (foldl * 1 (map (λ (m) (/ (- x m) (- j m))) (remove j (range 0 (+ k 1))))))

(define (lagrange-function k x ylist)
(apply + (map (λ (y j) (* y (lagrange-basis-simple k j x))) ylist (range 0 (+ k 1)))))

; Part 1 answer
(apply + (map (λ (ylist) (lagrange-function 20 21 ylist)) parsetree))

; Part 2 answer
(apply + (map (λ (ylist) (lagrange-function 20 -1 ylist)) parsetree))
