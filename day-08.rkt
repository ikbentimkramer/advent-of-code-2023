#lang racket

(require rackunit)
(require racket/generator)

(define in-test (open-input-file "day-08-input-test.txt"))
(define in (open-input-file "day-08-input.txt"))

; To make switching between in-test and in easier
(define cur-in in)

; A position-based parse today. Mostly because I still have to figure
; out how to match characters on something other than eq?
(define directions (string-trim (read-line cur-in))) ; trin necessary for carriage returns
(define newline (read-line cur-in))
(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    [else
     (local [(define l (read-line in))]
       (parse
        (cons
         (cons
          (substring l 0 3)
          (cons
           (substring l 7 10)
           (substring l 12 15)))
         acc)
        in))]))
(define parsetree (parse '() cur-in))

; The input is in a sense two networks: a left and a right
; network. The following functions extract the edge for the left and
; for the right network.
(define (make-left-edge item)
  (cons (car item) (cadr item)))
(check-equal? (make-left-edge (cons "ZZZ" (cons "AAA" "BBB"))) (cons "ZZZ" "AAA"))

(define (make-right-edge item)
  (cons (car item) (cddr item)))
(check-equal? (make-right-edge (cons "ZZZ" (cons "AAA" "BBB"))) (cons "ZZZ" "BBB"))

(define left (make-hash (map make-left-edge parsetree)))
(define right (make-hash (map make-right-edge parsetree)))

(define direction-gen
  (sequence->repeated-generator (string->list directions)))

; cnt is a number representing the number of steps. loc is the current location
(define (part1 dir cnt loc)
  (cond
    [(string=? loc "ZZZ") cnt]
    [(eq? dir #\L) (part1 (direction-gen) (add1 cnt) (hash-ref left loc))]
    [(eq? dir #\R) (part1 (direction-gen) (add1 cnt) (hash-ref right loc))]))

(define part1-solution (part1 (direction-gen) 0 "AAA"))

;  PART 2  ;

; a lookup table to see what the last letter of a node is
(define last-letter (make-hash (map (λ (x) (cons x (substring x 2 3))) (hash-keys left))))

; The nodes ending with an A
(define a-end (filter (λ (x) (string=? (hash-ref last-letter x) "A"))  (hash-keys last-letter)))

(define direction-gen-pt2
  (sequence->repeated-generator (string->list directions)))

; in part 2, loc is a list. Turns out to be quite slow
(define (part2 gen dir cnt loc)
  (cond
    [(null? (filter (λ (lc) (not (string=? (hash-ref last-letter lc) "Z"))) loc)) cnt]
    [(eq? dir #\L) (part2 gen (gen) (add1 cnt) (map (λ (lc) (hash-ref left lc)) loc))]
    [(eq? dir #\R) (part2 gen (gen) (add1 cnt) (map (λ (lc) (hash-ref right lc)) loc))]))

; Running (part2 (direction-gen-pt2) 0 (list x)) is terribly slow

; Running them individually is not!
(define pt2-runlengths (map
 (λ (x)
   (local [(define gen (sequence->repeated-generator (string->list directions)))]
     (part2 gen (gen) 0 (list x))))
 a-end))

; They all go round fully
(eq? (apply + (map (λ (x) (remainder x (string-length directions))) pt2-runlengths)) 0)

; So the solution may be the product of all quotients, times the
; number of steps. The product of quotients it the number of full
; passes, so the multiplication with the number of steps is necessary.
(apply * (cons
          (string-length directions)
          (map
           (λ (x) (quotient x (string-length directions)))
           pt2-runlengths)))

