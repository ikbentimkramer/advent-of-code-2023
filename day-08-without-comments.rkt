#lang racket

(require racket/generator)

(define in (open-input-file "day-08-input.txt"))

(define directions (string-trim (read-line in)))
(define newline (read-line in))
(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    [else
     (local [(define l (read-line in))]
       (parse
        (cons
         (cons (substring l 0 3)
               (cons (substring l 7 10) (substring l 12 15)))
         acc)
        in))]))
(define parsetree (parse '() in))

(define (make-left-edge item)
  (cons (car item) (cadr item)))
(define (make-right-edge item)
  (cons (car item) (cddr item)))

(define left (make-hash (map make-left-edge parsetree)))
(define right (make-hash (map make-right-edge parsetree)))

(define direction-gen
  (sequence->repeated-generator (string->list directions)))

(define (part1 dir cnt loc)
  (cond
    [(string=? loc "ZZZ") cnt]
    [(eq? dir #\L) (part1 (direction-gen) (add1 cnt) (hash-ref left loc))]
    [(eq? dir #\R) (part1 (direction-gen) (add1 cnt) (hash-ref right loc))]))

(define part1-solution (part1 (direction-gen) 0 "AAA"))

(define last-letter (make-hash (map (λ (x) (cons x (substring x 2 3))) (hash-keys left))))

(define a-end (filter (λ (x) (string=? (hash-ref last-letter x) "A"))  (hash-keys last-letter)))

(define (part2 gen dir cnt loc)
  (cond
    [(null? (filter (λ (lc) (not (string=? (hash-ref last-letter lc) "Z"))) loc)) cnt]
    [(eq? dir #\L) (part2 gen (gen) (add1 cnt) (map (λ (lc) (hash-ref left lc)) loc))]
    [(eq? dir #\R) (part2 gen (gen) (add1 cnt) (map (λ (lc) (hash-ref right lc)) loc))]))

(define pt2-runlengths (map
 (λ (x)
   (local [(define gen (sequence->repeated-generator (string->list directions)))]
     (part2 gen (gen) 0 (list x))))
 a-end))

(define part2-solution (apply * (cons
          (string-length directions)
          (map
           (λ (x) (quotient x (string-length directions)))
           pt2-runlengths))))
