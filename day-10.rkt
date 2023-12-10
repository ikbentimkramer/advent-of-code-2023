#lang racket

(require rackunit)
; I found out about this library yesterday; will be useful here
(require math/matrix)

(define in (open-input-file "day-10-input.txt"))
(define in-test (open-input-file "day-10-input-test.txt"))
(define in-cur in)

(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    ; string-trim to remove \r
    [else (parse (cons (string->list (string-trim (read-line in))) acc) in)]))

(define parsetree (parse '() in-cur))
; reverse, because we want the top line in the input file to be row 0
(define matr (list*->matrix (reverse parsetree)))
(define s-pos (cons 25 77 )) ; Full input version
; (define s-pos (cons 2 1)) ; Test input version
(check-equal? (matrix-ref matr (car s-pos) (cdr s-pos)) #\S)

; Direction is a compass-direction: north east south west. Represented
; by the symbols: 'n 'e 's 'w.
; Pipe is a pipe-segment, represented as a character: | - L J 7 F S. S
; is an alias for |.
; Probably better to implement this as a hashtable lookup, but this works.
; direction pipe -> direction
(define (going-to came-from pipe)
  (cond
    [(and (or (eq? #\| pipe) (eq? #\S pipe)) (eq? 'n came-from)) 's]
    [(and (or (eq? #\| pipe) (eq? #\S pipe)) (eq? 's came-from)) 'n]
    [(and (eq? #\- pipe) (eq? 'e came-from)) 'w]
    [(and (eq? #\- pipe) (eq? 'w came-from)) 'e]
    [(and (eq? #\L pipe) (eq? 'n came-from)) 'e]
    [(and (eq? #\L pipe) (eq? 'e came-from)) 'n]
    [(and (eq? #\J pipe) (eq? 'n came-from)) 'w]
    [(and (eq? #\J pipe) (eq? 'w came-from)) 'n]
    [(and (eq? #\7 pipe) (eq? 'w came-from)) 's]
    [(and (eq? #\7 pipe) (eq? 's came-from)) 'w]
    [(and (eq? #\F pipe) (eq? 'e came-from)) 's]
    [(and (eq? #\F pipe) (eq? 's came-from)) 'e]
    [else (error "unknown direction")]))
(check-equal? (going-to 'n #\S) 's)
(check-equal? (going-to 's #\F) 'e)

(define (came-from going-to-dir)
  (cond
    [(eq? going-to-dir 'n) 's]
    [(eq? going-to-dir 'e) 'w]
    [(eq? going-to-dir 's) 'n]
    [(eq? going-to-dir 'w) 'e]))
    

; Find the position of the next node, given our current position and
; direction we're going. Position is a pair of row and column
(define (next-pos cur-pos direction)
  (cond
    [(eq? 'n direction) (cons (- (car cur-pos) 1) (cdr cur-pos))]
    [(eq? 'e direction) (cons (car cur-pos) (add1 (cdr cur-pos)))]
    [(eq? 's direction) (cons (add1 (car cur-pos)) (cdr cur-pos))]
    [(eq? 'w direction) (cons (car cur-pos) (- (cdr cur-pos) 1))]))
(check-equal? (next-pos (cons 2 2) 's) (cons 3 2))

; Helper for reading matrix at position
(define (m-ref-pos matr pos)
  (matrix-ref matr (car pos) (cdr pos)))

; Moving away from the same position, we move in two directions at the
; same time. When we meet again, we found the farthest spot.
(define (part1 pos1 dir1 pos2 dir2 cnt)
  (cond
    ; When we're at the same position and count is not 0, we found the
    ; farthest spot
    [(and (equal? pos1 pos2) (not (eq? cnt 0))) cnt]
    [else (part1
           (next-pos pos1 (going-to dir1 (m-ref-pos matr pos1)))
           (came-from (going-to dir1 (m-ref-pos matr pos1)))
           (next-pos pos2 (going-to dir2 (m-ref-pos matr pos2)))
           (came-from (going-to dir2 (m-ref-pos matr pos2)))
           (add1 cnt))]))

(define part1-solution (part1 s-pos 'n s-pos 's 0))

;; PART 2 ;;

; To find whether a point of the map is inside or outside the loop, we
; can count the number of times it crosses the loop moving eastwards
; (or any direction, really. As long as you check all points in that
; direction). If the number of crosses is odd, the point is inside of
; the loop. If the number is even, the point is outside the loop.

; Find al positions of the loop
(define (find-loop acc pos dir)
  (cond
    ; When we're back at s (so acc is no longer empty) we found all
    ; points
    [(and (eq? (m-ref-pos matr pos) #\S) (not (null? acc))) acc]
    [else (find-loop
           (cons pos acc)
           (next-pos pos (going-to dir (m-ref-pos matr pos)))
           (came-from (going-to dir (m-ref-pos matr pos))))]))

(define loop-positions (list->set (find-loop '() s-pos 's)))
(define loop-crossings
  (list->set
   (filter
    (λ (x)
      (not
       (or
        (eq? (m-ref-pos matr x) #\-)
        ; F and 7 are filtered out, because I assume we always run
        ; along the northern edge of a pipe. So if we encounter F or
        ; 7, we do not cross.
        (eq? (m-ref-pos matr x) #\F)
        (eq? (m-ref-pos matr x) #\7))))
    (set->list loop-positions))))

; Returns true if the pos is on the loop
(define (is-loop? pos)
  (set-member? loop-positions pos))

(define rowlength 140)
(define collength rowlength)
(define (crossings cnt pos)
  (cond
    [(>= (cdr pos) rowlength) cnt]
    [(set-member? loop-crossings pos) (crossings (add1 cnt) (cons (car pos) (add1 (cdr pos))))]
    [else (crossings cnt (cons (car pos) (add1 (cdr pos))))]))

(define (part2 rowpos colpos cnt)
  (cond
    [(>= colpos rowlength) (part2 (add1 rowpos) 0 cnt)]
    [(>= rowpos collength) cnt]
    [(is-loop? (cons rowpos colpos)) (part2 rowpos (add1 colpos) cnt)]
    [(even? (crossings 0 (cons rowpos colpos))) (part2 rowpos (add1 colpos) cnt)]
    [else (part2 rowpos (add1 colpos) (add1 cnt))]))

(define part2-solution (part2 0 0 0))
