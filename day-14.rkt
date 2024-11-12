#lang racket

; square rock has a position and a number of round rocks that have
; moved against it.
;
; position pos is a pair of integers.
; number of round rocks round-rock-count is an integer of at least 0.
(struct square-rock (pos round-rock-count))
(define (new-square-rock pos)
  (square-rock pos 0))

; calculate the score from a square-rock. len is the length of the satellite dish
(define (calc-score sr len)
  (*
   (/ (square-rock-round-rock-count sr) 2)
   (+ (sub1 (- len (cdr (square-rock-pos sr))))
      (- (- len (cdr (square-rock-pos sr))) (square-rock-round-rock-count sr)))))

;;; file parser
(define (square-rock-add1 sr)
  (square-rock (square-rock-pos sr) (add1 (square-rock-round-rock-count sr))))

; only necessary during init, when we do not know the width of the matrix
(define (add-empty x y acc)
  (cond
    [(list? acc) (cons (list (square-rock (cons x (sub1 y)) 0)) acc)]
    [else acc]))

(define (add-round-rock x y acc)
  (cond
    [(list? acc) (cons (list (square-rock (cons x (sub1 y)) 1)) acc)]
    [else (hash-set acc x (cons (square-rock-add1 (car (hash-ref acc x))) (cdr (hash-ref acc x))))]))

(define (add-square-rock x y acc)
  (cond
    [(list? acc) (cons (list (new-square-rock (cons x y))) acc)]
    [else (hash-set acc x (cons (new-square-rock (cons x y)) (hash-ref acc x)))]))

(define (end-init init? acc)
  (cond
    [init? (make-immutable-hash (map cons (range (length acc)) (reverse acc)))]
    [else acc]))

(define (parse init? x y acc in)
  (local [(define cur (read-char in))]
    (cond
      [(eof-object? cur) (cons y acc)]
      [(char=? #\return cur) (parse init? x y acc in)]
      [(char=? #\newline cur) (parse #f 0 (add1 y) (end-init init? acc) in)]
      [(char=? #\. cur) (parse init? (add1 x) y (add-empty x y acc) in)]
      [(char=? #\O cur) (parse init? (add1 x) y (add-round-rock x y acc) in)]
      [(char=? #\# cur) (parse init? (add1 x) y (add-square-rock x y acc) in)]
      [else
       (error
        (string-append
         "Parse error: expected input at line: " (number->string y)
         ", char: " (number->string x)))])))
      
(define in (open-input-file "day-14-input.txt"))
(define parseres (parse #t 0 0 '() in))
(define len (car parseres)) ; No need to add 1, because file ends with a newline
(define parsetree (cdr parseres))
(define square-rocks (append* (map cdr (hash->list parsetree))))
(define part-1 (apply + (map (λ (x) (calc-score x len)) square-rocks)))

;;; PART 2 ;;;

;;; First, parse file and find positions of both round and square rocks

(define in-pt2 (open-input-file "day-14-input-test.txt"))

; bounding-box
;
; Parameters
; ----------
; x0 the leftmost coordinate (integer)
; x1 the rightmost coordinate (integer)
; y0 the topmost coordinate (integer)
; y1 the bottommost coordinate (integer)
(struct bounding-box (x0 y0 x1 y1))

; parse-pt2  -  parser for part 2
;
; Parameters
; ----------
; in  input port
; bb  bounding box
; rr  list of round rocks
; sr  list of square rocks
;
; Returns
; -------
; A list with the first element being the bounding box, the second the
; list of round rocks and the third the list of square rocks.
(define (parse-pt2 in bb rr sr) '(bb rr sr))

(define parseres-pt2 (parse-pt2 in-pt2 (bounding-box 0 0 0 0) '() '()))

;;; Second, find the bounding box

(define bb (first parseres-pt2))
(define rr (second parseres-pt2))
(define sr (third parseres-pt2))

;;; Third, place square rocks all around the border of the bounding box.
;;; this is so we can always find a square rock for a round rock to roll
;;; up against.

; make-border  -  generate a square-rock border around a bounding box
;
; Parameters
; ----------
; bb  bounding box
;
; Returns
; -------
; A list of square rocks that enclose the bounding box.
(define (make-border bb) '())

;;; Fourth, for every (cons round-rock, direction): find coordinates of
;;; closest square rock in direction

; sr-key
;
; Parameters
; ----------
; key  pair of a symbol ('vert 'horiz) and a column (integer)
; search  instructs which direction to search in to find the
;         closest square rock. Is a pair of ('greater 'less) and
;         an integer to use as base.
(struct sr-key (key search))

; find-sr-key  -  translate values into key for square-rock lookup
;
; Parameters
; ----------
; dir  symbol representing direction ('north 'east 'south 'west)
; coords  pair of xy coordinates of the round rock in question
;
; Returns
; -------
; sr-key struct
(define (find-sr-key dir coords) (sr-key (cons 'vert 0) (cons 'greater 0)))

;;; Fifth, add a round-rock to the square-rock at found coordinates

;;; Sixth, for each (cons square-rock, direction): place back all
;;; round-rocks.

;;; Seventh, check if current output signifies a cycle. If cycle is
;;; found, decrease the counter to the remainder of the counter divided
;;; by the cycle length.

;;; Finally, score the state the dish is in 
