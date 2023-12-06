#lang racket

(require rackunit)

; An abbrev is a pair of a symbol and a string to match. The string is
; the key, so this comes first in the representation.
(define (make-abbrev symbol str)
  (cons str symbol))

; The token list defines how to translate the data format to symbols
(define token-list
  (list
   (make-abbrev '0 "0")
   (make-abbrev '1 "1")
   (make-abbrev '2 "2")
   (make-abbrev '3 "3")
   (make-abbrev '4 "4")
   (make-abbrev '5 "5")
   (make-abbrev '6 "6")
   (make-abbrev '7 "7")
   (make-abbrev '8 "8")
   (make-abbrev '9 "9")
   (make-abbrev 'seeds "seeds: ")
   (make-abbrev 'seed-to-soil "seed-to-soil map:")
   (make-abbrev 'soil-to-fertilizer "soil-to-fertilizer map:")
   (make-abbrev 'fertilizer-to-water "fertilizer-to-water map:")
   (make-abbrev 'water-to-light "water-to-light map:")
   (make-abbrev 'light-to-temperature "light-to-temperature map:")
   (make-abbrev 'temperature-to-humidity "temperature-to-humidity map:")
   (make-abbrev 'humidity-to-location "humidity-to-location map:")
   (make-abbrev 'space " ")
   (make-abbrev 'cr "\r")
   (make-abbrev 'nl "\n")))

; A regex that matches token-list
(define token-regex
  (regexp (string-join (list "(" (string-join (dict-keys token-list) "|") ")") "")))

; Match token string to symbol
(define (token-match str)
  (dict-ref token-list str))

; function that converts the input file into a symbol list
(define (lexer in)
  (cond
    [(eof-object? (peek-char in)) '(#f)]
    [else (list (token-match (regexp-quote (bytes->string/utf-8 (car (regexp-match token-regex in))))))]))

(define (lexer-peek in)
  (cond
    [(eof-object? (peek-char in)) '(#f)]
    [else (list (token-match (bytes->string/utf-8 (car (regexp-match-peek token-regex in)))))]))

(define (base-parse stack in)
  (cond
    ; Convert to numbers
    [(or
      (eq? (caar stack) '0)
      (eq? (caar stack) '1)
      (eq? (caar stack) '2)
      (eq? (caar stack) '3)
      (eq? (caar stack) '4)
      (eq? (caar stack) '5)
      (eq? (caar stack) '6)
      (eq? (caar stack) '7)
      (eq? (caar stack) '8)
      (eq? (caar stack) '9))
     (base-parse (cons (cons 'number (number->string (caar stack))) (cdr stack)) in)]
    ; Combine sequential numbers to number
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'number) (eq? (caadr stack) 'number))
     (base-parse (cons (cons 'number (string-append (cdadr stack) (cdar stack))) (cddr stack)) in)]
    ; Replace \r\n or \n to new-line
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (eq? (caadr stack) 'cr))
     (base-parse (cons '(new-line) (cddr stack)) in)]
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (not (eq? (caadr stack) 'cr)))
     (base-parse (cons '(new-line) (cddr stack)) in)]
    ; Replace double newlines with double-nl
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'new-line) (eq? (caadr stack) 'new-line))
     (base-parse (cons '(double-nl) (cddr stack)) in)]
    ; Convert to maps
    [(or
      (eq? (caar stack) 'seed-to-soil)
      (eq? (caar stack) 'soil-to-fertilizer)
      (eq? (caar stack) 'fertilizer-to-water)
      (eq? (caar stack) 'water-to-light)
      (eq? (caar stack) 'light-to-temperature)
      (eq? (caar stack) 'temperature-to-humidity)
      (eq? (caar stack) 'humidity-to-location))
     (base-parse (cons (cons 'map (cons (caar stack) '())) (cdr stack)) in)]
    ; Remove newline after map
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'new-line) (eq? (caadr stack) 'map))
     (base-parse (cdr stack) in)]
    ; return the stack
    [else stack]))

(define (seeds-parse stack in)
  (cond
    ; Add to stack as long as it's a number, new-line, double-nl, space or null
    [(or
      (null? stack)
      (eq? (caar stack) 'number)
      (eq? (caar stack) 'space)
      (eq? (caar stack) 'cr)
      (eq? (caar stack) 'new-line))
     (seeds-parse (base-parse (cons (lexer in) stack) in) in)]
    ; Remove seeds
    [(eq? (caar stack) 'seeds) (seeds-parse (cdr stack) in)]
    ; Replace double-nl with seeds-list
    [(eq? (caar stack) 'double-nl)
     (seeds-parse (cons (cons 'seeds-list '()) (cdr stack)) in)]
    ; Remove space after seeds-list
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'seeds-list) (eq? (caadr stack) 'space))
     (seeds-parse (cons (car stack) (cddr stack)) in)]
    ; Add number to seeds list
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'seeds-list) (eq? (caadr stack) 'number))
     (seeds-parse (cons (cons 'seeds-list (cons (cdadr stack) (cdar stack))) (cddr stack)) in)]
    [else stack]))

(define (map-parse stack in)
  (cond
    ; If the top value is '(#f) we reached eof and return the stack
    ; this check is necessary because we collapse the newlines for maps
    [(false? (caar stack)) (cdr stack)]
    ; Add more to stack in these cases
    [(or
      (eq? (caar stack) 'map)
      (eq? (caar stack) 'cr)
      (eq? (caar stack) 'number)
      (eq? (caar stack) 'space))
     (map-parse (base-parse (cons (lexer in) stack) in) in)]
    ; Create a map-vals
    [(and (eq? (caar stack) 'new-line) (not (or (eq? (lexer-peek in) 'cr) (eq? (lexer-peek in) 'nl))))
     (map-parse (cons '(map-vals) (cdr stack)) in)]
    ; Remove space after map-vals
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'map-vals) (eq? (caadr stack) 'space))
     (map-parse (cons (car stack) (cddr stack)) in)]
    ; Add numbers
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'map-vals) (eq? (caadr stack) 'number))
     (map-parse (cons (cons 'map-vals (cons (cdadr stack) (cdar stack))) (cddr stack)) in)]
    ; Create a map-list
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'map-vals) (eq? (caadr stack) 'map))
     (map-parse (cons (cons 'map (cons (cadadr stack) (cons (car stack) (cddadr stack)))) (cddr stack)) in)]
    [(and (eq? (caar stack) 'new-line) (or (eq? (lexer-peek in) 'cr) (eq? (lexer-peek in) 'nl)))
     (map-parse (cons '(map-list) (cdr stack)) in)]
    [else stack]))
    
(define (parse stack in)
  (cond
    ; If the stack is empty, put something on there
    [(null? stack) (parse (base-parse (cons (lexer in) stack) in) in)]
    ; If the top value is '(#f) we reached eof and return the stack
    [(false? (caar stack)) (cdr stack)]
    [(eq? (caar stack) 'seeds) (parse (seeds-parse stack in) in)]
    [(eq? (caar stack) 'map) (map-parse stack in)]
    [else (parse (base-parse (cons (lexer in) stack) in) in)]))

(define in-test (open-input-file "day-05-input-test.txt"))
(define in (open-input-file "day-05-input.txt"))
(define parsetree (reverse (parse '() in))) ; seeds-list on top

; Extract the seed numbers as list
(define (make-seeds-list l)
  (map string->number (cdr l)))
(define seeds-list
  (make-seeds-list (car parsetree)))

; Extract the map-vals as list
(define (make-map-vals l)
  (make-seeds-list l))

; Make maps
(define (make-maps l)
  (cons
   (car l)
   (map make-map-vals (cdr l))))
(define maps (map make-maps (map cdr (cdr parsetree))))

(define (find-map id)
  (dict-ref maps id))

; Calculate next value from v and map-vals mvs
(define (next-value v default mvs)
  (cond
    [(null? mvs) default]
    [(or (< (- v (cadar mvs)) 0) (>= v (+ (cadar mvs) (caddar mvs)))) (next-value v default (cdr mvs))]
    [else (+ (- v (cadar mvs)) (caar mvs))]))

; Go through all steps
(define (part1 seeds map-ids)
  (cond
    [(null? map-ids) seeds]
    [else
     (part1
      (map (λ (seed) (next-value seed seed (find-map (car map-ids)))) seeds)
      (cdr map-ids))]))

(define order-of-maps
  (list
   'seed-to-soil
   'soil-to-fertilizer
   'fertilizer-to-water
   'water-to-light
   'light-to-temperature
   'temperature-to-humidity
   'humidity-to-location))

(define solution-part-1 (eval (cons 'min (part1 seeds-list order-of-maps))))

;; PART 2 ;;

; Takes a seed list and transforms it into a seed range list. acc is
; an empty list as start input.
(define (make-seed-range-list acc l)
  (cond
    [(null? l) acc]
    [else (make-seed-range-list (cons (cons (car l) (+ (car l) (cadr l))) acc) (cddr l))]))

; Find lower bound of map-value
(define (mv-lb mv)
  (cadr mv))

; Find upper bound of map-value
(define (mv-ub mv)
  (+ (cadr mv) (caddr mv)))

; Find mapped value of mv
(define (mv-ref mv v)
  (cond
    [(or (< v (mv-lb mv)) (> v (mv-ub mv))) (error "Value out of range")]
    [else (+ (- v (mv-lb mv)) (car mv))]))


; Finds the range that matches and does not match. pair of two lists,
; first is the ranges that do not match second is range that does
; match.
(define (find-matching-range r mv)
  (cond
    ; Out of bounds
    [(or (<= (cdr r) (mv-lb mv)) (>= (car r) (mv-ub mv))) (cons (list r) '())]
    ; r starting before start, ending before end
    [(and (< (car r) (mv-lb mv)) (<= (cdr r) (mv-ub mv)) (> (cdr r) (mv-lb mv)))
     (cons (list (cons (car r) (mv-lb mv))) (list (cons (mv-ref mv (mv-lb mv)) (mv-ref mv (cdr r)))))]
    ; r starting before start, ending after end
    [(and (< (car r) (mv-lb mv)) (> (cdr r) (mv-lb mv)))
     (cons
      (list (cons (car r) (mv-lb mv)) (cons (mv-ub mv) (cdr r)))
      (list (cons (mv-ref mv (mv-lb mv)) (mv-ref mv (mv-ub mv)))))]
    ; r starting after start, ending before end
    [(and (>= (car r) (mv-lb mv)) (<= (cdr r) (mv-ub mv)))
     (cons '() (list (cons (mv-ref mv (car r)) (mv-ref mv (cdr r)))))]
    ; r starting after start, ending after end
    [(and (>= (car r) (mv-lb mv)) (> (cdr r) (mv-ub mv)) (< (car r) (mv-ub mv)))
     (cons (list (cons (mv-ub mv) (cdr r))) (list (cons (mv-ref mv (car r)) (mv-ref mv (mv-ub mv)))))]
    [else (error "Bounds check went wrong")]))

(check-equal? (find-matching-range (cons 1 4) (list 3 4 5)) (cons (list (cons 1 4)) '()))
(check-equal? (find-matching-range (cons 9 11) (list 3 4 5)) (cons (list (cons 9 11)) '()))
(check-equal? (find-matching-range (cons 20 110) (list 3 4 5)) (cons (list (cons 20 110)) '()))
(check-equal? (find-matching-range (cons 1 4) (list 3 2 5)) (cons (list (cons 1 2)) (list (cons 3 5))))
(check-equal? (find-matching-range (cons 2 4) (list 3 2 5)) (cons '() (list (cons 3 5))))
(check-equal? (find-matching-range (cons 2 7) (list 3 2 5)) (cons '() (list (cons 3 8))))
(check-equal? (find-matching-range (cons 1 5) (list 3 2 2)) (cons (list (cons 1 2) (cons 4 5)) (list (cons 3 5))))
(check-equal? (find-matching-range (cons 3 7) (list 3 2 2)) (cons (list (cons 4 7)) (list (cons 4 5))))


(define srl (make-seed-range-list '() seeds-list))

; acc is result stack, cur is current range, stack is a stack of
; ranges to process, mv is a list of map-values to process, dmv is the default list of match-values
(define (rec-find-matches dmv acc cur stack mv)
  (cond
    [(and (null? stack) (null? cur)) acc]
    [(and (null? stack) (null? mv)) (cons cur acc)]
    [(null? cur) (rec-find-matches dmv acc (car stack) (cdr stack) dmv)]
    [(null? mv) (rec-find-matches dmv (cons cur acc) (car stack) (cdr stack) dmv)]
    [else
     (local [(define m (find-matching-range cur (car mv)))]
       (cond
         ; Case: no range returned, matching range. Reset mv.
         [(null? (car m)) (rec-find-matches dmv (append (cdr m) acc) '() stack dmv)]
         ; Case: two ranges returned, always one matching range
         [(not (null? (cdar m)))
          (rec-find-matches dmv (cons (cadr m) acc) (caar m) (cons (cadar m) stack) (cdr mv))]
         ; Case: one range returned, maybe a matching range
         [(not (null? (car m)))
          (rec-find-matches dmv (append (cdr m) acc) (caar m) stack (cdr mv))]))]))


(define (part2 ranges map-ids)
  (cond
    [(null? map-ids) ranges]
    [else (part2
           (rec-find-matches (find-map (car map-ids)) '() '() ranges (find-map (car map-ids)))
           (cdr map-ids))]))

; Part 2 solution
(eval (cons 'min (map car (part2 srl order-of-maps))))
