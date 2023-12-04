#lang racket

; NOTE ;
; ---- ;
; This version was written after the first one, after perusing the
; internet about this challenge. The part 2 bit can be implemented way
; more efficiently, by keeping a stack. The parser is kept the
; same. It may not be the most efficient, but it works and it's mine.

; A card is represented as a pair, consisting of a number and a pair
; of sets of numbers. The number is the identifier of the card. The
; first set of the pair of sets is the winning set. The second pair is
; the found set.
(define (make-card id win found)
  (cons id (cons win found)))

; An abbrev is a pair of a symbol and a string to match. The string is
; the key, so this comes first in the representation.
(define (make-abbrev symbol str)
  (cons str symbol))

; The token list defines how to translate the data format to symbols
(define token-list
  (list
   (make-abbrev 'number-sep " ")
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
   (make-abbrev 'game-start "Card")
   (make-abbrev 'id-sep ":")
   (make-abbrev 'winset-sep (regexp-quote "|"))
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

(define (parse stack in)
  (cond
    ; If the stack is empty, put something on there
    [(null? stack) (parse (cons (lexer in) stack) in)]
    ; If the top value is '(#f) we reached eof and return the parse
    [(false? (caar stack)) (cdr stack)]
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
     (parse (cons (cons 'number (number->string (caar stack))) (cdr stack)) in)]
    ; Combine sequential numbers to number
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'number) (eq? (caadr stack) 'number))
     (parse (cons (cons 'number (string-append (cdadr stack) (cdar stack))) (cddr stack)) in)]
    ; Replace \r\n or \n to game-end
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (eq? (caadr stack) 'cr))
     (parse (cons '(game-end) (cddr stack)) in)]
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (not (eq? (caadr stack) 'cr)))
     (parse (cons '(game-end) (cddr stack)) in)]
    ; remove superfluous number-seps
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'number-sep) (not (eq? (caadr stack) 'number)))
     (parse (cdr stack) in)]
    ; Find identifier
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'id-sep) (eq? (caadr stack) 'number))
     (parse (cons (cons 'identifier (cdadr stack)) (cddr stack)) in)]
    ; Create winset
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (eq? (caar stack) 'winset-sep)
      (eq? (caadr stack) 'number-sep)
      (eq? (caaddr stack) 'number))
     (parse (cons (cons 'winset (list (cdaddr stack))) (cdddr stack)) in)]
    ; Add to winset
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (eq? (caar stack) 'winset)
      (eq? (caadr stack) 'number-sep)
      (eq? (caaddr stack) 'number))
     (parse (cons (cons 'winset (cons (cdaddr stack) (cdar stack))) (cdddr stack)) in)]
    ; Create foundset
    [(and
      (not (null? (cdr stack)))
      (eq? (caar stack) 'game-end)
      (eq? (caadr stack) 'number))
     (parse (cons (cons 'foundset (list (cdadr stack))) (cddr stack)) in)]
    ; Add to foundset
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (eq? (caar stack) 'foundset)
      (eq? (caadr stack) 'number-sep)
      (eq? (caaddr stack) 'number))
     (parse (cons (cons 'foundset (cons (cdaddr stack) (cdar stack))) (cdddr stack)) in)]
    ; Create game
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (not (null? (cdddr stack)))
      (eq? (caar stack) 'foundset)
      (eq? (caadr stack) 'winset)
      (eq? (caaddr stack) 'identifier)
      (eq? (car (cadddr stack)) 'game-start))
     (parse (cons (cons 'game (cons (cdaddr stack) (cons (cdadr stack) (cdar stack)))) (cddddr stack)) in)]
    ; Put the next item on the stack
    [else (parse (cons (lexer in) stack) in)]))

(define in-test (open-input-file "day-04-input-test.txt"))
(define in (open-input-file "day-04-input.txt"))
(define parsetree (parse '() in))
(define card-list
  (map
   (λ (game)
     (make-card
      (string->number (cadr game))
      (list->set (map string->number (caddr game)))
      (list->set (map string->number (cdddr game)))))
   parsetree))

; Part 1 solution
(eval
 (cons
  '+
  (map
   (λ (card)
     (if
      (eq? (set-count (set-intersect (cadr card) (cddr card))) 0)
      0
      (expt 2 (- (set-count (set-intersect (cadr card) (cddr card))) 1))))
   card-list)))

; Part 2 solution

; Find the number of wins for each card
(define (find-wins cards)
  (map (λ (card) (cons (car card) (set-count (set-intersect (cadr card) (cddr card))))) cards))

; Store wins in a list, sorted by id, descending
(define win-list (sort (find-wins card-list) (λ (x y) (< (car x) (car y)))))

; Add mult to the top n items on the stack
(define (increment-top-n stack acc mult n)
  (cond
    [(eq? n 0) (append (reverse acc) stack)]
    [(null? stack) (increment-top-n stack (cons mult acc) mult (- n 1))]
    [else (increment-top-n (cdr stack) (cons (+ (car stack) mult) acc) mult (- n 1))]))

; Loop through each cards and add the number of new cards to the stack
(define (process-cards acc stack cl)
  (cond
    [(null? cl) acc]
    [(null? stack) (process-cards (+ acc 1) (increment-top-n stack '() 1 (cdar cl)) (cdr cl))]
    [else (process-cards
           (+ acc (car stack) 1)
           (increment-top-n (cdr stack) '() (+ ( car stack) 1) (cdar cl))
           (cdr cl))]))
