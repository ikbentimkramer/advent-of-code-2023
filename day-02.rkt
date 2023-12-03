#lang racket

; A cube is a single colour. We represent these cubes as one of the
; following symbols: red green blue
(define cube '(red green blue)) 

; A bag is a collection of cubes. What cubes are in the bag is
; unknown, but we do have information about how many cubes of each
; colour are at least in the bag. We represent this knowledge as a
; list of three pairs. Each of the pairs has a cube type as first
; element and the number of cubes we know are at least in the bag as
; second element.
(define (make-bag at-least-red at-least-green at-least-blue)
  (list
   (cons 'red at-least-red)
   (cons 'green at-least-green)
   (cons 'blue at-least-blue)))

; A grab is a random pick of cubes in a bag. For now we represent it
; as a bag, but the number in the pair is the actual number of red,
; green or blue cubes in a grab instead of at-least.
(define (make-grab number-of-red number-of-green number-of-blue)
  (make-bag number-of-red number-of-green number-of-blue))

; A game is a collection of grabs from the same bag. The game has an
; identifier, represented as a number. The game is represented as a
; pair, with the first element being the identifier and the second
; element a list of grabs.
(define (make-game identifier grabs)
  (cons identifier grabs))

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
   (make-abbrev 'red "red")
   (make-abbrev 'green "green")
   (make-abbrev 'blue "blue")
   (make-abbrev 'game-start "Game")
   (make-abbrev 'id-sep ":")
   (make-abbrev 'grab-sep ";")
   (make-abbrev 'cube-sep ",")
   (make-abbrev 'cr "\r")
   (make-abbrev 'nl "\n")))

; A regex that matches token-list
(define token-regex
  (regexp (string-join (list "(" (string-join (dict-keys token-list) "|") ")") "")))

; Match token string to symbol
(define (token-match str)
  (dict-ref token-list str))

; The port with the text format
(define in (open-input-file "day-02-input.txt")) 

; function that converts the input file into a symbol list
(define (lexer-rec acc in)
  (cond
    [(eof-object? (peek-char in)) (reverse acc)]
    [else (lexer-rec (cons (token-match (bytes->string/utf-8 (car (regexp-match token-regex in)))) acc) in)]))

(define (lexer in)
  (cond
    [(eof-object? (peek-char in)) '(#f)]
    [else (list (token-match (bytes->string/utf-8 (car (regexp-match token-regex in)))))]))

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
    ; Throw away cube-sep
    [(eq? (caar stack) 'cube-sep) (parse (cdr stack) in)]
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
    ; Convert to cubes
    [(or
      (eq? (caar stack) 'red)
      (eq? (caar stack) 'green)
      (eq? (caar stack) 'blue))
     (parse (cons (list 'cube (caar stack)) (cdr stack)) in)]
    ; Replace \r\n or \n to game-end
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (eq? (caadr stack) 'cr))
     (parse (cons '(game-end) (cddr stack)) in)]
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'nl) (not (eq? (caadr stack) 'cr)))
     (parse (cons '(game-end) (cddr stack)) in)]
    ; Find identifier
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'id-sep) (eq? (caadr stack) 'number))
     (parse (cons (cons 'identifier (cdadr stack)) (cddr stack)) in)]
    ; Combine cube and number
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'cube) (eq? (caadr stack) 'number))
     (parse (cons (cons 'cube-aggr (cons (cadar stack) (cdadr stack))) (cddr stack)) in)]
    ; Create grab
    [(and (not (null? (cdr stack))) (eq? (caar stack) 'cube-aggr) (eq? (caadr stack) 'grab))
     (parse (cons (cons 'grab (cons (cdar stack) (cdadr stack))) (cddr stack)) in)]
    [(eq? (caar stack) 'cube-aggr) (parse (cons (cons 'grab (list (cdar stack))) (cdr stack)) in)]
    ; At game end, create list of grabs
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (not (null? (cdddr stack)))
      (eq? (caar stack) 'game-end)
      (eq? (caadr stack) 'grab)
      (eq? (caaddr stack) 'grab-sep)
      (eq? (car (cadddr stack)) 'grab))
     (parse
      (cons
       '(game-end)
       (cons
        (cons
         'grab-list
         (list
          (cdr (cadddr stack))
          (cdadr stack)))
        (cdr (cdddr stack))))
      in)]
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (not (null? (cdddr stack)))
      (eq? (caar stack) 'game-end)
      (eq? (caadr stack) 'grab-list)
      (eq? (caaddr stack) 'grab-sep)
      (eq? (car (cadddr stack)) 'grab))
     (parse
      (cons
       '(game-end)
       (cons
        (cons
         'grab-list
         (cons
          (cdr (cadddr stack))
          (cdadr stack)))
        (cdr (cdddr stack))))
      in)]
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (eq? (caar stack) 'game-end)
      (eq? (caadr stack) 'grab)
      (eq? (caaddr stack) 'identifier))
     (parse
      (cons
       '(game-end)
       (cons
        (cons 'grab-list (list (cdadr stack)))
        (cddr stack)))
      in)]
    ; Create game
    [(and
      (not (null? (cdr stack)))
      (not (null? (cddr stack)))
      (not (null? (cdddr stack)))
      (eq? (caar stack) 'game-end)
      (eq? (caadr stack) 'grab-list)
      (eq? (caaddr stack) 'identifier)
      (eq? (car (cadddr stack)) 'game-start))
     (parse
      (cons
       (cons
        'game
        (cons
         (cdaddr stack)
         (cdadr stack)))
       (cddddr stack))
      in)]
    ; Put the next item on the stack
    [else (parse (cons (lexer in) stack) in)]))

; Convert the parse tree to a list of games
(define (make-game-list parse-tree)
  (map
   (λ (game)
     (make-game
      (string->number (cadr game))
      (map
       (λ (grab)
         (make-grab
          (if (dict-has-key? grab 'red) (string->number (dict-ref grab 'red)) 0)
          (if (dict-has-key? grab 'green) (string->number (dict-ref grab 'green)) 0)
          (if (dict-has-key? grab 'blue) (string->number (dict-ref grab 'blue)) 0)))
       (cddr game))))
   parse-tree))

; Combine a grab with a bag
(define (combine-bag bag grab)
  (make-bag
   (max (dict-ref bag 'red) (dict-ref grab 'red))
   (max (dict-ref bag 'green) (dict-ref grab 'green))
   (max (dict-ref bag 'blue) (dict-ref grab 'blue))))

; Part 1 specific bag filter
(define (part1-filter bag)
  (and
   (<= (dict-ref bag 'red) 12)
   (<= (dict-ref bag 'green) 13)
   (<= (dict-ref bag 'blue) 14)))

; and helper
(define (and-2 a b)
  (and a b))

; Part 1 game filter
(define (part1-game-filter game)
  (part1-filter (cdr game)))

; Find game bags
(define (find-game-bags game-list)
  (map
   (λ (game)
     (make-game
      (car game)
      (foldl
       combine-bag
       (make-bag 0 0 0)
       (cdr game))))
   game-list))

(define bags
 (find-game-bags (make-game-list (parse '() in))))

(define part1-filtered-bags
  (filter part1-game-filter bags))

; Day 2 part 1 result
(eval (cons '+ (map car part1-filtered-bags)))

; Day 2 part 2 result
(eval
 (cons
  '+
  (map
   (λ (game)
     (*
      (dict-ref (cdr game) 'red)
      (dict-ref (cdr game) 'green)
      (dict-ref (cdr game) 'blue)))
   bags)))
