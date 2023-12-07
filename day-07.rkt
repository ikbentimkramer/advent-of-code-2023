#lang racket

(require rackunit)

; Inputs
(define in (open-input-file "day-07-input.txt"))
(define in-test (open-input-file "day-07-input-test.txt"))

; The file structure is quite simple, so a simple parse function today
(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    [else (local [(define line (read-line in))]
            (parse
             (cons
              (cons
               (substring line 0 5)
               (string->number (string-trim (substring line 6 (string-length line)))))
              acc)
             in))]))

; the result of our parse is a list of pairs. The first item in the
; pair is a string representing a hand, the second is the number
; representing a bid.
(define parsetree (parse '() in))

; A hand is a pair of a rank and a list of cards. The rank is
; represented by a number. -1 means the rank is unknown. The list of
; cards is a list of five symbols: |2| |3| |4| |5| |6| |7| |8| |9| T J Q K A
(define (make-hand str)
  (cons -1 (map (λ (x) (string->symbol (string-trim (string x) "|"))) (string->list str))))
(check-equal?  (make-hand "A2T4A") (cons -1 '(A |2| T |4| A)))

; Convert a card to a two-space string, representing its sort order with a leading zero
(define (card-sort-order card)
  (cond
    [(eq? card '|2|) "02"]
    [(eq? card '|3|) "03"]
    [(eq? card '|4|) "04"]
    [(eq? card '|5|) "05"]
    [(eq? card '|6|) "06"]
    [(eq? card '|7|) "07"]
    [(eq? card '|8|) "08"]
    [(eq? card '|9|) "09"]
    [(eq? card 'T) "10"]
    [(eq? card 'J) "11"]
    [(eq? card 'Q) "12"]
    [(eq? card 'K) "13"]
    [(eq? card 'A) "14"]))

; convert a hand to a number, giving its sort order regardless of type
(define (hand-sort-order hand)
  (string->number (string-append* "" (map card-sort-order (cdr hand)))))

(check-equal? (hand-sort-order (make-hand "23AT8")) 203141008)

; convert a list of items to a summary, giving the counts of each
; item. A count is represented as a hash, with the key being a
; item and value the count.
(define (summary acc cl)
  (cond
    [(null? cl) acc]
    [(hash-has-key? acc (car cl)) (summary (hash-update acc (car cl) add1) (cdr cl))]
    [else (summary (hash-set acc (car cl) 1) (cdr cl))]))

; wrapper around cardlist-summary to get the summaries for a hand
(define (hand-summary hand)
  (summary (hash) (cdr hand)))
(check-equal? (hand-summary (make-hand "AAAAA")) (hash 'A 5))
(check-equal? (hand-summary (make-hand "AAA22")) (hash 'A 3 '|2| 2))

; Counts how many of each count are in the hand summary
(define (hand-summary-summary hs)
  (summary (hash) (map cdr (hash->list hs))))
(check-equal? (hand-summary-summary (hash 'A 3 '|2| 2)) (hash 2 1 3 1))

; determine type of the hand-summary-summary (hss)
(define (hss-type hss)
  (cond
    [(hash-has-key? hss 5) 'five-of-a-kind]
    [(hash-has-key? hss 4) 'four-of-a-kind]
    [(and (hash-has-key? hss 3) (hash-has-key? hss 2)) 'full-house]
    [(hash-has-key? hss 3) 'three-of-a-kind]
    [(and (hash-has-key? hss 2) (eq? (hash-ref hss 2) 2)) 'two-pair]
    [(and (hash-has-key? hss 2) (eq? (hash-ref hss 2) 1)) 'one-pair]
    [(and (hash-has-key? hss 1) (eq? (hash-ref hss 1) 5)) 'high-card]
    [else (error "no hss-type")]))
(check-equal? (hss-type (hash 2 2)) 'two-pair)

; determine type order
(define (type-sort-order type)
  (cond
    [(eq? type 'five-of-a-kind) 7]
    [(eq? type 'four-of-a-kind) 6]
    [(eq? type 'full-house) 5]
    [(eq? type 'three-of-a-kind) 4]
    [(eq? type 'two-pair) 3]
    [(eq? type 'one-pair) 2]
    [(eq? type 'high-card) 1]
    [else (error "not a type or no sort order available")]))

; determine hand type sort order
(define (hand-type-sort-order hand)
  (type-sort-order
   (hss-type
    (hand-summary-summary
     (hand-summary hand)))))
(check-equal? (hand-type-sort-order (make-hand "AT223")) 2)

; determine which hand is less than the other
(define (hand-less-than lh rh)
  (cond
    [(eq? (hand-type-sort-order lh) (hand-type-sort-order rh))
     (< (hand-sort-order lh) (hand-sort-order rh))]
    [else (< (hand-type-sort-order lh) (hand-type-sort-order rh))]))
(check-equal? (hand-less-than (make-hand "AAAJJ") (make-hand "44443")) #t)

(define part1
  (foldl
   (λ (y x)
     (cons (add1 (car x)) (+ (* (car x) (cdr y)) (cdr x))))
   (cons 1 0)
   (sort parsetree (λ (l r) (hand-less-than (make-hand (car l)) (make-hand (car r)))))))

; PART 2

; get joker count from hand summary
(define (make-joker hs)
  (cond
    [(not (hash-has-key? hs 'J)) 0]
    [else (hash-ref hs 'J)]))
(check-equal? (make-joker (hash 'K 3 'J 2)) 2)
(check-equal? (make-joker (hash 'A 5)) 0)

; remove joker count from hand summary
(define (hand-summary-remove-joker hs)
  (hash-remove hs 'J))

; alter hand-summary-summary based on joker count. If we get a joker,
; we always increment the max key with the number of jokers and set
; the value to 1. Two pair is a special case, which results in a full
; house if there is a single joker.
(define (hss-add-joker hss j)
  (cond
    [(eq? j 0) hss]
    [(and (eq? j 1) (hash-has-key? hss 2) (eq? (hash-ref hss 2) 2)) (hash 3 1 2 1)]
    [else (hash (+ (foldl max 0 (hash-keys hss)) j) 1)]))
(check-equal? (hss-add-joker (hash 1 5) 0) (hash 1 5))
(check-equal? (hss-add-joker (hash 1 4) 1) (hash 2 1))
(check-equal? (hss-add-joker (hash 2 2) 1) (hash 3 1 2 1))

; sort order with jokers
(define (card-sort-order-j card)
  (cond
    [(eq? card '|2|) "02"]
    [(eq? card '|3|) "03"]
    [(eq? card '|4|) "04"]
    [(eq? card '|5|) "05"]
    [(eq? card '|6|) "06"]
    [(eq? card '|7|) "07"]
    [(eq? card '|8|) "08"]
    [(eq? card '|9|) "09"]
    [(eq? card 'T) "10"]
    [(eq? card 'J) "01"] ; changed
    [(eq? card 'Q) "12"]
    [(eq? card 'K) "13"]
    [(eq? card 'A) "14"]))

; hand sort order with jokers
(define (hand-sort-order-j hand)
  (string->number (string-append* "" (map card-sort-order-j (cdr hand)))))

; Alter the hand type sort order
(define (hand-type-sort-order-j hand)
  (type-sort-order
   (hss-type
    (hss-add-joker
     (hand-summary-summary
      (hand-summary-remove-joker (hand-summary hand)))
     (make-joker (hand-summary hand))))))
(check-equal? (hand-type-sort-order-j (make-hand "AAJQQ")) 5)

; joker version
(define (hand-less-than-j lh rh)
  (cond
    [(eq? (hand-type-sort-order-j lh) (hand-type-sort-order-j rh))
     (< (hand-sort-order-j lh) (hand-sort-order-j rh))]
    [else (< (hand-type-sort-order-j lh) (hand-type-sort-order-j rh))]))

(define part2
  (foldl
   (λ (y x)
     (cons (add1 (car x)) (+ (* (car x) (cdr y)) (cdr x))))
   (cons 1 0)
   (sort parsetree (λ (l r) (hand-less-than-j (make-hand (car l)) (make-hand (car r)))))))
part2
