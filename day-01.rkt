#lang racket

(require rackunit)

; a regex that matches numbers.
(define regex-num (regexp "[0-9]"))

; a regex that matches written numbers
(define regex-written-num (regexp "(oneight|twone|threeight|fiveight|eightwo|eightree|nineight|one|two|three|four|five|six|seven|eight|nine)"))

; maps str to a number
(define (written-num-map str)
  (cond
    [(string=? "oneight" str) 18]
    [(string=? "twone" str) 21]
    [(string=? "threeight" str) 38]
    [(string=? "fiveight" str) 58]
    [(string=? "eightwo" str) 82]
    [(string=? "eighthree" str) 83]
    [(string=? "nineight" str) 98]
    [(string=? "one" str) 1]
    [(string=? "two" str) 2]
    [(string=? "three" str) 3]
    [(string=? "four" str) 4]
    [(string=? "five" str) 5]
    [(string=? "six" str) 6]
    [(string=? "seven" str) 7]
    [(string=? "eight" str) 8]
    [(string=? "nine" str) 9]
    [else (error "not a written number")]))
; testing
(check-equal? (written-num-map "six") 6)
(check-exn exn:fail? (thunk (written-num-map "abs")))

; replace written numbers with actual numbers in str
(define (written-num->num str)
  (regexp-replace* regex-written-num str (λ (all fst) (number->string (written-num-map fst)))))
; testing
(check-equal? (written-num->num "bbcone") "bbc1")
(check-equal? (written-num->num "threesixnine") "369")
(check-equal? (written-num->num "6twone") "621")

; finds all numbers in str. '() when no numbers found
(define (find-nums str) (regexp-match* regex-num str))
; testing
(check-equal? (find-nums "a2bcdeE") '("2"))
(check-equal? (find-nums "25dsW3v5A") '("2" "5" "3" "5"))
(check-equal? (find-nums "a") '())

; find the first and last item in lst
(define (find-first-and-last lst) (list (first lst) (last lst)))
;; testing
(check-equal? (find-first-and-last '(1 3 5)) '(1 5))

; find the first and last number of str, join them and represent as a number
(define (decode-part1 str) (string->number (string-join (find-first-and-last (find-nums str)) "")))
; testing
(check-equal? (decode-part1 "1abc2") 12)
(check-equal? (decode-part1 "a1b2c3d4e5f") 15)
(check-equal? (decode-part1 "treb7uchet") 77)

; same as decode-part1, but replace written-nums with numbers first
(define (decode-part2 str)
  (string->number (string-join (find-first-and-last (find-nums (written-num->num str))) "")))
; testing
(check-equal? (decode-part2 "1abc2") 12)
(check-equal? (decode-part2 "a1b2c3d4e5f") 15)
(check-equal? (decode-part2 "treb7uchet") 77)
(check-equal? (decode-part2 "two1nine") 29)
(check-equal? (decode-part2 "xtwone3four") 24)
(check-equal? (decode-part2 "zoneight234") 14)
(check-equal? (decode-part2 "2zoneight") 28)

; find the total sum of decoded numbers in the input file
(define (rec-find-sum decoder acc inp)
  (cond
    [(eof-object? (peek-char inp)) acc]
    [else (rec-find-sum decoder (+ acc (decoder (read-line inp))) inp)]))
; wrappers
(define (find-sum-part1 inp) (rec-find-sum decode-part1 0 inp))
(define (find-sum-part2 inp) (rec-find-sum decode-part2 0 inp))

; in is a port that represents the input file
(define in (open-input-file "day-01-input.txt"))

; Solution part 1
(find-sum-part1 in)

; Reset in
(file-position in 0)

; Solution part 2
(find-sum-part2 in)
