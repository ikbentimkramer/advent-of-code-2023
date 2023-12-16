#lang racket

(require rackunit)

; Let's load the data
(define in (open-input-file "day-12-input.txt"))
(define (parse acc in)
  (cond
    [(eof-object? (peek-char in)) acc]
    [else (parse
           (cons
            (string-split (string-trim (read-line in)) " ")
            acc)
           in)]))

(define in-split
  (map
   (λ (x)
     (cons
      (car x)
      (map string->number (string-split (cadr x) ","))))
   (parse '() in)))

(define (runcounts cnt acc str)
  (cond
    [(string? str) (runcounts cnt acc (string->list str))]
    [(and (or (null? str) (eq? (car str) #\?)) (> cnt 0)) (reverse (cons cnt acc))]
    [(or (null? str) (eq? (car str) #\?)) (reverse acc)]
    [(eq? (car str) #\#) (runcounts (add1 cnt) acc (cdr str))]
    [(and
      (not (eq? (car str) #\#))
      (eq? cnt 0)) (runcounts cnt acc (cdr str))]
    [else (runcounts 0 (cons cnt acc) (cdr str))]))
(check-equal? (runcounts 0 '() "###..##.....#.") '(3 2 1))
(check-equal? (runcounts 0 '() "#.#.###") '(1 1 3))
(check-equal? (runcounts 0 '() ".##?#.") '(2))

(define (good-dir? str rc)
  (local [(define r (runcounts 0 '() str))]
    (if (> (length r) (length rc)) #f
    (andmap (λ (x y) (<= x y)) (append r (make-list (- (length rc) (length r)) 0)) rc))))
(check-equal? (good-dir? "####....." '(3 2 1)) #f)
(check-equal? (good-dir? "###.#...." '(3 2 1)) #t)
; (check-equal? (good-dir? "##.##..." '(3 2 1)) #f) ; Returns #t. This is why good-dir is not good enough

(define (fa bt? rc cnt acc)
  (cond
    [(null? acc) cnt]
    [(and bt? (not (caar acc))) (fa bt? rc cnt (cdr acc))]
    [(and bt? (caar acc)) (fa #f rc cnt (cons (cons #f (cdar acc)) (cdr acc)))]
    [(not (good-dir? (cdar acc) rc)) (fa #t rc cnt (cdr acc))]
    [(and (string-contains? (cdar acc) "?") (caar acc))
     (fa bt? rc cnt (cons (cons #t (string-replace (cdar acc) "?" "#" #:all? #f)) acc))]
    [(and (string-contains? (cdar acc) "?") (not (caar acc)))
     (fa bt? rc cnt (cons (cons #t (string-replace (cdar acc) "?" "." #:all? #f)) acc))]
    [(and (not (string-contains? (cdar acc) "?"))
          (equal? (runcounts 0 '() (cdar acc)) rc))
     (fa #t rc (add1 cnt) (cdr acc))]
    [(and (not (string-contains? (cdar acc) "?"))
          (not (equal? (runcounts 0 '() (cdar acc)) rc)))
     (fa #t rc cnt (cdr acc))]
    ))
(check-equal? (fa #f '(1 1 3) 0 (list (cons #t ".??..??...?##."))) 4)

;(define part1-solution (apply + (map (λ (x) (fa #f (cdr x) 0 (list (cons #t (car x))))) in-split)))

;; PART 2 ;;

(define in-split-pt2
  (map
   (λ (x)
     (cons
      (string-join (make-list 5 (car x)) "?")
      (flatten (make-list 5 (cdr x)))))
   in-split))

; So unfortunately this runs for too long. I was kind of proud of the
; good-dir? optimization, but it's not good enough.
; (define part2-solution (apply + (map (λ (x) (fa #f (cdr x) 0 (list (cons #t (car x))))) in-split-pt2)))

; Because the goal for me this AoC is to learn, and I am pretty
; stumped, I went to the internet for help. The 'solution' is to use a
; recursive function with memoization, but I doubt my solver has
; memoizable inputs.

; I'd like to find a different solution. I could try using a stack
; with all valid, partially filled strings I found up to that
; point. Find the next "?", replace with both # and ., check if valid,
; then add to new stack if valid. If invalid, I do not need to check
; the following branches either.

; Below is attempt #3. This attempt tries compiling the second part of
; the input (the list with all runs of broken springs) to a state
; machine. This is implemented as a function that takes a state and an
; input, and returns a list of next states. We iterate over the
; inputs, calculating all states per input. At the end, we count the
; number of states to get our answer.

; Inspiration for this attempt was found in a piece on
; nondeterministic finite automata for regular expressions. Link:
; https://swtch.com/~rsc/regexp/regexp1.html

; There are four possible inputs: . # ? $. $ is the end of the line. #
; are broken springs, . are working springs, ? are both broken and
; working springs.

; There are three types of state:
;
;   (T1)  A state that accepts 0 and stays in the same state, or accepts
;         broken spring (#) or end-of-line ($) to transition
;   (T2)  A state that accepts one and only one of the following: broken
;         spring (#), working spring (.) or end of line ($)
;   (T3)  The accept state (no transitions possible)
;
; To note a repetition of one or more of the same state, we write a +
; after the state name. To indicate which character T2 accepts, we
; append the character it accepts. So if we want to write down a
; repetition of one or more broken springs we write T2#+. If we want
; to use a + to indicate a repetition of a pattern, we can use
; parentheses to indicate the pattern. For example (T1 T2.)+ matches
; T1 T2. T1 T2. T1 T2.

; Every list of broken spring runs compiles to the following type structure:
;
; (T1# T2#+ T2.)+ T1# T2#+ T1$ T3
;
; The number of repetitions of the parenthesised pattern is equal to
; the number of runs in the list of broken springs minus one. The
; number of repetitions inside each pattern is equal to the length of
; the corresponding run. The last number of runs repetition is outside
; the parenthesised pattern, because we need to match 0 or more
; working springs before transitioning to T3.

; make a T3 transition function
(define (make-t3)
  (λ (char) '()))
(check-equal? ((make-t3) #\#) '())

; make a T2 transition function
(define (make-t2 char-to-accept next-state)
  (λ (char)
    (if
     (or
      (and (not (eq? #\$ char-to-accept)) (eq? char #\?))
      (eq? char char-to-accept)) (list next-state) '())))
(check-equal? ((make-t2 #\# 2) #\#) '(2))
(check-equal? ((make-t2 #\# 2) #\?) '(2))
(check-equal? ((make-t2 #\$ 2) #\?) '())
(check-equal? ((make-t2 #\# 2) #\.) '())
(check-equal? ((make-t2 #\. 2) #\.) '(2))
(check-equal? ((make-t2 #\. 2) #\?) '(2))

; make a T1 transition function
(define (make-t1 char-to-accept current-state next-state)
  (λ (char)
    (cond
      [(and
        (not (eq? #\$ char-to-accept))
        (eq? char #\?)) (list current-state next-state)]
      [(or (eq? char #\.) (eq? char #\?)) (list current-state)]
      [(eq? char char-to-accept) (list next-state)]
      [else '()])))
(check-equal? ((make-t1 #\# 1 2) #\.) '(1))
(check-equal? ((make-t1 #\# 1 2) #\#) '(2))
(check-equal? ((make-t1 #\# 1 2) #\?) '(1 2))
(check-equal? ((make-t1 #\$ 1 2) #\?) '(1))
(check-equal? ((make-t1 #\$ 1 2) #\$) '(2))
(check-equal? ((make-t1 #\# 1 2) #\$) '())

; Build a list of pairs with the state left and the transition
; right. Start counting states at offset. Last state will be (+ offset
; runlength 1), transitioning to (+ offset runlength 2).
(define (build-run offset runlength)
  (append*
   (list (cons offset (make-t1 #\# offset (add1 offset))))
   (build-list (sub1 runlength) (λ (x) (cons (+ offset x 1) (make-t2 #\# (+ offset x 2)))))
   (list (cons (+ offset runlength ) (make-t2 #\. (+ offset runlength 1))))
   '()))

; Same as build-run, but for the end sequence
(define (build-end-run offset runlength)
  (append*
   (list (cons offset (make-t1 #\# offset (add1 offset))))
   (build-list (sub1 runlength) (λ (x) (cons (+ offset x 1) (make-t2 #\# (+ offset x 2)))))
   (list (cons (+ offset runlength) (make-t1 #\$ (+ offset runlength) (+ offset runlength 1))))
   (list (cons (+ offset runlength 1) (make-t3)))
   '()))

; Build the state transition table based on a list of broken spring runs
(define (make-transition-table acc run-list)
  (cond
    [(null? run-list) (make-hash acc)]
    [(null? (cdr run-list))
     (make-transition-table (append (build-end-run (length acc) (car run-list)) acc) (cdr run-list))]
    [else (make-transition-table (append (build-run (length acc) (car run-list)) acc) (cdr run-list))]))

(define in-split-compiled
  (map
   (λ (x)
     (cons
      (string->list (string-append (car x) "$"))
      (make-transition-table '() (cdr x))))
   in-split-pt2))

(define (run-proc table in x)
  (local [(define res ((hash-ref table x) (car in)))]
    (cond
      [(null? res) '()]
      [else (map (λ (y) (cons x y)) res)])))

(define (make-acc-hash new-acc old-acc mapping)
  (cond
    [(null? mapping) new-acc]
    [(not (hash-has-key? new-acc (cdar mapping)))
     (make-acc-hash
      (hash-set new-acc (cdar mapping) (hash-ref old-acc (caar mapping)))
      old-acc
      (cdr mapping))]
    [else
     (make-acc-hash
      (hash-update new-acc (cdar mapping) (λ (x) (+ (hash-ref old-acc (caar mapping)) x)))
      old-acc
      (cdr mapping))]))

; acc is a hash with state as key and count as value. This should solve the OOM-issues.
(define (run-state-machine table acc in)
  (cond
    [(null? in) (car (hash-values acc))] ; We can do this because only the final state survives
    [else
     (run-state-machine
      table
      (make-acc-hash (hash) acc (apply append (map (λ (x) (run-proc table in x)) (hash-keys acc))))
      (cdr in))]))

; It works! 
(time
 (apply
  +
  (map
   (λ (x)
     (run-state-machine (cdr x) (hash 0 1) (car x)))
   in-split-compiled)))

