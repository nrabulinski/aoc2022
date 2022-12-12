#lang racket
(require racket/string)

(define move-pairs (list (cons 'rock 'scissors) (cons 'paper 'rock) (cons 'scissors 'paper)))

(define (find-winning move)
  (compose1 (curry eq? move) car))

(define (find-losing move)
  (compose1 (curry eq? move) cdr))

(define (cmp-move a b)
  (if (eq? a b)
      'draw
      (match (findf (find-winning a) move-pairs)
        [(cons _ other)
         #:when (eq? b other)
         'win]
        [_ 'lose])))

; Complement opponent's move according to *our* outcome
(define (complement-move move outcome)
  (match outcome
    ['win (car (findf (find-losing move) move-pairs))]
    ['lose (cdr (findf (find-winning move) move-pairs))]
    ['draw move]))

(define (parse-move char)
  (match char
    [(or "A" "X") 'rock]
    [(or "B" "Y") 'paper]
    [(or "C" "Z") 'scissors]))

(define (parse-outcome char)
  (match char
    ["X" 'lose]
    ["Y" 'draw]
    ["Z" 'win]))

(define (move-points move)
  (match move
    ['rock 1]
    ['paper 2]
    ['scissors 3]))

(define (get-points our outcome)
  (+ (move-points our)
     (match outcome
       ['win 6]
       ['draw 3]
       ['lose 0])))

(define (calculate-points-naive line)
  (match-let ([(list their our) (map parse-move (string-split line))])
    (get-points our (cmp-move our their))))

(define (calculate-points line)
  (match-let* ([(list their our) (string-split line)]
               [their (parse-move their)]
               [outcome (parse-outcome our)]
               [our (complement-move their outcome)])
    (get-points our outcome)))

(define (go)
  (let ([f (file->lines "assets/day2")])
    (display "Part 1: ")
    (display (foldl + 0 (map calculate-points-naive f)))
    (display "\n")
    (display "Part 2: ")
    (display (foldl + 0 (map calculate-points f)))
    (display "\n")))

(go)
