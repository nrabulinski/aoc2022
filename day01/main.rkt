#lang racket

(define (collect val acc)
  (if (non-empty-string? val)
      (let ([head (first acc)] [tail (rest acc)]) (list* (+ head (string->number val)) tail))
      (list* 0 acc)))

(define (go)
  (let* ([f (file->lines "assets/day1")] [cals (foldl collect '(0) f)])
    (display "Part 1: ")
    (display (argmax identity cals))
    (display "\n")
    (display "Part 2: ")
    (display (foldl + 0 (take (sort cals >) 3)))
    (display "\n")))

(go)
