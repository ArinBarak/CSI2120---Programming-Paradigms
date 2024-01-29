#lang racket
;Q1: returns the number of all numbers in the range [a, b] that are not divisible by d.
(define (numNotDiv a b d) 
  (if (> a b)
      0
      (if (eqv? 0 (modulo a d))
          (numNotDiv (+ a 1) b d)
          (+ 1 (numNotDiv (+ a 1) b d)))
    )
)

;Q2:  returns a list of all integers in the range [1, n] which are missing from the given list.
(define (missing L x)
  (if (= x 0)
      '()
      (if (member x L)
          (missing L (- x 1))
          (cons x (missing L (- x 1))))))
  

;Q3: this function takes a list of coins and an integer number and returns a list of change.
(define (coin-change L a)
  (if (null? L)
      '()
      (if (< 0 (quotient a (car  L)))
          (cons (quotient a (car L)) (coin-change (cdr L) (remainder a (car L))))
          (cons 0 (coin-change (cdr L) a)))
  )
  )