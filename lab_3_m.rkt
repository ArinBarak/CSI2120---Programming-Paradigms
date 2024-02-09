#lang racket
(define (min-element L)
  (if (null? L)
      2000000000 
      (letrec ((mini
                (lambda (L minx)
                  (cond ((null? L) minx)
                        ((< (car L) minx) (mini (cdr L) (car L)))
                        (#t (mini (cdr L) minx))))))
        (mini L (car L))))
  )

(define (palindrome str)
  
  )