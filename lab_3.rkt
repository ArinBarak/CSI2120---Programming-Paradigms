#lang racket
;Q1: returns the first non-repeated number.
(define (first-unique L)
(letrec ((helper-first
          (lambda (L)
            (cond ((null? L) 0)
                  ((member (car L) (cdr L)) (helper-first (cdr L)))
                  (#t (car L))))))
  (helper-first L ))
  )


;Q2: reverses the given string.
(define (reverse-string str)
  (let ((len (string-length str)))
    (let loop ((i (- len 1)) (res ""))
      (if (< i 0)
          res
          (let ((char (string-ref str i)))
            (loop (- i 1) (string-append res (string char))))
          ))))


;Q3:  returns a list of all substrings with the given length of the given string.
(define (all-substrings str n)
  (letrec ((loop
            (lambda(b)
              (if (<= (+ b n) (string-length str))
                  (cons (substring str b (+ b n))(loop (+ b 1)))
                  '())
                  )))
    (loop 0)))
    




    