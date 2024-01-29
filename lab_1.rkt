#lang racket
; TASK 1
(- (+ 7 (* 13 22)) (* (/ 51 64) (- 19 (/ 45 (+ 32 11)))))
(define PI 3.1415)
(define PI/4 (/ PI 4))

(define (trigo1 x)   ; first trigonometric equation
  (+ (expt (sin x) 2) (expt (cos x) 2))
  )

(define (trigo2-1 x) ; first part of the second trigonometric equation
   (* (* (sin x) 2) (cos x))
  )

(define (trigo2-2 x) ; second part of the second trigonometric equation
  (sin (* 2 x ))
  )

(define (trigo3-1 x) ; first part of the third trigonometric equation
  (- (expt (cos x) 2) (expt (sin x) 2))
  )

(define (trigo3-2 x) ; second part of the third trigonometric equation
  (cos (* 2 x ))
  )

; TASK 2

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1))))
  )

(define (power x y)
  (if (= y 0)
      1
      (* x (power x (- y 1))))  
  )

; TASK 3

(define (factsum x y) ; x! + y!  
  (+ (fact x) (fact y))
  )