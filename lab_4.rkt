#lang racket
(define (rotate vec m)
  (let ((len (vector-length vec)))
    (letrec ((help-rot
             (lambda (vect res i m)
               (cond ((= i len)
                      res)
                     ((= len m)
                      (help-rot vect res (+ 1 i) 0))
                     ((= i 0)
                      (vector-set! res 0 (vector-ref vect m))
                      (vector-set! res (- len 1) (vector-ref vect (- m 1)))
                      (help-rot vect res (+ 1 i) (+ 1 m)))
                     (#t
                      (vector-set! res i (vector-ref vect m))
                      (help-rot vect res (+ 1 i) (+ 1 m)))
                    )
               )
      ))
      (help-rot vec (make-vector len) 0 m))
      ))


;(distance-recursive ‘#(100 22 34 56 22 18 8 22 99 11) 22) ==> 6

(define (distance-recursive vec x)
  (let ((len (vector-length vec)))
    (letrec ((help-search
              (lambda (i f l c)
                (cond ((= i len) (if (> c 1) (- l f) 0))
                      ((> c 0)
                       (if (= (vector-ref vec i) x)
                           (help-search (+ i 1) f i (+ c 1))
                           (help-search (+ i 1) f l c)))
                      ((= (vector-ref vec i) x)
                       (help-search (+ i 1) i l (+ c 1)))
                      
                      (#t
                       (help-search (+ i 1) f l c))
                      ))))
      (help-search 0 0 0 0))))

(define (distance-loop vec x)
  (let ((len (vector-length vec))
        (f -1)
        (l -1)
        (c 0))
    (let loop ((i 0))
      (cond ((>= i len) (if (and (>= f 0) (>= l 0))
                           (- l f)
                           0))
            ((= (vector-ref vec i) x)
             (if (> c 0)
                 (set! l i)
                 ((set! f i)(+ c 1)))
             (loop (+ i 1)))
            (else (loop (+ i 1)))))
    )
  )
             
    