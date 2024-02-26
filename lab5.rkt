#lang racket
;Q1
;test case:
;   >(insert #(15 12 4 8) 32)
;   '#(32 15 4 8 12)

(define (insert heap value)
   (let ((new-heap (vector-append heap (vector value))))
    (upheap new-heap)
    new-heap))

(define (upheap heap)
  (let ((li (- (vector-length heap) 1 )))
    (letrec ((help-upheap
              (lambda (heap i)
                (let ((parenti (floor (/ (- i 1) 2))))
                  (cond ((< i 1) heap)
                        ((> (vector-ref heap i) (vector-ref heap parenti))
                         (let ((temp (vector-ref heap i)))
                           (vector-set! heap i (vector-ref heap parenti))
                           (vector-set! heap parenti temp)
                           (help-upheap heap parenti)))
                        (else heap))))))
      (help-upheap heap li))))



;Q2
;test case:
;    > (remove #(89 65 45 5 2 41))
;    '#(65 41 45 5 2 ())
(define (remove heap)
  (let ((mutable (vector-copy heap)))
    (let ((len (- (vector-length mutable) 1)))
      (vector-set! mutable 0 (vector-ref mutable len))
      (vector-set! mutable len null)
      (downheap mutable)
      mutable)))

(define (downheap heap)
  (let ((len (- (vector-length heap) 1)))
    (letrec ((help-downheap
              (lambda (heap i)
                (let ((leftchild (+ (* i 2) 1)) (rightchild (+ (+ i 2) 2)))
                  (cond ((or (= leftchild len) (= rightchild len))
                         heap)
                        ((> leftchild len)
                         heap)
                        ((> rightchild len)
                         (if (> (vector-ref heap leftchild) (vector-ref heap i))
                             (let ((temp (vector-ref heap leftchild)))
                               (vector-set! heap leftchild (vector-ref heap i))
                               (vector-set! heap i temp)
                               (help-downheap heap leftchild))
                             heap))
                        ((or (> (vector-ref heap leftchild) (vector-ref heap i))
                             (> (vector-ref heap rightchild) (vector-ref heap i)))
                         (if (> (vector-ref heap leftchild) (vector-ref heap rightchild))
                             (let ((temp (vector-ref heap leftchild)))
                               (vector-set! heap leftchild (vector-ref heap i))
                               (vector-set! heap i temp)
                               (help-downheap heap leftchild))
                             (let ((temp (vector-ref heap rightchild)))
                               (vector-set! heap rightchild (vector-ref heap i))
                               (vector-set! heap i temp)
                               (help-downheap heap rightchild))))
                        (else heap))))))
      (help-downheap heap 0))))