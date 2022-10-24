#lang racket
;1
(define (min a)
  (define (g a s)
    (if (= a 0) s (g(quotient a 10)(if (< s (remainder a 10)) s (remainder a 10)))))
  (g (abs a) (abs (remainder a 10))))
;2.1
(define (same a)
   (= 1 (length(remove-duplicates(string->list(number->string a))))))
;2.2
(define (same1 a)
  (define (itter a s)
    (if (= a 0) (= 1 (length s))
        (let ((x (remainder a 10)) (y (quotient a 10)))
          (if (member x s) (itter y s) (itter y (cons x s))))))
  (if (= a 0) #t (itter a null)))
;2.3
(define (same2 x)
  (define (f a b)
    (if (= a 0) #t (if (= (remainder a 10) b)(f (quotient a 10) b) #f )))(f x (remainder x 10)))
;3
(define (factorial a)
  (define (number c d)
    (if (if (= a c) (- d 1) (if (< c a) (number (* c d) (+ d 1)) #f)) #t #f))
  (number 1 1))
;4
(define (fibonacci a)
  (define (andrey g s)
    (if (>= s a)
        (if (> (- s a)(- a (- s g))) g s)
        (andrey (-(+ g s) g) (+ g s)))) (andrey 1 1))
;5
(define (perfect a)
  (define (it s d)
    (if (>= a (sqr d))
        (if (= 0 (remainder a d))
            (if (= (sqr d) a)
                (it (+ s d) (+ d 1))
                (it (+ s d (quotient a d)) (+ d 1)))
            (it s (+ d 1)))
        s))
  (and (> a 1) (= a (it 1 2))))