#lang racket
 ;Переименуйие функции прежде чем пиздить лабу. Нет, Андрей != функция
;
;2.1
(define (same a)
   (= 1 (length(remove-duplicates(string->list(number->string a))))))

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
