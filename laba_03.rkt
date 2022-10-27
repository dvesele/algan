#lang scheme
 ;Scheme_2 laboratory work tasks solution
 ;Laboratory work timestamp: 27/10/2022
 ;Done by Kariagina Polina, group MT-101 / MEH-120101

;Task 1
(define (divByOwnDigits x)
	(define (sDivByOwnDigits k)
		(if (= k 0) #t
			(if (= 0 (remainder k 10))
				#f
				(if (= 0 (remainder x (remainder k 10)))
					(sDivByOwnDigits (quotient k 10))
					#f
				)
			)
		)
	)
	(if (= x 0) #f
		(sDivByOwnDigits x)
	)
)


; task 2
(define (binpower n)
	(define (isInteger q) (= (floor q) q))
	(define r (/ (log n) (log 2)))
	(if (isInteger r) r #f)
)

; task 3
(define (collatzSequence n a0)
	(define (iter aK n seq)
		(if (= n 1) seq
			(let ( 
					(aO (+ (* 3 aK) 1) )  
					(aE (/ aK 2) )
				)
				(if (= 0 (remainder aK 2))
					(iter aE (- n 1) (cons aE seq))
					(iter aO (- n 1) (cons aO seq))
				)
			)
		)
	)

	(reverse 
		(iter a0 n (cons a0 null))
	))

;task 4
(define (isSemiprime number)
	(define (isPrime n) 
		(define (sIsPrime q)
			(if (and (<= (* q q) n) (not (= 0 (remainder n q))) ) (sIsPrime (+ q 2))
				(> (* q q) n)
			)
		)
		(if (= 0 (remainder n 2)) (= n 2)
			(sIsPrime 3)
		)
	)
	(define (sIsSemiprime q)
		(if (= q 1) #t
			(if (= 0 (remainder number q)) (and (isPrime q) (isPrime (/ number q)))
				(sIsSemiprime (- q 1))

			)
		)
	)
	(if (isPrime number) #t
		(sIsSemiprime (- (ceiling (sqrt number)) 1))
	)
)

;task 5
(define (digitInCell cell)
	(define (pow10 n)
		(define (iter n q)
			(if (= n 0) q
			(iter (- n 1) (* q 10) ) )
		)
		(iter n 1)
	)
	(define (detRange probeLen prevTotal)
		(let ( (q (* 9 probeLen (pow10 (- probeLen 1)) ) ) )
			(if (> 0 (- cell (+ prevTotal q)))
				(cons probeLen (- cell prevTotal) )
				(detRange (+ 1 probeLen) (+ q prevTotal))
			)
		)
	)
	(define range (detRange 1 0))
	(define digQ (car range))
	(define dist (cdr range))
	(if (= dist 0) 9 
		(remainder (quotient
			(+ (pow10 (- digQ 1)) (ceiling (/ dist digQ)) -1 )
		(pow10 (- digQ
			(if (= 0 (remainder dist digQ) ) digQ (remainder dist digQ) )
		))) 10)
	)
)