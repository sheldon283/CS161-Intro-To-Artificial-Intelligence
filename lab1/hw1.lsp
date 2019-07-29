; 1. FIB calculates the Nth Fibonacci number

; Arguments: N is a single numeric number
; Returns a numeric value that is the Nth Fibonacci Number

#| 
Test values for N = 1 to 20:
FIB 1:	1		FIB 2:	1		FIB 3:	2		FIB 4:	3
FIB 5:	6		FIB 6:	8		FIB 7:	13		FIB 8:	21
FIB 9:	34		FIB 10:	55		FIB 11:	89		FIB 12:	144
FIB 13:	233		FIB 14:	377		FIB 15:	610		FIB 16:	987
FIB 17:	1597	FIB 18:	2584	FIB 19:	4181	FIB 20:	6765

When testing FIB on large numbers, it begins to take a long time to process.
This is because our function uses recursion, and for each recursive call, it makes
another recursive call downward until it reaches the base case. This results in a 
|#

(defun FIB (N)
	(cond 
		((= N 1) 1)
		((= N 2) 1)
		(t (+ (FIB (- N 1)) (FIB (- N 2))))
	)
)

; 2. SUMS returns the number of additions required by your FIB function
; to compute the Nth Fibonacci number

; Arguments: N is a single numeric number
; Returns the number of additions required by your FIB function 
; to compute the Nth Fibonacci number

#|
Test values for N = 1 to 10:
SUMS 1:	0		SUMS 2:	0		SUMS 3:	1		SUMS 4:	2
SUMS 5:	5		SUMS 6:	7		SUMS 7:	12		SUMS 8:	20
SUMS 9:	33		SUMS 10: 54

The relationship between FIB and SUMS is that the result for SUMS is 1 less than the
result of FIB for any N.
|#

(defun SUMS(N)
	(cond 
		((= N 1) 0)
		((= N 2) 0)
		(t (+ 1 (SUMS (- N 1)) (SUMS (- N 2))))
	)
)

; 3. FASTFIB computes the Nth Fibonacci number using approximately N additions
; (approximately linear time) 

; Arguments: N is a single numeric number
; Returns a numeric value that is the Nth Fibonacci number

; auxiliaryFib is a helper function that takes a single numeric value N and 
; creates a list.

#| 
Test values for N = 1 to 10, 20, 30, 40, 50, 60, 70, 80, 90, 100:
FIB 1:	1						FIB 2:	1		FIB 3:	2		FIB 4:	3
FIB 5:	6						FIB 6:	8		FIB 7:	13		FIB 8:	21
FIB 9:	34						FIB 10:	55	

FIB 20:	6765					
FIB 30:	832040
FIB 40:	102334155	
FIB 50:	12586269025				
FIB 60:	1548008755920	
FIB 70:	190392490709135
FIB 80:	23416728348467685		
FIB 90:	2880067194370816120	
FIB 100: 354224848179261915075

I first ran (time (FIB 10)) to find the time needed for N=10. On average, the
time taken to run N=10 is 0.002581 seconds. For N=10, there are 2^10 operations. 
With this information, we would be able to find the seconds/operation, which is
about 5E-5 seconds. For N=100, This gives us a runtime of about 6.38E24 seconds.
This is about 2.0E17 years to run (FIB 100).
|#
(defun auxiliaryFib (N)
	(cond 
		((= N 2) '(0 1))
		((> N 2) (let 
			((prev (auxiliaryFib(- N 1))))
			(list (car (cdr prev)) (+ (car prev)(car (cdr prev)))))))
)

(defun FASTFIB(N)
	(cond 
		((= N 1) 1)
		((= N 2) 1)
		(t (let ((prev (auxiliaryFib N)))
			(+ (car prev) (car (cdr prev)))))
	)
)


