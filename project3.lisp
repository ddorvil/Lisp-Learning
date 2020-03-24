(setq ext:*warn-on-redefinition* nil)+
; (print "What's your name")

; (defvar *name* (read))

; (defun hello-you (*name*)
; 	(format t "Hello ~a! ~%" *name*))

; (hello-you *name*)

; (print "Enter a number: ")
; (defvar num (read))
; (defun abs (num)
; 	(if (= num)
; 		(format t "~d" *num*)
; 		(format t (* num -1 ~a))))

; list functions 

; for some reason copy and paste does not work
(defun append (x y)
	(if (null x)
		y
		(cons (first x)
			(append (rest x) y))))



; math functions

(defun abs (x)
	(princ "This is the absolute value function")
	(if (< x 0) (negate x)
		x))

(defun factorial (x)
	(princ "This is the factorial function")
	(if (> x 0)
	(* x (factorial (- x 1)))
	1))

(defun gcd (x y) 
	(princ "This is the greatest common divisor function")
	(if (= y 0)
		x
		(gcd y (mod x y))
		)
	)

(defun lcm (x y)
	(princ "This is the least common multiple function")
	; (* x y) = lcm(x y) *(/ (gcd(x y))
	; lcm(x y) = (* x y) / (gcd(x y))))
	; (* x y (/ gcd(x y)))
	(/(abs(* x y)) (gcd x y)))
	
	

; helper function for absolute value function
(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))

