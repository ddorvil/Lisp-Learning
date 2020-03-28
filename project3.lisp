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



(defun list. (x y)
  (cons x (cons y '())))

(defun append (x y)
	(if (null x)
		y
		(cons (first x)
			(append (rest x) y))))

(defun reverse (x)
	(if (eq x())
		()
		(append (reverse (cdr x)) (list(car x))))
	)

(defun addtoend (x y)
	(if(null y)
		'()

	(reverse (cons x (reverse y)))))

(defun remove-all (x y)
	(if (null y)
		'()
		(if (eq x (car y))
			(remove-all x (cdr y))
			(cons (car y) (remove-all x (cdr y))))))
		
; set
; (defun member (x y)
; 	(cond ((null y) x)
; 		((eq x (car y)) t)
; 		(t (member x (cdr y))))

; (defun member (x y)
; 	(cond ((null y) nil)
; 		((equal x (car y)) y)
; 		(t (member x (cdr y)))))

(defun member (x y)
	(if (null y) nil
		(if (equalp x (car y)) t
	    (member x (cdr y)))))

(defun union (x y)
	(cond ((null x) y)
		((member (first x) y) (union(rest x) y))
		(t (cons(first x)(union(rest x) y)))))
		
	
(defun intersection (x y)
	(cond ((null x) nil)
		( (member (first x) y)(cons(first x) (intersection (rest x) y)))
		(t (intersection(rest x) y))))
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

