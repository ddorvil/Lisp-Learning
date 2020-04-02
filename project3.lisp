(setq ext:*warn-on-redefinition* nil)+

; list functions 

; append two lists
(defun append (x y)
	(if (null x)
		y
		(cons (first x)
			(append (rest x) y))))

; reverse a list
(defun reverse (x)
	(if (eq x())
		()
		(append (reverse (cdr x)) (list(car x))))
	)

; Map a function over every element in a list
;(defun add3 (x) (+ 3 x))

; (defun map (m l)                              ;can't figure this one out
;   (if (eq l nil)
;       l
;       (funcall m l)
;     )
; )

; remove duplicates from a list
(defun nub (l)                                               
    (reverse (nub-helper l nil))                                                      
)

; helper function for nub
(defun nub-helper (x y)                                                                
    (if (eq x nil)                                                                              
        y
        (if (member (car x) y)                                                          
            (nub-helper (cdr x) y)
            (nub-helper (cdr x) (cons (car x) y))                                  
        )
    )
)

; fold-left
(defun fold (n m l)                                         
    (if(eq l nil) l
        (funcall m n (apply '+ l))
    )
)

; add an element to the end of the list
(defun addtoend (x y)
	(if(null y)
		'()

	(reverse (cons x (reverse y)))))

; index of 
(defun indexof (e l)                                                                                          
    (indexof-helper e l 0)                                                              
)

; index of helper function 
(defun indexof-helper (e l i)                                                      
    (if (eq l nil)                                                                            
        -1
        (if (eq (car l) e)                                                                  
            i
            (indexof-helper e (cdr l) (+ i 1))                                      
        )
    )
)

; remove-all function 
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

; set functions
; set membership 
(defun member (x y)
	(if (null y) nil
		(if (equalp x (car y)) t
	    (member x (cdr y)))))

; insert function
(defun insert (n s)
    (if(member n s)
        s
        (cons n s)
    )
)

; set intersection	
(defun intersection (x y)
	(cond ((null x) nil)
		( (member (first x) y)(cons(first x) (intersection (rest x) y)))
		(t (intersection(rest x) y))))

; set union
(defun union (x y)
	(cond ((null x) y)
		((member (first x) y) (union(rest x) y))
		(t (cons(first x)(union(rest x) y)))))

; set difference 
(defun difference (x y)
	(cond
		((null x) x)
		    ((member (car x) y) (difference (cdr x) y))
    	(t (cons (car x) (difference (cdr x) y)))))

; symmetrical difference 
(defun symdiff (x y) 
	(union (difference x y) (difference y x)))

; set cardinality 
(defun cardinality (x)
	(if (null x)
		0
		(1+ (cardinality (rest x) ))))

; math functions

; absolute value function
(defun abs (x)
	(princ "This is the absolute value function")
	(if (< x 0) (negate x)
		x))

; factorial function 
(defun factorial (x)
	(princ "This is the factorial function")
	(if (> x 0)
	(* x (factorial (- x 1)))
	1))

; right triangle function 
(defun right-tri (a b c)                                    
    (eq (* c c) (+ (* a a) (* b b)))
)

; GCD function 
(defun gcd (x y) 
	(princ "This is the greatest common divisor function")
	(if (= y 0)
		x
		(gcd y (mod x y))
		)
	)

; LCM function 
(defun lcm (x y)
	(princ "This is the least common multiple function")
	; (* x y) = lcm(x y) *(/ (gcd(x y))
	; lcm(x y) = (* x y) / (gcd(x y))))
	; (* x y (/ gcd(x y)))
	(/(abs(* x y)) (gcd x y)))

; fibonacci numbers 
(defun nth-fibo(n) 
	(cond((eq n 0) 0) 
		((or (eq n 1) (eq n 2)) 1) 
		((+ (nth-fibo (- n 1)) 
		    (nth-fibo (- n 2))))))

; prime numbers 
(defun primep (n &optional (d (- n 1))) 
	(if (/= n 1) 
	    (or (= d 1) 
		(and (/= (rem n d) 0) 
		     (primep n (- d 1)))) ()))

	

; helper function for absolute value function
(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))

;required functions 
(defun is-divisible(x y)
    (= (mod x y) 0))

(defun getDivisors(x y divisor)
    (if (equalp x divisor)
        y (if (is-divisible x divisor)
            (getDivisors x (+ y divisor) (+ divisor 1))
            (getDivisors x y (+ divisor 1))
        )
    ))

(defun sum(x)
    (getDivisors x 0 1))

(defun perfectp(x)
    (equalp (sum x) x))

(defun abundantp (x)
	(>= (sum x) x)

	)

(defun deficientp (x)
	(<= (sum x) x))

(defun intro ()
    (format t "~%***
*** CSC173 Computation and Formal Systems
*** Project 3: Functional Programming in Lisp
***************
*** Dominique Dorvil
*** Lamine Male
****************
*** List functions
(APPEND (1 3 X A) (4 2 B)) => (1 3 X A 4 2 B)
(REVERSE (A B C D)) => (D C B A)
(NUB (1 1 2 4 1 2 5)) => (1 2 4 5)
(FOLD 10 - (1 3 2)) => 4
(ADDTOEND D (A B C)) => (A B C D)
(ADDTOEND2 D (A B C)) => (A B C D)
(INDEXOF A (B C A D)) => 2
(INDEXOF A (B C D F)) => -1
(REMOVE-ALL A (B A C A A D A)) => (B C D)

*** Set functions
(MEMBER A (B C A D)) => T
(INSERT A (B C D)) => (A B C D)
(INSERT A (A B C D)) => (A B C D)
(INTERSECTION (A B C) (A C D)) => (A C)
(UNION (A B C) (A C D)) => (A B C D)
(DIFFERENCE (A B C) (A C D)) => (B)
(DIFFERENCE (A C D) (A B C)) => (D)
(SYMDIFF (A B C) (A C D)) => (B D)
(CARDINALITY (A B C)) => 3


*** Math functions
(ABS 7) => 7
(ABS -7) => 7
(FACTORIAL 5) => 120
(RIGHT-TRI 3 4 5) => T
(RIGHT-TRI 1 2 3) => NIL
(GCD 8 12) => 4
(LCM 4 6) => 12
(NTH-FIBO 6) => 8
(NTH-FIBO 10) => 55
(PRIMEP 5) => T
(PRIMEP 6) => NIL


*** Required functions
(PERFECTP 5) => NIL
(PERFECTP 6) => T
(ABUNDANTP 5) => NIL
(ABUNDANTP 12) => T
(DEFICIENTP 5) => T
(DEFICIENTP 12) => NIL~%")
)

(defun repl ()                                      
    (intro)                                                                                    
    (format t "~%Testing append function~%")
    (append-repl)
    (format t "~%Testing reverse function~%")
    (reverse-repl)
    (format t "~%Testing nub function~%")
    (nub-repl)
    (format t "~%Testing fold function~%")
    (fold-repl)
    (format t "~%Testing addtoend function~%")
    (addtoend-repl)
    (format t "~%Testing function indexof~%")
    (indexof-repl)
    (format t "~%Testing function remove-all~%")
    (remove-all-repl)
    (format t "~%Testing member function~%")
    (member-repl)
    (format t "~%Testing insert function~%")
    (insert-repl)
    (format t "~%Testing intersection function~%")
    (intersection-repl)
    (format t "~%Testing union function~%")
    (union-repl)
    (format t "~%Testing difference function~%")
    (difference-repl)
    (format t "~%Testing symdiff function~%")
    (symdiff-repl)
    (format t "~%Testing function cardinality~%")
    (cardinality-repl)
    (format t "~%Testing abs function~%")
    (abs-repl)
    (format t "~%Testing factorial function~%")
    (factorial-repl)
    (format t "~%Testing right-tri function~%")
    (right-tri-repl)
    (format t "~%Testing gcd function~%")
    (gcd-repl)
    (format t "~%Testing lcm function~%")
    (lcm-repl)
    (format t "~%Testing nth-fibo function~%")
    (nth-fibo-repl)
    (format t "~%Testing primep function~%")
    (primep-repl)
    (format t "~%Testing perfectp function~%")
    (perfectp-repl)
    (format t "~%Testing abundantp function~%")
    (abundantp-repl)
    (format t "~%Testing deficientp function~%")
    (deficientp-repl)
    "THIS IS THE END."
)

(defun quit (function input1 input2 input3 num)                      ;checks if input is quit
    (if (equalp (format nil "~a" input1) "quit")
        nil
        (print-function function input1 input2 input3 num)
    )
)

(defun print-function (function input1 input2 input3 num)                 ;Helps with repl, evaluates and prints input
    (if (eq num 1)
        (format t "(~a ~a) => ~a~%" function input1 (funcall function input1))
        (if (eq num 2)
            (format t "(~a ~a ~a) => ~a~%" function input1 input2 (funcall function input1 input2))
            (format t "(~a ~a ~a ~a) => ~a~%" function input1 input2 input3 (funcall function input1 input2 input3))
        )
    )
    t
)

(defun append-repl ()
    (format t "Input two lists (type \"quit append\" to quit): ")
    (finish-output nil)
    (if (quit 'append (read) (read) nil 2)
        (append-repl)
    )
)

(defun reverse-repl ()
    (format t "Input a list (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'reverse (read) nil nil 1)
        (reverse-repl)
    )
)

(defun nub-repl ()
    (format t "Input a set (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'nub (read) nil nil 1)
        (nub-repl)
    )
)

(defun fold-repl ()
    (format t "Input a  initial value, function, and list) (type \"quit fold fucntion\" to quit): ")
    (finish-output nil)
    (if (quit 'fold (read) (read) (read) 3)
        (fold-repl)
    )
)

(defun addtoend-repl ()
    (format t "Input an element and a list (type \"quit addtoend\" to quit): ")
    (finish-output nil)
    (if (quit 'addtoend (read) (read) nil 2)
        (addtoend-repl)
    )
)

(defun indexof-repl ()
    (format t "Input an element and a list (type \"quit indexof\" to quit): ")
    (finish-output nil)
    (if (quit 'indexof (read) (read) nil 2)
        (indexof-repl)
    )
)

(defun remove-all-repl ()
    (format t "Input an element and a list (type \"quit remove-all\" to quit): ")
    (finish-output nil)
    (if (quit 'remove-all (read) (read) nil 2)
        (remove-all-repl)
    )
)

(defun member-repl ()
    (format t "Input an element and a set (type \"quit member\" to quit): ")
    (finish-output nil)
    (if (quit 'member (read) (read) nil 2)
        (member-repl)
    )
)

(defun insert-repl ()
    (format t "Input an element and a set (type \"quit insert\" to quit): ")
    (finish-output nil)
    (if (quit 'insert (read) (read) nil 2)
        (insert-repl)
    )
)

(defun intersection-repl ()
    (format t "Input two sets (type \"quit intersection\" to quit): ")
    (finish-output nil)
    (if (quit 'intersection (read) (read) nil 2)
        (intersection-repl)
    )
)

(defun union-repl ()
    (format t "Input two sets (type \"quit union\" to quit): ")
    (finish-output nil)
    (if (quit 'union (read) (read) nil 2)
        (union-repl)
    )
)

(defun difference-repl ()
    (format t "Input two sets (type \"quit difference\" to quit): ")
    (finish-output nil)
    (if (quit 'difference (read) (read) nil 2)
        (difference-repl)
    )
)

(defun symdiff-repl ()
    (format t "Input two sets (type \"quit symdiff\" to quit): ")
    (finish-output nil)
    (if (quit 'symdiff (read) (read) nil 2)
        (symdiff-repl)
    )
)

(defun cardinality-repl ()
    (format t "Input a set (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'cardinality (read) nil nil 1)
        (cardinality-repl)
    )
)

(defun abs-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'abs (read) nil nil 1)
        (abs-repl)
    )
)

(defun factorial-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'factorial (read) nil nil 1)
        (factorial-repl)
    )
)

(defun right-tri-repl ()
    (format t "Input three numbers (type \"quit right triangle\" to quit): ")
    (finish-output nil)
    (if (quit 'right-tri (read) (read) (read) 3)
        (right-tri-repl)
    )
)

(defun gcd-repl ()
    (format t "Input two numbers (type \"quit gcd\" to quit): ")
    (finish-output nil)
    (if (quit 'gcd (read) nil nil 1)
        (gcd-repl)
    )
)

(defun lcm-repl ()
    (format t "Input two numbers (type \"quit lcm\" to quit): ")
    (finish-output nil)
    (if (quit 'lcm (read) nil nil 1)
        (lcm-repl)
    )
)

(defun nth-fibo-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'nth-fibo (read) nil nil 1)
        (nth-fibo-repl)
    )
)

(defun primep-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'primep (read) nil nil 1)
        (primep-repl)
    )
)

(defun perfectp-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'perfectp (read) nil nil 1)
        (perfectp-repl)
    )
)

(defun abundantp-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'abundantp (read) nil nil 1)
        (abundantp-repl)
    )
)


(defun deficientp-repl ()
    (format t "Input a number (type \"quit\" to quit): ")
    (finish-output nil)
    (if (quit 'deficientp (read) nil nil 1)
        (deficientp-repl)
    )
)

((repl)
