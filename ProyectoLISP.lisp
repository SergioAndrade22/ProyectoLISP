(DEFUN sumaPrimos (N)
	(COND
		((= N 0) 
			0
		)
		((esPrimoShell(N)) 
			(+ N (sumaPrimos (- N 1)))
		)
		(T 
			(sumaPrimos (- N 1))
		)
	)
)

(DEFUN esPrimoShell (N)
	(LET ((A 2) (B (/ N 2)))
		(esPrimo N A B)
	)
)

(DEFUN esPrimo (N A B)
	(COND
		((> B A)
			(COND
				((= (REM N B) 0)
					NIL
				)
				((= (REM N A) 0)
					NIL
				)
				(T
					(esPrimo N (+ A 1) (- B 1))
				)
			)
		)
		(T
			NIL
		)
	)
)