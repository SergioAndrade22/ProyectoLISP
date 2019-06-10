; Función que calcula la suma de todos los números naturales primos hasta un N dado. Hasta un N = 3352
(DEFUN sumaPrimos (N)
	(COND
		((< N 0) ; el programa no considera la suma de numeros primos en "sentido negativo"
			NIL
		)
		((= N 0)  ; si N es 0, corta la recursión se devuelve 0
			0
		)
		((isPrimeShell N) ; si el N actual es primo, la suma de primos de N es igual a N más la suma de primos de N-1
			(+ N (sumaPrimos (- N 1)))
		)
		(T ; si N no es primo, la suma de primos de N es igual a la suma de primos de N-1
			(sumaPrimos (- N 1))
		)
	)
)

; Función cáscara auxiliar a la función isPrime
(DEFUN isPrimeShell (N)
	(LET ((B 2))
		(isPrime N B)
	)
)
; Función auxiliar que calcula si un número dado "N" es primo, "B" es pasado por parámetro y empieza en 2. Se considera que 1 NO es primo.
(DEFUN isPrime (N B)
	(COND
		((= N 1) ; caso base de los "N", 1 no se considera número primo
			NIL
		)
		((< B (/ (+ N 1) 2))  ; estoy en un caso en el cual tengo que evaluar si N es primo. No existe B tal que B > N/2 y B es divisor de B
			(COND
				(( = (MOD N B) 0) ; si ocurre que el módulo (MOD) de N y B es 0, entonces N es divisible por B por lo cuál no es primo
					NIL
				)
				(T ; si N no era divisible por B debo verificar que no sea divisible por B+1
					(isPrime N (+ B 1))
				)
			)
		)
		(T ; llega el caso en que B >= N por lo que N es primo
			T
		)
	)
)