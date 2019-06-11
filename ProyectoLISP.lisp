;-------------------------------------------EJERCICIO MATRIZ TRASPUESTA------------------------------------------
; Funcion que computa la traspuesta de una matriz representado como una lista de listas
(DEFUN trans (M)
	(COND
		((LISTP M) ; primero se verifica que M sea efectivamente una lista
			(COND
				((NOT (LISTP (CAR M))) ; luego se verifica que M sea una representacion valida de una matriz
					"ERROR: La lista ingresada no es una matriz."
				)
				((NULL (CDR M)) ; si M es una matriz de una unica fila, la traspuesta de M es M
					M
				)
				(T ; caso general
					(COND
						((NULL M) ; si M es NIL llego al caso base de la recursion
							NIL
						)
						((NULL (CAR M)) ; verificacion que evita una recursion infinita, si en M se encuentra un NIL se asume que ese es el final de la matriz
							NIL
						)
						(T ; caso recursivo que concatena la fila resultado de la instancia actual con el resultado de la siguiente instancia sobre M reducida en una fila
							(CONS (compRow M) (trans (reduceMatrix M)))
						)
					)
				)
			)
		)
		(T ; en caso de que M no fuera una lista esta funcion no tiene sentido alguno
			"ERROR: Esta funcion fue implementada para operar sobre matrices representadas como lista de listas."
		)
	)
)

; Funcion auxiliar que permite obtener, a partir de una matriz M representada como lista de listas, una fila resultado de trasponer la matriz
(DEFUN compRow (M)
	(COND
		((NULL M) ; si M es vacia, una fila traspuesta de una matriz vacia es una fila vacia
			NIL
		)
		(T ; en caso contrario se concatena el primer elemento de la primer fila de la instancia actual de M, con el primer elemento de la primer fila de la instancia siguiente con M reducida en una fila
			(CONS (CAR (CAR M)) (compRow (CDR M)))
		)
	)
)

; Funcion auxiliar que, dada una matriz M representada como lista de listas, elimina el primer elemento de cada fila
(DEFUN reduceMatrix (M)
	(COND
		((NULL M) ; si M es vacia no necesito continuar reduciendo, devuelvo vacio
			NIL
		) 
		(T ; caso contrario la reduccion de M es el resultado de concatenar la primer fila de M sin su primer elemento, con la reduccion sobre el cuerpo de M
			(CONS (CDR (CAR M)) (reduceMatrix (CDR M)))
		)
	)
)
;-------------------------------------------EJERCICIO SUMA DE PRIMOS-------------------------------------------
; Función que calcula la suma de todos los números naturales primos hasta un N dado. Hasta un N = 3352
(DEFUN sumaPrimos (N)
	(COND
		((INTEGERP N)
			(COND
				((> N 3352)
					"EROR: el numero suministrado supera el limite de ejecucion del programa"
				)
				((< N 0) ; el programa no considera la suma de numeros primos en "sentido negativo"
					"ERROR: Esta funcion espera recibir como argumento un entero mayor o igual a 0."
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
		(T ; si el parametro siministrado no es un entero no se puede saber hasta que numero calcular los primos
			"ERROR: Esta funcion fue implementada para operar sobre enteros."
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

;-------------------------------------------EJERCICIO PERMUTACION LEXICA-------------------------------------------
; Funcion que calcula la permutacion lexica de una lista L suministrada por parametro. Se asume que L se encuentra ordenada lexicograficamente
(DEFUN permLex (L)
	(COND
		((NOT(LISTP L)) ; si L no es una lista esta funcion no tiene razon de ejecutarse
			"ERROR: se espera recibir una lista como dato de entrada."
		)
		((NULL L)
			NIL
		)
		(T
			(permutar L L)
		)
	)
)

(DEFUN permutar (L L2)
	(COND
		((NULL L)
			NIL
		)
		(T
			(LET ((A L2))
				(LET ((N (LIST-LENGTH (DELETE (CAR L) A))))
					(APPEND 
						(concatenarMulti 
							(CAR L) 
							(rightShifts (DELETE (CAR L) A) (- N 1))
						)
						(permutar
							(CDR L)
							L2
						)
					)
				)
			)
		)
	)
)

(DEFUN concatenarMulti (A L)
	(COND
		((NULL (CDR L))
			(LIST (CONS A (CAR L)))
		)
		(T
			(CONS (CONS A (CAR L)) (concatenarMulti A (CDR L)))
		)
	)
)

(DEFUN rightShifts (L N)
	(COND
		((= N 0)
			(LIST L)
		)
		(T
			(LET ((SL (Shift L)))
				(APPEND (LIST L) (rightShifts SL (- N 1)))
			)
		)
	)
)

(DEFUN shift (L)
	(COND
		((= (LIST-LENGTH L) 0)
			NIL
		)
		(T
			(LET ((RL (REVERSE L)))
				(CONS (CAR RL) (REVERSE (CDR RL)))
			)
		)
	)
)