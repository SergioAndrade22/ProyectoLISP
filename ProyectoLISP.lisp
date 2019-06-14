;-------------------------------------------EJERCICIO MATRIZ TRASPUESTA------------------------------------------
; Funcion que computa la traspuesta de una matriz representada como una lista de listas
(DEFUN trans (M)
	(COND
		((LISTP M) ; primero se verifica que M sea efectivamente una lista
			(COND
				((OR (NOT (LISTP (CAR M)))(NOT (matrix M (LIST-LENGTH (CAR M))))) ; luego, que M sea una representacion valida de una matriz
					"ERROR: La lista ingresada no es una matriz."
				)
				(T ; si es una matriz valida comienza la ejecucion
					(COND
						((NULL (CAR M)) ; si M es vacia, la traspuesta de una matriz vacia es una matriz vacia
							NIL
						)
						(T ; si no, la transpuesta de M es la concatenacion de la primer fila transpuesta con la transpuesta de M',
							;siendo M' M sin el primer elemento de cada fila
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

; Funcion auxiliar que permite obtener, a partir de una matriz M representada como lista de listas, 
; una fila resultado de concatenar el primer elemento de cada fila de la matriz
(DEFUN compRow (M)
	(COND
		((NULL M) ; si M es vacia, una fila traspuesta de una matriz vacia es una fila vacia
			NIL
		)
		(T ; si no, se concatena el primer elemento de la primer fila M, 
			;con la fila fila resultante de aplicar compRow sobre M', siendo M' el cuerpo de M
			(CONS (CAR (CAR M)) (compRow (CDR M)))
		)
	)
)

; Funcion auxiliar que, dada una matriz M representada como lista de listas, elimina el primer elemento de cada fila
(DEFUN reduceMatrix (M)
	(COND
		((NULL M) ; si M es vacia su reduccion es vacio
			NIL
		) 
		(T ; si no, la reduccion de M es el resultado de concatenar la primer fila de M sin su primer elemento, 
			;con la reduccion de M', siendo M' el cuerpo de M
			(CONS (CDR (CAR M)) (reduceMatrix (CDR M)))
		)
	)
)

; Funcion auxiliar que verifica que la matriz pasado por parametro cumple con la condicion de tener siempre 
; el mismo numero de columnas en todas sus filas
; M es la matriz que se desea verificar
; I es la cantidad de columnas en la primer fila de M
(DEFUN matrix (M I)
	(COND 
		((= (LIST-LENGTH M) 1) ; si resta un unico elemento de en la matriz
			(COND
				((= (LIST-LENGTH (CAR M)) I) ; se verifica que el largo de ese elemento coincide con el inicial
					T
				)
				(T ; si no lo es, M no era una matriz en un principio
					NIL
				)
			)
		)
		(T ; si hay mas de un unico elemento en M 
			(COND
				((= (LIST-LENGTH (CAR M)) I) ; en caso de que coincidan el largo de la primer fila y el largo de la cabeza, 
											;M sera una matriz <=> M', siendo M' M sin su primer fila, es una matriz
					(matrix (CDR M) I)
				)
				(T ; si no, M no era una matriz
					NIL
				)
			)
		)
	)
)
;-------------------------------------------EJERCICIO SUMA DE PRIMOS-------------------------------------------
; Función que calcula la suma de todos los números naturales primos hasta un N dado. Hasta un N = 3352
(DEFUN sumaPrimos (N)
	(COND
		((INTEGERP N) ; ejecucion valida solo si N es un integer
			(COND
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
		(isPrime N B) ; llamada a la funcion que verficia si un numero es primo o no
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
; Funcion que calcula la permutacion lexica de una lista L suministrada por parametro. 
; Se asume que L se encuentra ordenada lexicograficamente
(DEFUN permLex (L)
	(COND
		((LISTP L) ; verifico que L sea efectivamente una lista
			(COND
				((= 1 (LIST-LENGTH L)) ; la unica permutacion posible de una lista con un unico elemento es esa misma lista
					(LIST L)
				)
				(T
					(permute L L) ; llamada a la funcion auxiliar que realiza la permutacion
				)
			)
		)
		(T ; si el parámetro no es una lista no tiene sentido la función
			"ERROR: Esta funcion espera recibir por parametro una lista."
		)
	)
)

; Funcion que realiza la permutacion de una lista, llevando registro en otra de que elementos faltan permutar
; L1 es la lista de elementos que restan permutar
; L2 es la lista real con todos los elementos
(DEFUN permute (L1 L2)
	(COND
		((= (LIST-LENGTH L1) 0) ; si no restan elementos por permutar el resultado de la permutacion es vacio
			NIL
		)
		(T ; caso contrario la permutacion lexica de una lista L es el resultado de concatenar la cabeza de L 
			;a las permutaciones lexicas del cuerpo
			;y formar una lista entro esto y las permutaciones lexicas del resto de L
			(APPEND 
				(addToALL (CAR L1) (permLex (DELETE (CAR L1) L2))) 
				(permute (CDR L1) (reArrange (CAR L1) (DELETE (CAR L1) L2) (- (LIST-LENGTH L2) (- (LIST-LENGTH L1) 1))))
			)
		)
	)
)

; Funcion que, dada una lista de listas L, inserta el parametro E como cabeza de la misma
; devuelve una lista con las listas modificadas
(DEFUN addToALL (E L)
	(COND
		((= (LIST-LENGTH L) 0) ; si no hay mas elementos en la lista finaliza la recursion
			NIL
		)
		(T ; se realiza la concatenacion de E a la primer lista, luego se llama recursivamente
			(CONS (CONS E (CAR L)) (addToALL E (CDR L))) 
		)
	)
)

; Funcion que, dado un elemento E y una lista L inserta el elemento en la posicion I de L
(DEFUN reArrange (E L I)
	(COND
		((= I 0) ; si la I vale 0 encontre la posicion donde debo insertar E
			(CONS E L)
		)
		(T ; caso contrario, tomo por separado el cuerpo de L, reduzco I en 1, llamo recursivamente y vuelvo a concatenar
			(CONS (CAR L) (reArrange E (CDR L) (- I 1)))
		)
	)
)