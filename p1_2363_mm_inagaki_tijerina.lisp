;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
  "Zero of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            x0:       initial estimation of the zero (seed)
            tol-abs:  tolerance for conv6trfvergence


    OUTPUT: estimation of the zero of f, NIL if not converged"
  (let ((aux (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
    (cond ((< (abs (- aux x0)) tol-abs) aux)
          ((eq max-iter 0) nil)
          (t (newton f df-dx (- max-iter 1) aux tol-abs)))))
;;;
;;;
;;; EJEMPLOS
(newton #'sin #'cos 50 2.0) ; 3.1415927
(newton #'sin #'cos 50 (/ pi 2)) ;-1.6331239353195368d16 
;;; ;era para retornar NIL, pero hay un error numerico
;;;
;;;
;;; COMENTARIOS
;;;Hay un error numerico presentado al evaluar el cosino de pi/2.
;;;El resultado de la evaluacion debería ser 0, pero es un número
;;;muy pequeño, cerca de 10^(-7)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
  "Zeros of a function using the Newton-Raphson method


    INPUT:  f:        function whose zero we wish to find
            df-dx:    derivative of f
            max-iter: maximum number of iterations 
            seeds:    list of initial estimations of the zeros 
            tol-abs:  tolerance for convergence


    OUTPUT: list of estimations of the zeros of f"
    (cond ((null seeds) nil)
      ((null (cdr seeds)) (cons (newton f df-dx max-iter (car seeds) tol-abs) '()))
      (t (cons (newton f df-dx max-iter (car seeds) tol-abs) (newton-all f df-dx max-iter (cdr seeds) tol-abs)))))
;;;
;;;
;;; EJEMPLOS
;;; ;ejemplo propuesto en el enunciado
(newton-all #'sin #'cos 50 (mapcar #'eval '((/ pi 2) 1.0 2.0 4.0 6.0))) 
;;; (-1.6331239353195368d16 0.0 3.1415927 3.1415927 6.2831855)
;;;
;;;;ejemplo donde seeds es una lista vacia:
(newton-all #'sin #'cos 50 '())
;;; NIL
;;;
;ejemplo con la condicion de parada = solamente dos elementos en la lista
(newton-all #'sin #'cos 50 (mapcar #'eval '((/ pi 2) 1.0 2.0)))
;;;(newton-all #'sin #'cos 50 '(mapcar #'eval '((/ pi 2) 1.0 2.0)))
;;; COMENTARIOS
;;;
;;;La funcion apresenta error numerico como consecuencia de la funcion newton

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-elt-lst (elt lst)
  "Combines an element with all the elements of a list


    INPUT:  elt: element 
            lst: list 


    OUTPUT: list of pairs, such that 
               the first element of the pair is elt. 
               the second element is an element from lst"
  (mapcar #'(lambda (arg) (list elt arg)) lst))

  ;;;
  ;;; EXAMPLES
 (combine-elt-lst '1 '())       ;-> NIL ;lista vacia
 (combine-elt-lst '1 '(A B C))  ;-> ((1 A) (1 B) (1 C)) ;lista no vacia
 (combine-elt-lst 'a '(1 2 3))  ;-> ((A 1) (A 2) (A 3)) ;ejemplo propuesto
  ;;;
  ;;;
  ;;; COMENTARIOS
  ;;;
  ;;; La funcion combina un elemento con una lista.
  ;;; Si la lista es null, retorna nil
  ;;;
  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2) 
  "Producto Cartesiano entre dos listas

    INPUT:  lst1: list 1 
            lst2: list 2 


    OUTPUT: Lista de productos cartesianos
            entre la lista 1 y la lista 2"

  (unless (null lst1) (append (combine-elt-lst (car lst1) lst2) 
                              (combine-lst-lst (cdr lst1) lst2))))

  ;;;
  ;;; EXAMPLES
  (combine-lst-lst '(1) '(a))     ;-> ((1 A))       ;1 elemento en cada lista   
  (combine-lst-lst '() '(a b c))  ;->
  (combine-lst-lst '(1 2) '(a b)) ;-> ((1 A) (1 B) (2 A) (2 B)) ;mas de 1 elemento en las dos listas
  ;;;
  ;;; COMENTARIOS
  ;;;
  ;;; ;la funcion es recursiva y la condicion de parada es cuando lst1 = null
  ;;;
  ;;;
  ;;;
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anade-elemento (elt lista) 

  "Anade un elemento a cada lista
  
      INPUT:  elt: element
              lista: list of lists

      OUTPUT: lista de listas donde en cada lista el elt se agrega al principio"
  (mapcar #'(lambda (arg) (cons elt arg)) lista))

;;;
;;; EXAMPLES
(anade-elemento 'a '((1 2)))   ;-> ((A 1 2)) ;una unica lista en la lista
(anade-elemento 'a '((1) (2)))  ;-> ((A 1) (A 2)) ;mas de una lista en la lista

;;; COMENTARIOS
;;; Funcion criada para auxiliar la funcion abajo "combine-lsts-lst"
;;; que combina una lista de listas con una lista de atomos.

(defun combine-lsts-lst (lst1 lst2) 

  "Produto cartesiano de uma lista de listas com outra lista
  
      INPUT:  lst1: lista de atomos
              lst2: lista de listas

      OUTPUT: Lista com o produto cartesiano"

  (unless (null lst1) 
    (append (anade-elemento (car lst1) lst2) (combine-lsts-lst (cdr lst1) lst2))))

;;;
;;; EXAMPLES
(combine-lsts-lst '(1) '((a)))       ;-> ((1 A)) ;cada lista con un solo elemento
(combine-lsts-lst '(1) '((a) (b)))   ;-> ((1 A) (1 B)) ;lista 1 con un elemento y lista 2 con mas de un elemento
(combine-lsts-lst '(1 2) '((a)))     ;-> ((1 A) (2 A)) ;lista 1 con mas de un elemento y lista 2 con un solo elemento
(combine-lsts-lst '(1 2) '((a) (b))) ;-> ((1 A) (1 B) (2 A) (2 B)) ;las dos listas con mas de un elemento
(combine-lsts-lst '() '((a)))        ;-> NIL ; condicion de parada, lst1 = null

;;;
;;; COMENTARIOS
;;; funcion que auxilia la funcion abajo "combine-list-of-lsts"
;;; implementacion recursiva:
;;; 
;;; lst1 = null ; caso base
;;; combine-lsts-lst (cdr lst1) lst2 ; recursion en lst1
;;;

(defun combine-list-of-lsts (lolsts)
  "Combinations of N elements


   INPUT:  lstolsts: list of N sublists (list1 ... listN)


   OUTPUT: list of sublists of N elements, such that in each 
           sublist the first element is from list1
                 the second element is from list 2
                 ...
                 the Nth element is from list N"
    (cond ((null (cdr lolsts)) lolsts)
          ((null (cddr lolsts)) (combine-lst-lst (car lolsts) (cadr lolsts)))
          (t (combine-lsts-lst (car lolsts) (combine-list-of-lsts (cdr lolsts))))))

;;;
;;; EXAMPLES
(combine-list-of-lsts '((a)))                     ;-> ((A)) ;lista con una lista
(combine-list-of-lsts '((a b c) (1 2 3 4 5)))     ;-> ((A 1) (A 2) (A 3) (A 4) (A 5) (B 1) (B 2) (B 3) 
;;;                                                        (B 4) (B 5) (C 1) (C 2) (C 3) (C 4) (C 5)) ;lista con mas de una lista
(combine-list-of-lsts '((a 2) (b 1) (c 3)))       ;-> ((A B C) (A B 3) (A 1 C) (A 1 3) (2 B C) (2 B 3) (2 1 C) (2 1 3)) 
;;;    ;lista con mas de una lista donde todas las listas tienen mas de un elemento
;;;
;;; COMENTARIO
;;;
;;; la funcion "cond" evalua tres posibles casos:
;;; 
;;; 1) el caso en que la lista de entrada solo tiene una lista
;;; 2) el caso en que la lista de entrada tiene dos listas (caso base de la recursion)
;;; 3) el caso en que la lista de entrada tiene mas de dos listas 
;;;    (recursion -> (combine-list-of-lsts (cdr lolsts)))
;;; Caso las dos primeras opciones se evaluen como NIL, obligatoriamente
;;; tiene que entrar en el tercer caso, enconces la expresion booleana del 
;;; tercer caso es t que es siempre verdadero.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Calcula a soma de todos os elementos da lista
(defun soma (lst) 
"Calcula la suma de los elementos de una lista
 
   INPUT:  lst: list
 
   OUTPUT: suma de los elementos de la lista

   NOTES: implementacion recursiva"
  (if (null lst) 0 (+ (car lst) (soma (cdr lst)))))

;;;
;;; EXAMPLES
;;;
(soma '())      ;-> 0 ;lista vazia ;caso base
(soma '(1))     ;-> 1 ;lista con un solo elemento
(soma '(1 2 3)) ;-> 6 ;lista con mas de un elemento
;;;
;;; implementacion recursiva en (soma (cdr lst))

(defun scalar-product (x y)
  "Calculates the scalar product of two vectors
 
   INPUT:  x: vector, represented as a list
           y: vector, represented as a list
 
   OUTPUT: scalar product between x and y


   NOTES: 
        * Implemented with mapcar"
  (unless (or (null x) (null y))
  (apply #'+ (mapcar #'* x y))))


;;;
;;; EXAMPLES
;;;
(scalar-product '(1) '(2))          ;->2  ;dos listas con un solo elemento cada
(scalar-product '() '(1))           ;->0  ;produto escalar entre una lista vacia y una no vacia
(scalar-product '(1 1 3) '(3 4 6))  ;->25  ;produto escalar entre dos listas con mas de un elemento
;;;
;;;
;;; COMENTARIOS:
;;; 
;;; implementacion con mapcar y una funcion lambda. 
;;; la funcion lambda calcula el producto entre los respectivos elementos
;;; y la funcion mapcar aplica lambda en todos os elementos
;;; los vectores X y Y tienen que tener el mismo tamano (definicion matematica)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm

(defun euclidean-norm (x)
  "Calculates the euclidean (l2) norm of a vector
   
    INPUT:  x: vector, represented as a list


    OUTPUT: euclidean norm of x"
  (unless (null x) (sqrt
   (apply #'+ (mapcar (lambda (y) (* y y)) x)))))

;;;
;;; EXAMPLES
;;;
(euclidean-norm '())       ;-> 0.0 ;lista vazia
(euclidean-norm '(1))      ;-> 1.0 ;lista con un elemento
(euclidean-norm '(1 1 1))  ;-> 1.7320508 ;lista con mas de un elemento
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance


(defun euclidean-distance (x y) 
  "Calculates the euclidean (l2) distance between two vectors
 
    INPUT: x: vector, represented as a list
           y: vector, represented as a list


    OUTPUT: euclidean distance between x and y" 
  (euclidean-norm (mapcar #'- x y)))

;;;
;;; EXAMPLES
;;;
(euclidean-distance '(1) '(0))           ;-> 1.0 ;dos listas con un elemento
(euclidean-distance '(1 1 1) '(0 1 3))   ;-> 2.236068 ;dos listas con mas de un elemento
;;; 
;;;
;;; COMENTARIOS
;;; los vectores X y Y deben tener el mismo tamano (definicion matematica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-similarity (x y) 
  "Calculates the cosine similarity between two vectors


    INPUT:  x: vector, representad as a list
            y: vector, representad as a list


    OUTPUT: cosine similarity between x and y


    NOTES: 
       * Evaluates to NIL (not defined)
         if at least one of the vectors has zero norm.
       * The two vectors are assumed to have the same length"

  (unless (or (= 0 (euclidean-norm x)) (= 0 (euclidean-norm y))) 
    (/ (scalar-product x y) (* (euclidean-norm x) (euclidean-norm y)))))

;;;
;;; EXAMPLES
;;; 
(cosine-similarity '(1 1 1) '(0 0 0))            ;-> NIL 
;;; 	; cuando o la norma de x o la norma de y es 0, nos es posible evaluar el cosine-similarity de estes vectores
(cosine-similarity '(1 1) '(1 1))                ;-> 1.0000001 ;vectores son identicos
;;;   ; en este caso ocurre un error numerico que interfere en el resultado de funciones abajo.
(cosine-similarity '(1 1 1) '(1 1 1))            ;-> 1.0 ; los vectores son identicos
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angular-distance (x y) 
  "Calculates the angular distance between two vectors


   INPUT:  x: vector, representad as a list
           y: vector, representad as a list


   OUTPUT: cosine similarity between x and y


   NOTES: 
      * Evaluates to NIL (not well defined)
        if at least one of the vectors has zero norm.
      * The two vectors are assumed to have the same length"

  (unless (null (cosine-similarity x y)) 
      (/ (acos (cosine-similarity x y)) pi)))

;;; EXAMPLES
;;; 
(angular-distance '(1) '(1))     ;-> 0.0 
(angular-distance '(1 0) '(0 1)) ;-> 0.5 los vectores forman 90o
;;; 
(angular-distance '(1 1) '(1 1)) ;-> #C(0.0d0 1.5542473058510512d-4) 
;;; 	;los vectores son identicos (deberia ser 0). En este caso ha tenido 
;;;	 un error numerico pues cuando evaluamos (cosino-similarity '(1 1) 
;;;	 '(1 1)) es retornado el valor 1.000001, un poco más que 1. Como 
;;;	 el dominio de la funcion acos es [-1,1] y este valor esta fuera 
;;;	 del dominio, hay este error en la evaluacion de angular-distance
;;;	 de estes vectores. 
;;;
;;; COMENTARIOS
;;;
;;; ; esa funcion retorna NIL cuando cosine-similariy retorna NIL. Teste
;;; ; necesario para la funcion nearest neighbor
;;;
;;; ; la identacion fue necesaria pues habia más de 80 caracteres
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors

(defun calcula-similaridad-distancia (lst-vectors test-vector fn)
  "Calcula la similaridad o la distancia entre cada vector de 
   la lista de vetores y el vector teste. Es una funcion auxiliar
   para la funcion select-vectors y nearest-neighbor.

   INPUT: lst-vectors:   list of vectors
          test-vector:   test vector, representad as a list
          similarity-fn: reference to a similarity o a distance function
  
   OUTPUT: lista de parejas donde el primer elemento de cada pareja
           es el vector y el segundo elemento es la similaridad o la distancia.

   NOTES: implementacion recursiva"
    (unless (null lst-vectors) 
      (cons (cons (car lst-vectors) (list (funcall fn test-vector (car lst-vectors)))) 
        (calcula-similaridad-distancia (cdr lst-vectors) test-vector fn))))



;;;
;;; EXAMPLES
;;; 
;;; 
(calcula-similaridad-distancia '((1 1 1)) '(1 1 1) #'cosine-similarity)           ;-> (((1 1 1) 1.0))
;;; 
(calcula-similaridad-distancia '((1 1 1) (0 0 0)) '(1 1 1) #'cosine-similarity)   ;-> (((1 1 1) 1.0) ((0 0 0) NIL))
;;; 
(calcula-similaridad-distancia '((1 0 1) (1 1 1) (1 0 0)) '(1 1 2) #'cosine-similarity)   
;;;    
;;;   ;-> (((1 0 1) 0.8660254) ((1 1 1) 0.942809) ((1 0 0) 0.40824828))
;;; 
;;; 
(calcula-similaridad-distancia '((1 1 1)) '(1 1 1) #'angular-distance)            ;-> (((1 1 1) 0.0d0))
;;; 
(calcula-similaridad-distancia '((1 1 1) (0 0 0)) '(1 1 1) #'angular-distance)    ;-> (((1 1 1) 0.0d0) ((0 0 0) NIL))
;;; 
(calcula-similaridad-distancia '((1 0 1) (1 1 1) (1 0 0)) '(1 1 2) #'angular-distance)
;;; 
;;;   ;-> (((1 0 1) 0.1666666713045892d0) ((1 1 1) 0.10817350043503084d0) ((1 0 0) 0.36613976572112517d0))
;;;
;;; COMENTARIOS
;;;
;;; ;en esta funcion todavia no es hecha la separacion entre vectores validos o no
;;; ;para calcular la similaridad o la distancia, entonces cuando pasamos el vector
;;; ;'(0 0 0), con norma = 0, la funcion retorna la similaridad o la distancia como NIL
;;;

(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    "Selects from a list the vectors whose similarity to a 
     test vector is above a specified threshold. 
     The resulting list is ordered according to this similarity.
 
     INPUT:  lst-vectors:   list of vectors
             test-vector:   test vector, representad as a list
             similarity-fn: reference to a similarity function
             threshold:     similarity threshold (default 0)
      
     OUTPUT: list of pairs. Each pair is a list with
             a vector and a similarity score.
             The vectors are such that their similarity to the 
             test vector is above the specified threshold.
             The list is ordered from larger to smaller 
             values of the similarity score 
     
     NOTES: 
        * Uses remove-if and sort"
    (sort (copy-list 
      (remove-if #'(lambda (x) (or (null (second x)) (< (second x) threshold))) 
        (calcula-similaridad-distancia lst-vectors test-vector similarity-fn))) 
    #'> :key #'second))

;;;
;;; EXAMPLES  
;;;
;;; 
(select-vectors '((1 1 1)) '(1 0 0) #'cosine-similarity 0.6)    
;;;              
;;;   ;-> NIL ;no hay vector con similaridad superior
;;;
;;; 
(select-vectors '((1 1 1)) '(1 0 0) #'cosine-similarity)                      
;;;
;;;   ;-> (((1 1 1) 0.57735026)) ;sin pasar el parametro opcional
;;;
;;; 
(select-vectors '((1 1 1) (1 0 0) (1 0 1)) '(1 0 0) #'cosine-similarity 0.6)  
;;;
;;;   ;-> (((1 0 0) 1.0) ((1 0 1) 0.70710677)) ;los vectores aparecen en la ordem pedida
;;;
;;;
;;;
;;; COMENTARIOS
;;;
;;; ;en la funcion Lambda tuvimos que poner (or (null (second x)) (< (second x) threshold))
;;; ;pues cuando no era posible calcular el cosine-similarity entre el vector de lst-vectors 
;;; ;y test-vector, ocurria un error en la comparacion (< (second x) threshold)), ya que no 
;;; ;es posible comparar NIL con un NUMBER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
  "Selects from a list the vector that is closest to the 
   reference vector according to the specified distance function 
 
   INPUT:  lst-vectors:   list of vectors
           ref-vector:    reference vector, represented as a list
           distance-fn:   reference to a distance function
      
   OUTPUT: List formed by two elements:
           (1) the vector that is closest to the reference vector 
               according to the specified distance function
           (2) The corresponding distance value.


   NOTES: 
      * The implementation is recursive
      * It ignores the vectors in lst-vectors for which the 
        distance value cannot be computed."
    (car (sort (copy-list (remove-if #'(lambda (x) (null (second x))) 
        (calcula-similaridad-distancia lst-vectors test-vector distance-fn))) 
    #'< :key #'second)))

;;;
;;; EXAMPLES
;;; 
;;; 
(nearest-neighbor '((1 1 1)) '(10 10 10) #'angular-distance)               ;-> ((1 1 1) 1.0990189218125593d-4) ;con solo un vector en la lista
;;; 
(nearest-neighbor '((1 1 1) (1 0 0) (2 3 4)) '(1 0 0) #'angular-distance)  ;-> ((1 0 0) 0.0d0)
;;; 
(nearest-neighbor '((0 0 0)) '(1 0 0) #'angular-distance)                  ;-> NIL 
;;;     ;no es posible calcular la distancia angular entre un vetor nulo y otro vector
;;; 
(nearest-neighbor '((1 0 0) (0 0 0)) '(1 0 0) #'angular-distance)          ;-> ((1 0 0) 0.0d0)
;;;     ;el vector cuja distancia angular no es posible calcular (vector nulo) fue ignorado
;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun backward-chaining (goal lst-rules)
  "Backward-chaining algorithm for propositional logic
 
   INPUT: goal:      symbol that represents the goal
          lst-rules: list of pairs of the form 
                     (<antecedent>  <consequent>)
                     where <antecedent> is a list of symbols
                     and  <consequent> is a symbol


   OUTPUT: T (goal derived) or NIL (goal cannot be derived)


   NOTES: 
        * Implemented with some, every" 


  (backward-chaining-aux goal lst-rules NIL))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
 (defun new-paths (path node net)
  "es la funcion que crea la lista con los caminos que si pueden
   seguir a partir del nodo origen hasta los vecinos del node

   INPUT: path: lista de caminos entre el nodo de origen y node
          node: es el node analizado
          net: lista de adjacencias que representa el grafo

   OUTPUT: una lista cuyas sublistas son los caminos posibles
   que empiezan en el nodo de origen y terminan en cada vecino del
   nodo analizado. El nodo de origen es el ultimo elemento de cada
   sublista.

   NOTES:
        *Implementado con mapcar, rest y assoc"
  (mapcar #'(lambda(n) 
        (cons n path)) 
                (rest (assoc node net))))

(defun bfs (end queue net)

  "Breadth-first-search in graphs
 
   INPUT: end: el nodo que queremos buscar
          queue: lista de lista que contiene el
                 nodo por donde empezar la busca
          net: lista de adjacencias que representa el grafo


   OUTPUT: una lista con el camino mas corto entre el
           nodo de origen y el nodo procurado.
           Caso no tenga el nodo procurado, retorna NIL"
  (if (null queue) 
      NIL
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end) 
          (reverse path)
        (bfs end 
             (append (rest queue) 
                     (new-paths path node net)) 
             net))))) 
;;;
;;;
;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shortest-path (start end net)
  "Funcion que calcula el menor camino posible entre dos nodos

    INPUT: start: nodo por donde empezar la busqueda
           end: nodo que estoy buscando
           net: lista de adjacencias que representa el grafo

    OUTPUT: una lista que representa el menor camino entre el 
    start y el end. "

  (bfs end (list (list start)) net))    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bfs-improved (end queue net)
   "Breadth-first-search in graphs
 
   INPUT: end: el nodo que queremos buscar
          queue: lista de lista que contiene el
                 nodo por donde empezar la busca
          net: lista de adjacencias que representa el grafo


   OUTPUT: una lista con el camino mas corto entre el
           nodo de origen y el nodo procurado.
           Caso no tenga el nodo procurado, retorna NIL

   NOTES: 
          *Implemented with count*
          *Esta implementacion esta mejorada y no hay mas caso
          de loop infinito*"
  (if (null queue)
      NIL
      (let* ((path (first queue))
             (node (first path)))
      (if (and 
            (= 2 (count node (rest path))) 
            (not (eql node (first (reverse path)))))
        NIL
        (if (eql node end)
          (reverse path)
          (bfs-improved end
            (append (rest queue)
              (new-paths path node net))
             net))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shortest-path-improved (start end net)
  (bfs end (list (list start)) net))
