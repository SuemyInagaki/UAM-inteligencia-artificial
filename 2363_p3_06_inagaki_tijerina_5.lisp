; Alias para el ranking
(defvar *alias* '|DanielySuemyD|)

(defun eval-fn (player board)
  (+ 
  	(- 
  		(* 5 (length (filter (corners board) #'(lambda (x) (eq x player))))) 
  		(* 5 (length (filter (corners board) #'(lambda (x) (eq x (opponent player)))))))
  	(- 
  		(get-corners (legal-moves player board)) 
  		(get-corners (legal-moves (opponent player) board)))))

(defun filter (l q)
	(remove-if-not q l))

(defun corners (board)
	(list (aref board 11) (aref board 18) (aref board 81) (aref board 88)))

(defun get-corners (l)
	(length (filter l #'(lambda (x) (member x '(11 18 81 88))))))

