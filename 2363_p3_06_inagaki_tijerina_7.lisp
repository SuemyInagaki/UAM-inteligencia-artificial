; Alias para el ranking
(defvar *alias* '|DanielySuemyPosition5|)

(defun eval-fn (player board)
  (+ 
  	(- 
  		(* 5 (length (filter (corners board) #'(lambda (x) (eq x player))))) 
  		(* 5 (length (filter (corners board) #'(lambda (x) (eq x (opponent player)))))))
  	(- 
  		(get-corners (legal-moves player board)) 
  		(get-corners (legal-moves (opponent player) board)))
    (-
      (* 2 (length (filter (sides board) #'(lambda (x) (eq x player)))))
      (* 2 (length (filter (sides board) #'(lambda (x) (eq x (opponent player)))))))
    (-
      (* 0.5 (get-sides (legal-moves player board)))
      (* 0.5 (get-sides (legal-moves (opponent player) board))))))

(defun filter (l q)
	(remove-if-not q l))

(defun corners (board)
	(list (aref board 11) 
        (aref board 18) 
        (aref board 81) 
        (aref board 88)))

(defun get-corners (l)
	(length (filter l #'(lambda (x) (member x '(11 18 81 88))))))

(defun sides (board)
  (mapcar #'(lambda (x) (aref board x)) 
    '(13 14 15 16 31 41 51 61 38 48 58 68 83 84 85 86)))

(defun get-sides (l)
  (length (filter l #'(lambda (x) (member x 
    '(13 14 15 16 31 41 51 61 38 48 58 68 83 84 85 86))))))