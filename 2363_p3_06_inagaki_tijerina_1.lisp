; Alias para el ranking
(defvar *alias* '|DanielySuemy3|)

; leva em consideração os movimentos ruins do oponente
; para tentar força-lo a realizar os movimentos ruins

(defun eval-fn (player board)
  (+  
    (* 6
      (-  
        (num-sides-x board (legal-moves (opponent player) board))
        (num-sides-x board (legal-moves player board))))
    (* 9
      (-
        (length (filter (corners board) #'(lambda (x) (eq x player)))) 
        (length (filter (corners board) #'(lambda (x) (eq x (opponent player)))))))
    (* 4.1 
        (- 
          (get-corners (legal-moves player board)) 
          (get-corners (legal-moves (opponent player) board))))
     
       (- 
          (length (legal-moves player board))
          (length (legal-moves (opponent player) board)))))



(defun filter (l q)
  (remove-if-not q l))

(defun corners (board)
  (list (aref board 11) (aref board 18) (aref board 81) (aref board 88)))

(defun get-corners (l)
  (length (filter l #'(lambda (x) (member x 
    '(11 18 81 88))))))


; bad squares for opponent
; bordas e X sem incluir os cantos
(defun sides-x (board)
  (mapcar #'(lambda (x) (aref board x))
  '(12 13 14 15 16 17  
      82 83 84 85 86 87 78 
      68 58 48 38 28 71 61 
      51 41 31 21 22 33 44 55 
      66 77 72 63 54 45 36 27)))


; retorna o numero de movimentos possiveis que incluem
; os movimentos que eu considero ruins (sides-x)
; lista = (legal-moves (opponent player) board)
; quando maior o numero retornado, melhor é para mim
; e pior é para o oponente
(defun num-sides-x (board lista)
  (if (null lista)
    0
    (if (find (car lista) (sides-x board)) 
      (+ 1 (num-sides-x board (cdr lista)))
      (num-sides-x board (cdr lista)))))
