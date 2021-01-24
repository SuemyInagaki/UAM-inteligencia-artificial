; Alias para el ranking
(defvar *alias* '|DanielySuemy1|)

(defun eval-fn (player board)
  (let ((corners (list (aref board 11)
  						(aref board 18) 
  						(aref board 81) 
  						(aref board 88))))
    (- (length (remove-if-not #'(lambda (x) (eq x player)) corners))
       (length (remove-if-not #'(lambda (x) (eq x (opponent player))) corners)))))