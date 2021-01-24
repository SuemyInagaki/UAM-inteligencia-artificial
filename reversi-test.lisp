(load "reversi-package")
(use-package 'reversi-package)

(reversi #'random-strategy #'random-strategy)

;; (reversi #'random-strategy #'human)

(defun mobility (player board)
  "The number of moves a player has."
  (length (legal-moves player board)))

;; (reversi #'human (alpha-beta-searcher 2 #'count-difference))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (let ((brd (get-board board)))
    (- (reduce #'+ (mapcar #'(lambda (row) (count player row)) brd))
       (reduce #'+ (mapcar #'(lambda (row) (count (opponent player) row)) brd)))))

;; (reversi #'human (alpha-beta-searcher 2 #'mobility))

(round-robin
 (list 
       (alpha-beta-searcher 2 #'count-difference)
       (alpha-beta-searcher 2 #'mobility)
       (alpha-beta-searcher 2 #'daniel)
       (alpha-beta-searcher 2 #'weighted-squares)
       (alpha-beta-searcher 2 #'modified-weighted-squares)
       )
 5
 10
 '(
   count-difference
   mobility
   daniel
   weighted-squares
   modified-weighted-squares)
 )

;; (reversi (alpha-beta-searcher 2 #'eval-fn) (alpha-beta-searcher 2 #'mobility))

(round-robin
 (list (alpha-beta-searcher 2 #'count-difference)
       (alpha-beta-searcher 2 #'mobility)
       #'random-strategy
       (alpha-beta-searcher 2 #'eval-fn1)
       (alpha-beta-searcher 2 #'eval-fn2)
       (alpha-beta-searcher 2 #'eval-fn3)
       (alpha-beta-searcher 2 #'eval-fn4)
       (alpha-beta-searcher 2 #'eval-fn5)
       (alpha-beta-searcher 2 #'eval-fn6)
       (alpha-beta-searcher 2 #'eval-fn7))
 100
 10
 '(count-dif
   mobility
   random-str
   eval-fn1
   eval-fn2
   eval-fn3
   eval-fn4
   eval-fn5
   eval-fn6
   eval-fn7)
 )