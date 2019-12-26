(in-package :chess.ai)

(defun max (a b)
   (cond ((and (eql a :infinity) (eql b :infinity)) (error 'undefined-arithemtic))
         ((and (eql a :infinity) (eql b :-infinity)) a)
         ((and (eql a :-infinity) (eql b :infinity)) b)
         ((and (eql a :-infinity) (eql b :-infinity)) (error 'undefined-arithmetic))
         (t (cl:max a b))))

(defun negamax (board max-height alpha beta eval-func moves-func)
   (let ((possible-moves (moves-func board))
         (node-score :-infinity)
         (best-move nil)
         (move-result 0))
        (if (or (= max-height 0) (null possible-moves))
            (funcall #'eval-func board)
            (loop for move in possible-moves
                  while (< alpha beta) do
                  (progn (setf move-result (- (negamax (move-piece move board)
                                                       (1- max-height)
                                                       (- beta) (- alpha) 
                                                       eval-func moves-func)))
                         (when (> move-result node-score)
                               (setf best-move move) 
                               (setf node-score move-result))
                         (setf alpha (max node-score alpha))))
        node-score)))

(defun generate-move (board look-ahead eval-func moves-func)
   "generates the optimal move using eval-func. Moves are generated
   using moves-func. moves-func should be a function taking in a single variable
   representing the play space and returning a sequence of possible moves. 
   look-ahead is an integer designating the max number of moves the AI will look ahead for. 
   board is the game play space which will be passed into moves-func"
   ())

