;;;; Low Level Chess Board and Related Function Definitions

;;; A spot is an index in the chess board

(in-package :chess.game)

;;; board and auxiliary definitions

(defconstant +board-dim+ 8
   "the dimensions of a single side of a board")

(defclass board ()
   ((spots 
     :initarg :spots 
     :accessor spots)))

(defun make-board (contents)
   (make-instance 'board :spots 
      (if (eql contents :initial)
          (let ((specials '(rook knight bishop queen king bishop knight rook))
                (pawns (make-list +board-dim+ :initial-element 'pawn)))
               (flet ((white-piece (piece) (make-piece piece :white))
                      (black-piece (piece) (make-piece piece :black)))
                     (concatenate 'vector
                                  (mapcar #'white-piece specials)
                                  (mapcar #'white-piece pawns)
                                  (make-list (* +board-dim+ 4))
                                  (mapcar #'black-piece pawns)
                                  (mapcar #'black-piece specials))))
          contents)))

;;; generics

(defgeneric get-piece (target-pos chess-board)
   (:documentation "retrieves the piece at the passed position in the passed chess-board"))

(defgeneric place-piece (piece target-pos chess-board)
   (:documentation "places a piece at target-pos in chess-board"))

(defgeneric remove-piece (target-pos chess-board)
   (:documentation "removes the piece at target-pos in chess-board. returns the removed piece"))

(defgeneric move-piece (move chess-board) ; fix no error checking needed
   (:documentation "moves the piece at src-pos to dst-pos in chess-board"))

(defgeneric print-board (chess-board &optional stream)
   (:documentation "prints a playable chess board with black represented by 
   red and white by green"))

;;; helpers

(defun position->spot (pos)
   "converts a conventional chess position into an index for the spot array"
   (declare (type string pos))
   (let ((horizontal (alpha-position (char pos 0)))
         (vertical (1- (digit-char-p (char pos 1)))))
        (+ horizontal (* vertical +board-dim+))))

(defun print-row-divider (stream)
   (let ((*print-pretty* nil))
        (format stream "~c" #\+)
        (dotimes (i +board-dim+)
           (format stream "~a" "---+"))))

;;; generic implementations

(defmethod get-piece (target-pos (chess-board board))
   (let ((target-spot (position->spot target-pos))
         (all-spots (copy-seq (spots chess-board))))
        (elt all-spots target-spot)))

(defmethod place-piece (piece target-pos (chess-board board))
   (if (position-valid target-pos)
       (let ((all-spots (copy-seq (spots chess-board)))
             (target-spot (position->spot target-pos)))
            (setf (elt all-spots target-spot) piece)
            (make-instance 'board :spots all-spots))
       (error 'invalid-position 
              :text (format nil "~a out of the range of a valid position" target-pos))))
(defmethod remove-piece (target-pos (chess-board board))
   (place-piece nil target-pos chess-board))

(defmethod move-piece (move (chess-board board))
       (let ((all-spots (copy-seq (spots chess-board)))
             (src-spot (position->spot (src move)))
             (dst-spot (position->spot (dst move))))
            (if (elt all-spots src-spot)
                (progn (setf (elt all-spots dst-spot) (elt all-spots src-spot)
                             (elt all-spots src-spot) nil)
                       (make-instance 'board :spots all-spots))
                (error 'invalid-position :text (format nil "no piece at ~a" (src move))))))

(defmethod print-board ((chess-board board) &optional (stream t))
   (let ((*print-pretty* nil)
         (all-spots (copy-seq (spots chess-board))))
        (print-row-divider stream)
        (fresh-line stream)
        (loop for i from (1- +board-dim+) downto 0
              for row-start = (* i +board-dim+)
              do (format stream "~a" "|")
              do (loop for rank from row-start below (+ row-start +board-dim+)
                       do (progn (format stream "~c" #\space)
                                 (print-piece (elt all-spots rank) stream)
                                 (format stream "~c|" #\space)))
              do (progn (fresh-line stream)
                        (print-row-divider stream) 
                        (fresh-line stream)))))

(defmethod print-object ((object board) stream)
   (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a" (spots object))))

(defvar *board* (make-board :initial))