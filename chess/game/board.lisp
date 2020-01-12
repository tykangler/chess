;;;; Low Level Chess Board and Related Function Definitions

;;; A spot is an index in the chess board

(in-package :chess.game)

;;; board and auxiliary definitions

(defconstant +board-dim+ 8
   "the dimensions of one side of a board")
(defconstant +left+ -1) downdiag -9 updiag 7
(defconstant +right+ 1) downdiag -7 updiag 9
(defconstant +up+ +board-dim+)
(defconstant +down+ (- +board-dim+))

;; to compute diagonal step values, add the horizontal and vertical directions

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

;;; helpers

(defun position-valid (pos)
   "returns whether the passed string position (e6, a1) is valid.
   returns true if the rank of position is within characters 'a' and 'h', and
   file is within values 1 and 8"
   (declare (type string pos))
   (let ((horizontal (alpha-position (char pos 0)))
         (vertical (1- (digit-char-p (char pos 1)))))
        (and (>= horizontal 0) (< horizontal +board-dim+) 
             (>= vertical 0) (< vertical +board-dim+))))

(defun pos (string-pos)
   "converts a conventional chess position into an index for the spot array"
   (declare (type string string-pos))
   (when (position-valid string-pos)
         (let ((horizontal (alpha-position (char string-pos 0)))
               (vertical (1- (digit-char-p (char string-pos 1)))))
              (+ horizontal (* vertical +board-dim+)))))

(defun print-row-divider (stream)
   (let ((*print-pretty* nil))
        (format stream "~c" #\+)
        (dotimes (i +board-dim+)
           (format stream "~a" "---+"))))

(defun spot-in-row (spot row-no)
   "returns t if spot is in row-no of a chess board with dimensions +board-dim+"
   (and (>= spot (* row-no +board-dim+))
        (< spot (* (1+ row-no) +board-dim+))))

(defun spot-in-col (spot col-no)
   "returns t if spot is in col-no of a chess board with dimensions +board-dim+"
   (= col-no (mod spot +board-dim+)))

(defun out-of-bounds (spot)
   "returns t if spot is out of bounds a chess board with dimensions +board-dim+"
   (or (>= spot (expt +board-dim+ 2)) (< spot 0)))

;;; generics

(defgeneric get-piece (target-spot chess-board)
   (:documentation "retrieves the piece at the passed position in the passed chess-board"))

(defgeneric move-piece (move chess-board) 
   (:documentation "moves the piece at src-pos to dst-pos in chess-board"))

(defgeneric legal-moves (spot chess-board)
   (:documentation "returns a list of possible moves at position in chess board"))

(defgeneric spot-capturable (spot piece chess-board)
   (:documentation "returns t if piece can capture an opposing piece at spot
   in chess-board. if no piece exists at spot, returns nil. if a piece exists at spot,
   but is of the same side (e.g. has the same color), then returns nil."))

(defgeneric print-board (chess-board &optional stream)
   (:documentation "prints a playable chess board with black represented by 
   red and white by green"))

(defgeneric legal-piece-moves (spot piece chess-board)
   (:documentation "PRIVATE. returns a list of possible moves at position for 
   piece in chess-board"))

;;; generic implementations

(defmethod get-piece (target-spot (chess-board board))
   (elt (spots chess-board) target-spot))

(defmethod move-piece ((move basic-move) (chess-board board))
       (let ((all-spots (copy-seq (spots chess-board)))
             (src-spot (src move))
             (dst-spot (dst move)))
            (if (elt all-spots src-spot)
                (progn (setf (elt all-spots dst-spot) (elt all-spots src-spot)
                             (elt all-spots src-spot) nil)
                       (make-instance 'board :spots all-spots))
                (error 'invalid-position :text (format nil "no piece at given source")))))

(defmethod move-piece ((move promotion) (chess-board board))
   (let ((updated-board (call-next-method move chess-board)))
        (setf (elt updated-board (dst move)) (promote move))))

(defmethod legal-moves :before (pos (chess-board board)) 
   (when (null (elt (spots chess-board) pos))
         (error 'invalid-position :text "no piece at passed position")))

(defmethod legal-moves (spot (chess-board board))
   (legal-piece-moves spot (get-piece spot) chess-board))

(defmethod spot-capturable (spot piece (chess-board board))
   (unless (out-of-bounds spot)
           (let ((found-piece (get-piece spot chess-board)))
                (and found-piece (not (eql (color found-piece) (color piece)))))))

(defmethod print-board ((chess-board board) &optional (stream t))
   (let ((*print-pretty* nil)
         (all-spots (spots chess-board)))
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

(defmacro linear-moves ((var start end step) piece chess-board moves)
   "if a linear pattern can be found from a piece's movement options, then this macro 
   constructs a list of all possible moves following a certain direction. var is
   the variable used to traverse a direction. start is the variable's starting position, end
   is the end condition of the variable's traversal, and step is the amount of positions to
   step by."
   (let ((last-possible-dst (gensym)))
        `(let ((,last-possible-dst (do ((,var ,start (+ ,start ,step)))
                                        ((or ,end (get-piece ,var ,chess-board)) ,var)
                                        (push (make-basic-move ,start ,var) ,moves))))
              (when (spot-capturable ,last-possible-dst ,chess-board)
                    (push (make-basic-move ,start ,last-possible-dst) ,moves)))))

(defmethod legal-piece-moves (spot (piece pawn) (chess-board board))
   (let ((spot-front (+ spot +board-dim+))
         (spot-front-left (+ spot +up+ +left+))
         (spot-front-right (+ spot +up+ +right+))
         moves)
        (unless (spot-in-row spot (1- +board-dim+))
                (unless (get-piece spot-front chess-board)
                        (push (make-promotion spot spot-front) moves))
                (when (spot-capturable spot-front-left piece chess-board)
                      (push (make-basic-move spot spot-front-left) moves))
                (when (spot-capturable spot-front-right piece chess-board)
                      (push (make-basic-move spot spot-front-right) moves)))))

(defmethod legal-piece-moves (spot (piece rook) (chess-board board))
   (let ((next-mult (+ spot (- +board-dim+ (mod spot +board-dim+))))
         (rank (mod spot +board-dim+))
         (down-file (- (1- +board-dim+) (floor spot +board-dim+)))
         (moves nil))
        ; right
        (linear-moves (possible-dst spot 
                       (= possible-dst (1- next-mult))
                       +right+) 
                      piece chess-board moves)
        ; down
        (linear-moves (possible-dst spot 
                       (= possible-dst (mod spot +board-dim+)) 
                       +down+)
                      piece chess-board moves)
        ; left
        (linear-moves (possible-dstspot 
                       (= possible-dst (- spot (mod spot +board-dim+))) 
                       +left+)
                      piece chess-board moves)
        ; up
        (linear-moves (possible-dst spot 
                       (= possible-dst (+ spot (* +board-dim+ down-file)))
                       +up+)
                      piece chess-board moves)))

(defmethod legal-piece-moves (spot (piece bishop) (chess-board board))
   (let ((last-row-col (1- +board-dim+))
         (first-row-col 0)
         (moves nil))
        ; right-up
        (linear-moves (possible-dst spot
                       (or (spot-in-row possible-dst last-row-col)
                           (spot-in-col possible-dst last-row-col))
                       (+ +right+ +up+))
                      piece chess-board moves)
        ; right-down
        (linear-moves (possible-dst spot
                       (or (spot-in-row possible-dst first-row-col)
                           (spot-in-col possible-dst last-row-col))
                       (+ +right+ +down+))
                      piece chess-board moves)
        ; left-down
        (linear-moves (possible-dst spot
                       (or (spot-in-row possible-dst first-row-col)
                           (spot-in-col possible-dst first-row-col))
                       (+ +left+ +down+))
                      piece chess-board moves)
        ; left-up
        (linear-moves (possible-dst spot
                       (or (spot-in-row possible-dst last-row-col)
                           (spot-in-col possible-dst first-row-col))
                       (+ +left+ +up+))
                      piece chess-board moves)))

(defmethod legal-piece-moves (spot (piece knight) (chess-board board))
   (let ((moves nil)
         (all-ends (list (+ (* +up+ 2) +right+)
                         (+ (* +up+ 2) +left+)
                         (+ (* +left+ 2) +up+)
                         (+ (* +left+ 2) +down+)
                         (+ (* +down+ 2) +right+)
                         (+ (* +down+ 2) +left+)
                         (+ (* +right+ 2) +up+)
                         (+ (* +right+ 2) +down+))))
        (when (spot-capturable (+ spot (elt all-ends 0)) chess-board)
              (push (make-basic-move spot)))))

(defmethod legal-piece-moves (spot (piece queen) (chess-board board))
   (let ((moves nil))))

(defmethod legal-piece-moves (spot (piece king) (chess-board board))
   (let ((moves nil))))

(defmethod print-object ((object board) stream)
   (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a" (spots object))))

(defvar *chessboard* (make-board :initial))