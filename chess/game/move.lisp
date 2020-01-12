(in-package :chess.game)

(defclass move () ())

(defclass basic-move (move)
   ((source 
     :initarg :src 
     :reader src)
    (destination 
     :initarg :dst 
     :reader dst))
   (:documentation "a move class representing movement from a 
   source position to a destination position. it is undefined to set 
   positions after initialization. a valid position is, in regex terms, 
   /[a-h][1-8]/. If passed values do not satisfy this condition, 
   then the error 'invalid-position' is thrown."))

(defclass special-move (move) ())

(defclass promotion (basic-move special-move) 
   ((promote
     :initarg :promote
     :accessor promote))
   (:documentation "a move class representing a piece's promotion to
   another piece class. The piece at pos will be promoted to the piece
   designated by promote."))

(defclass castle (special-move) 
   ((king-pos
     :initarg :king
     :accessor king)
    (rook-pos
     :initarg :rook
     :accessor rook)))

(defun alpha-position (character)
   "returns the given character's index in the alphabet"
   (- (char-code character) (char-code #\a)))

(defun make-basic-move (src dst)
   "creates a move instance. acts as a constructor.
   Usage: (make-basic-move \"a1\" \"a7\")"
   (when (and (position-valid src) (position-valid dst))
         (make-instance 'basic-move :src src :dst dst)))

(defun make-promotion (&key pos promote)
   "creates an instance of a piece promotion")
