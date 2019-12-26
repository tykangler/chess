(in-package :chess.game)

(defclass move ()
   ((source 
     :initarg :src 
     :reader src)
    (destination 
     :initarg :dst 
     :reader dst))
   (:documentation "a move class representing a source position and a 
   destination position. it is undefined to set positions after 
   initialization. a valid position is, in regex terms, /[a-h][1-8]/.
   If passed values do not satisfy this condition, then the error 
   'invalid-move is thrown."))

(defun alpha-position (character)
   "returns the given character's index in the alphabet"
   (- (char-code character) (char-code #\a)))

(defun position-valid (pos)
   "PRIVATE. returns whether the passed string position (e6, a1) is valid.
   returns true if the rank of position is within characters 'a' and 'h', and
   file is within values 1 and 8"
   (declare (type string pos))
   (let ((horizontal (alpha-position (char pos 0)))
         (vertical (1- (digit-char-p (char pos 1)))))
        (and (>= horizontal 0) (< horizontal +board-dim+) 
             (>= vertical 0) (< vertical +board-dim+))))

(defmethod initialize-instance :around ((move move) &key src dst)
   (if (and (position-valid src) (position-valid dst))
       (call-next-method move :src src :dst dst)
       (error 'invalid-move :text "either source or destination position is invalid")))

(defun make-move (src dst)
   "creates a move instance. convenience function that acts as a constructor.
   Usage: (make-move \"a1\" \"a7\")"
   (make-instance 'move :src src :dst dst))

; (make-move :src "d3" "e4")