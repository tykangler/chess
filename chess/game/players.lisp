(in-package :chess.game)

(defparameter *num-players* 0
   "the number of players in the game")

(defclass player ()
   ((id :initarg :id :reader id)
    (moves :initarg :moves :accessor moves)
    (color :initarg :color :accessor color)
    (skill :initarg :skill :accessor skill)
    (high-score :initarg :high-score :accessor high-score)))

(defclass person (player)
   ((name :initarg :name)))

(defclass computer (player) ())

(defgeneric register-move (move player)
   (:documentation "registers a move from src-pos to dst-pos in person.
   This move is added to the move history of the player."))

(defgeneric name (player)
   (:documentation "returns a string used to id the player"))

(defgeneric (setf name) (new-name player)
   (:documentation "sets the string used to id the player"))

(defmethod register-move (move-played (player player))
   (push move-played (moves player)))

(defmethod name ((player person))
   (slot-value player 'name))

(defmethod name ((player computer))
   (slot-value player 'id))

(defmethod (setf name) (new-name (player person))
   (setf (slot-value player 'name) new-name))

(defmethod (setf name) (new-id (player computer))
   (setf (slot-value player 'id) new-id))

