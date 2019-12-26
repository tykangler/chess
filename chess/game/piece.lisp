(in-package :chess.game)

(defconstant +pieces+ '(pawn rook knight bishop queen king))

(deftype chess-piece-color ()
   '(member :black :white))

(deftype chess-piece-type ()
   (cons 'member +pieces+))

(defconstant +num-piece-types+ (length +pieces+)
   "the number of playable piece types")

(defclass piece ()
   ((color 
     :initarg :color
     :reader color)
    (rep
     :initform #\X
     :reader rep)))

(defclass pawn (piece)
   ((rep :initform #\P)))

(defclass rook (piece)
   ((rep :initform #\R)))

(defclass knight (piece)
   ((rep :initform #\N)))

(defclass bishop (piece)
   ((rep :initform #\B)))

(defclass queen (piece)
   ((rep :initform #\Q)))

(defclass king (piece)
   ((rep :initform #\K)))

(defun make-piece (piece-type color)
   (declare (chess-piece-type piece-type)
            (chess-piece-color color))
   (make-instance piece-type :color color))

(defgeneric print-piece (piece &optional stream)
   (:documentation "prettily prints a piece to stream. Black pieces are represented with
   red and white pieces are represented with green. This should really only be used
   when printing to stdout since only the console can render colors"))

(defmethod print-piece (piece &optional (stream t))
   (format stream "~c[~am~c~c[0m"
                  #\esc
                  (if piece (if (eql (color piece) :black) "091" "092") "0")
                  (if piece (rep piece) #\space)
                  #\esc))

(defmethod print-object ((object piece) stream)
   (print-unreadable-object (object stream :type t :identity t)
      (format stream "~a ~a" (color object) (rep object))))