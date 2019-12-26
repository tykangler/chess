(in-package :chess.game)

(defmacro define-chess-errors (conditions)
   `(progn ,@(mapcar (lambda (cond) 
                       `(define-condition ,cond (error)
                           ((text :initarg :text :reader text))
                           (:report (lambda (con stream) (format stream (text con))))))
                     conditions)))

;;; conditions

(define-chess-errors (invalid-move
                      invalid-position
                      undefined-arithmetic))   
