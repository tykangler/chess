(in-package :cl-user)

(defpackage :chess.game
   (:documentation "all things related to the gameplay")
   (:use :cl)
   (:shadow :position)
   (:export :move-piece
            :get-piece
            :place-piece
            :remove-piece
            :position-valid
            :register-move
            :print-board
            :name
            :player
            :person
            :computer
            :board
            :move
            :piece
            :pawn
            :rook
            :knight
            :bishop
            :queen
            :king
            :make-move
            :make-piece))

(defpackage :chess.ai
   (:documentation "AI functionality")
   (:use :cl)
   (:shadow :min :max)
   (:export))
