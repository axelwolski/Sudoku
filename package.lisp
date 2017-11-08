(cl:in-package #:common-lisp-user)

(defpackage #:sudoku (:use #:common-lisp)
  (:export
   #:*game*
   #:*sqrt-size*
   #:coor
   #:square
   #:squares
   #:game
   #:game-over
   #:game-undo ;; not mandatory
   #:game-with-grid
   #:init-game
   #:interactive-p ;; not mandatory
   #:coor-square
   #:make-square
   #:make-coor
   #:game-squares
   #:digit
   #:next-move
   #:possible-digits
   #:rcoor-to-coor
   #:zcoor-to-zone
   #:game-do
   #:assigned-p
   #:x-coor
   #:y-coor
   #:protected
   #:history ;; not mandatory
   #:run-game
   #:init-sudoku
   #:game-with-new-grid
   #:solved-p))

