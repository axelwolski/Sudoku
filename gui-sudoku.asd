(cl:in-package #:common-lisp-user)

(asdf:defsystem :gui-sudoku
  :depends-on (:hunchentoot :sudoku :cl-who)
  :serial t
  :components
  ((:file "package-gui")
   (:file "gui")
   ))
