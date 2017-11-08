(in-package :sudoku)
(defvar *strategies-relative-directory*)
(setq *strategies-relative-directory* "Strategies")
(defvar *strategies-directory*)
(defvar *objects-directory*)

(defun default-directory ()
  "The default directory."
  (truename "."))

(defun strategies-directory ()
  (concatenate 'string
	       (namestring 
		sudoku-system::*sudoku-directory*)
	       *strategies-relative-directory*))
 
(setq *strategies-directory* (strategies-directory))
(setq *objects-directory* (format nil "~A/Objects" *strategies-directory*))

(defun collect-strategies ()
  (loop
     for strategy-name in (directory (format nil "~A/*.lisp" *strategies-directory*))
     collect (pathname-name strategy-name)))
