(in-package :sudoku)

(defgeneric strategy-package-name (strategy-name)
  (:documentation "name of the package of STRATEGY-NAME"))

(defmethod strategy-package-name ((strategy-name string))
  (string-upcase strategy-name))

(defgeneric strategy-package-symbol (strategy-name))
(defmethod strategy-package-symbol ((strategy-name string))
  (intern
   (strategy-package-name strategy-name)
   :keyword))

(defgeneric make-strategy-package (strategy-name))
(defmethod make-strategy-package ((strategy-name string))
  (let* ((package-name (strategy-package-name strategy-name))
	 (package (find-package package-name)))
    (when package
      (delete-package package))
    (make-package
     package-name
     :use '(#:common-lisp))))

(defgeneric strategy-package (strategy-name))
(defmethod strategy-package ((strategy-name string))
  (find-package (strategy-package-symbol strategy-name)))

;;; Compiling
(defgeneric make-strategy-filename (strategy-name extension))
(defmethod make-strategy-filename ((strategy-name string) (extension string))
  (format nil "~A~A.~A" *strategies-directory* strategy-name extension))

(defgeneric make-strategy-source-filename (strategy-name))
(defmethod make-strategy-source-filename ((strategy-name string))
  (format nil "~A/~A.lisp" *strategies-directory* strategy-name))

(defgeneric make-strategy-object-filename (strategy-name))
(defmethod make-strategy-object-filename ((strategy-name string))
  (format
   nil "~A/~A.fasl"
   *objects-directory*
   (string-downcase
    (strategy-package-name strategy-name))))

(defgeneric compile-strategy (strategy-name))
(defmethod compile-strategy ((strategy-name string))
  (let ((*package* (make-strategy-package strategy-name)))
    (compile-file
     (make-strategy-source-filename strategy-name)
     :output-file (make-strategy-object-filename strategy-name)
     :print nil)))

(defun compile-strategies (strategy-names)
  (loop
    for strategy-name in strategy-names
    do (compile-strategy strategy-name)))

(defun compile-all-strategies ()
  (compile-strategies (collect-strategies)))

;;; Loading
(defgeneric load-strategy (strategy-name))
(defmethod load-strategy ((strategy-name string))
  (let ((object (make-strategy-object-filename strategy-name)))
    (let ((*package* (strategy-package strategy-name)))
      (load object))))
