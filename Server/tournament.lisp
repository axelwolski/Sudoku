(in-package :sudoku)

(defgeneric fill-tournament (strategies))
(defmethod fill-tournament  ((strategies vector))
  (loop
     with nb-strat = (length strategies)
     for i from 0 below nb-strat
     do (let ((s (aref strategies i)))
	  (setf (cdr s) 
		(server-games *grids* (car s))))
    finally (return strategies)))

(defgeneric run-tournament (strategies))
(defmethod run-tournament ((strategies vector))
  (sort
   strategies #'< :key #'cdr))

(defmethod run-tournament ((strategies list))
  (sort 
   (fill-tournament
    (make-array (length strategies)
		:initial-contents
		(mapcar (lambda (s) (cons s nil))
			strategies)))
   #'<
   :key #'cdr))

(defun global-tournament ()
  (load-grids)
  (run-tournament (collect-strategies)))

