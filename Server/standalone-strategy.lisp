(in-package :sudoku)

(defvar *playing-strategy*)
(defvar *display* nil)

(defvar *time-out*)
(setf *time-out* 0.015)

(defgeneric find-strategy-symbol (symbol-name strategy-name))
(defmethod find-strategy-symbol ((symbol-name string) (strategy-name string))
  (find-symbol
   symbol-name
   (strategy-package strategy-name)))

(defun init-standalone-symbol (strategy)
  (find-strategy-symbol "INIT-STANDALONE" strategy))

(defun main-standalone-symbol (strategy)
  (find-strategy-symbol "MAIN-STANDALONE" strategy))

(defun strategy-init-standalone (grid strategy)
  (funcall (init-standalone-symbol strategy) grid))

(defun strategy-main-standalone (strategy)
  (funcall (main-standalone-symbol strategy)))

(defun strategy-standalone ()
  (let* ((timeoutp (list nil))
	 (exceptionp nil)
	 (thread
	   (sb-thread::make-thread
	    (lambda (*standard-output*)
	      (let ((*standard-output* *standard-output*))
		(handler-case (strategy-main-standalone *playing-strategy*)
		  (condition () ;(c)
		    (setq exceptionp t)
;;		    (format t "raised ~S~%" c)
))))
	    :name "strategy-thread"
	    :arguments (list *standard-output*))))
    (if exceptionp
	(progn
	  (sb-thread:terminate-thread thread)
	  (signal (make-condition 'strategy-error :strategy-error-string "crashed")))
	(multiple-value-bind (x y d)
	    (sb-thread::join-thread thread :timeout *time-out* :default timeoutp)
	  (if (eq x timeoutp)
	      (signal (make-condition 'strategy-error :strategy-error-string "too slow"))
	      (make-square (make-coor x y) d))))))

(defgeneric standalone-fun (squares))
(defmethod standalone-fun ((squares squares))
  (let ((play (strategy-standalone)))
    (when *display*
      (format t "~A~%" play))
    play))

(defvar *standalone-strategy*
  (make-strategy "Standalone" #'standalone-fun))

(add-strategy *standalone-strategy*)
