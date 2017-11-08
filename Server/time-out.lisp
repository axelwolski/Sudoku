(in-package :sudoku)

(define-condition timeout-condition () 
  ((timeout :initarg :timeout)))

(defmacro with-timeout (the-timeout call &optional (name "unknown"))
  (let ((timeoutp (gensym))
	(timeout (gensym))
	(exceptionp (gensym))
	(thread (gensym))
	(result (gensym)))
    `(let* ((,timeout (eval ,the-timeout))
	    (,timeoutp (list nil))
	    (,exceptionp nil)
	    (,thread 
	     (sb-thread::make-thread
	      (lambda (*standard-output*)
		(let ((*standard-output* *standard-output*))
		  (handler-case ,call
		    (condition (c) 
		      (setq ,exceptionp t)
		      (format t "raised ~S~%" c)))))
	      :name (format nil "~A-thread" ,name)
	      :arguments (list *standard-output*))))
       (if ,exceptionp
	   (progn
	     (sb-thread:terminate-thread ,thread)
	     (signal (make-condition 'strategy-error :strategy-error-string "crashed")))
	   (let ((,result
		  (sb-thread::join-thread ,thread :timeout ,the-timeout :default ,timeoutp)))
	   (if (eq ,result ,timeoutp)
	       (signal (make-condition 'timeout-condition :timeout ,timeout))
	       ,result))))))
