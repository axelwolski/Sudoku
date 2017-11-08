(in-package :sudoku)

(defun server-load-code (strategy-name)
  (setq *playing-strategy* strategy-name)
  (load-strategy strategy-name))

(defun foo (n)
;;  (sleep n)
  (error "erreur volontaire"))

(defun init-strategy (grid strategy)
  (handler-case 
      (with-timeout *time-out* 
	(strategy-init-standalone grid *playing-strategy*)
	(foo 10)
	)
    (error (c)
      (format t "Error in init-standalone: ~A~%" c)
      0)
    (timeout-condition (c)
      (format t "init-standalone too slow: ~A~%" c)
      0)))

(defgeneric play-strategy (game))
(defmethod play-strategy ((game game))
  (handler-case 
      (prog1
	  (let ((res (run-game game)))
	    (print (game-squares game))
	    (terpri)
	    (format t "~A~%" (if (zerop res) "Failed" "Passed"))
	    res))
    (strategy-error (c)
      (format t "Disqualified: ~A~%" (strategy-error-string c))
      0)
    (error (c)
      (format t "Error in main-standalone: ~A~%" c)
      0)))

;;; Playing
(defgeneric server-play (game))
(defmethod server-play ((game game))
  (let ((*server-game* t))
    (print (game-squares game))
    (init-strategy (initial-grid game))
    (play-strategy game)))

(defun server-game (grid strategy-name &optional (load t))
  (when load
    (server-load-code strategy-name))
  (let ((game (make-game grid *standalone-strategy*)))
    (setq *game* game)
    (server-play game)))

(defun server-games (grids strategy-name)
  (server-load-code strategy-name)
  (format *error-output* "~A~%" *playing-strategy*)
  (let ((*one-game* nil)
	(*interactive* nil)
	(nb-success
	  (loop
	    for grid in grids
	    for i from 1
	    sum (progn
		  (format t "Grid ~A: " i)
		  (server-game grid strategy-name nil)))))
    (format t "~A/~A~%" nb-success (length grids))))
