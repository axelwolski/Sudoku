(in-package :sudoku)

(defun test-alien-strategy-on-grids (strategy-name &optional (compile t))
  (let ((*strategies-directory* ".")
	(*objects-directory* "."))
    (load-grids)
    (when compile
      (compile-strategy strategy-name))
    (server-games *grids* strategy-name)))

(defun test-one-grid (strategy grid)
;;    (format *standard-output* "strategy name (without the .lisp extension): ")
;;    (force-output *standard-output*)
   (let ((*strategies-directory* ".")
	 (*objects-directory* ".")
	 (*display* t))
     (compile-strategy strategy)
     (server-game grid strategy)))

(defun test-all-grids (strategy)
;;    (format *standard-output* "strategy name (without the .lisp extension): ")
;;    (force-output *standard-output*)
    (test-alien-strategy-on-grids strategy))
