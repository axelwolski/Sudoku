(in-package :sudoku)
(defvar *sqrt-size* 3 "side of the side of a zone")
(defvar *size* (* *sqrt-size* *sqrt-size*))
(defvar *nb-squares* (* *size* *size*))
(defvar *game* nil "the current instance of a game")

;; coordinates
(defgeneric x-coor (coor)
  (:documentation "x slot of coor"))

(defgeneric y-coor (coor)
  (:documentation "y slot of coor"))

(defclass coor ()
  ((x :initarg :x :reader x-coor :type integer)
   (y :initarg :y :reader y-coor :type integer))
  (:documentation "class for coordinates in the squares grid"))

(defgeneric make-coor (x y)
  (:documenation "instance of a coor [x,y]"))

(defgeneric zcoor-to-zone (zcoor)
  (:documentation
   "zone corresponding to the zone coordinate ZCOOR"))

(defgeneric rcoor-to-coor (zone rcoor)
  (:documentation "from zone and relative coor in zone to absolute coor"))

(defgeneric zcoor-to-zone (zcoor)
  (:documentation
   "zone corresponding to the zone coordinate ZCOOR"))

(defgeneric assigned-p (square)
  (:documentation "T if the digit slot a striclty positive"))

(defgeneric digit (square)
  (:documentation 
   "assigned digit of square (0 if unassigned)"))

(defgeneric possible-digits (square)
  (:documentation 
   "list of possible digits remaining in SQUARE"))

(defgeneric protected (square) 
  (:documentation 
   "T if square was originally filled in the grid"))

(defclass square ()
  ((coor :initarg :coor :reader coor) 
   (possible-digits :initform (copy-list *digits*)
		  :initarg :possible-digits :accessor possible-digits)
   (digit :accessor digit :initarg :digit :initform 0)
   (protected :accessor protected :initarg :protected :initform nil))
  (:documentation "one square of the squares"))

(defgeneric make-square (coor &optional digit)
  (:documentation "creates a square containing coor and digit"))

(defclass squares ()
  ((squares-array :initarg :squares-array :reader squares-array :type array :initform (make-squares-array *size*))
   (to-fill :initform *nb-squares* :accessor to-fill))
  (:documentation "class containing a two dimensional array of instances of the square class \
                   and the number of squares left to fill"))

(defgeneric coor-square (squares coor)
  (:documentation "the square at COOR in SQUARES"))

(defvar *game* nil "the current instance of a game")

(defgeneric initial-grid (game)
  (:documentation "the initial grid of GAME"))

(defgeneric game-squares (game)
  (:documentation "the squares of GAME"))

(defclass game ()
  ((game-squares :accessor game-squares :initarg :game-squares)
   (initial-grid :reader initial-grid :initarg :initial-grid))
  (:documentation "class for game instances"))

(defgeneric game-over (game)
  (:documentation "if the game is over (either won or lost"))

(defgeneric init-game (game)
  (:documentation "initializes GAME with its initial-grid"))

(defgeneric game-with-new-grid (&optional strategy)
  (:documentation "instance of game with a grid and STRATEGY"))

(defgeneric init-sudoku ()
  (:documentation "to initialize whatever you want to initialize"))

(defgeneric game-do (game square)
  (:documentation 
   "plays coor/digit of square in the coor-square in squares of GAME"))
