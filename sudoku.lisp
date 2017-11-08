;;; Projet de Programmation 3 / Licence 3 / Semestre 5
;;; Groupe A3
;;; Gael Trottier, Axel Wolski

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Jouer au Sudoku ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Initialise le sudoku (instruction du jeu).
(defun start-sudoku(grille)
  (format t "~%~%|####################################|~%|#########      SUDOKU     ##########|~%|####################################|~%~%~%Le but du jeu est de remplir les cases vides avec des chiffres allant de 1 à 9.~%Ils faut toujours veiller à ce qu'un même chiffre ne figure qu'une seule fois: ~%   - Par colonne. ~%   - Par ligne. ~%   - Par carré de neuf cases.~%La victoire se traduit par l'absence de cases vides!~%~%                 GLHF!~%~%|####################################|~%|######### Début du sudoku ##########|~%|####################################|~%~%")
  (validite-grille grille)
  (sudoku grille )
  )


;; Permet de jouer au sudoku.
(defun sudoku (grille)
  (print-grille grille)
  (if (eq 0 (case-vide grille))
      (progn(format t "~%~%Félicitation GG~%~%")
	    ())
      (progn(format t "       Q pour quitter~%")
	    (format t " Nombre de cases vides: ~D~%" (case-vide grille))
	    (format t "Colonne? ")
	    (setf j (read))
	    (setf j (conversionCharToInt j))
	    (if (eq 10 j)
		()
		(progn(format t "Ligne? ")
		      (setf i (read))
		      (if (eq nil (verifier-coordonnée grille j (- i 1)))
			  (progn (sudoku-error 0 grille)
				 ())
			  )
		      (format t "Valeur? ")
		      (setf v (read))
		      (if (eq nil (verifier-valeur grille j (- i 1) v))
			  (progn (sudoku-error 1 grille)
				 ()
			  ))
		      (inserer-valeur grille j (- i 1) v)
		      (sudoku grille)
		      )
		)
	    )
      )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Fonctions au bon fonctionnement du sudoku et pour sa création ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Lorsque nous jouons au sudoku nous indiquons au jeu la colonne selectionné
;; avec des caractères allant de A à I, il nous faut les convertir en intiger
;; afin de pouvoir nous déplacer corectement dans la grille.
;; Cette fonction permet de realiser cette conversion. 
(defun conversionCharToInt (j)
  (cond
    ((eq j 'A)
     (setf j 0))
    ((eq j 'B)
     (setf j 1))
    ((eq j 'C)
     (setf j 2))
    ((eq j 'D)
     (setf j 3))
    ((eq j 'E)
     (setf j 4))
    ((eq j 'F)
     (setf j 5))
    ((eq j 'G)
     (setf j 6))
    ((eq j 'H)
     (setf j 7))
    ((eq j 'I)
     (setf j 8))
    ((eq j 'Q)
     (setf j 10))
    )
  )

;; Permet de renvoyer le message d'erreur en cas de mauvaise coordonnées ou mauvaise valeur.
(defun sudoku-error (i grille)
  (if (= i 0)
      (format t "~%~% La coordonnée choisie n'est pas correcte~%~%")
      (format t "~%~% La valeur choisie est impossible ici~%~%"))
  (sudoku grille)
  )

;; Cette fonction permet de verifier le nombre de cases vides.
(defun case-vide (grille)
  (setf *comp* 0)
  (loop for i from 0 to 8
	do
	   (loop for j from 0 to 8
		 do
		    (if (eq (aref grille i j) 0)
			(setf *comp* (+ 1 *comp*))
			)
		 )
	)
  *comp*
  )

;; Vérifie si une grille est valide.
(defun validite-grille (grille)
  (assert (eq (array-rank grille) 2 ))
  (assert (eq (array-total-size grille) 81))
  )

;; Créer une grille définie.
(defun creer-grille ()
  (setf *grid* #2A((1 0 0 0 0 4 0 0 5)
		   (0 0 0 9 5 0 0 8 0)
		   (0 0 0 0 0 3 0 9 0)
		   (0 0 5 0 0 2 0 0 4)
		   (0 0 1 0 6 0 7 0 0)
		   (7 0 0 3 0 0 2 0 0)
		   (0 6 0 5 0 0 0 0 0)
		   (0 8 0 0 1 6 0 0 0)
		   (5 0 0 2 0 0 0 0 7))))

;; Réinitialise la grille grid.
(defun reinit-grille (grid)
  (setf *g* #2A((1 0 0 0 0 4 0 0 5)
		(0 0 0 9 5 0 0 8 0)
		(0 0 0 0 0 3 0 9 0)
		(0 0 5 0 0 2 0 0 4)
		(0 0 1 0 6 0 7 0 0)
		(7 0 0 3 0 0 2 0 0)
		(0 6 0 5 0 0 0 0 0)
		(0 8 0 0 1 6 0 0 0)
		(5 0 0 2 0 0 0 0 7)))
  (loop for i from 0 to 8
     do
       (loop for j from 0 to 8
	  do
	    (setf (aref grid i j) (aref *g* i j))))
)


;; Créer une grille définie dans les paramètres.
(defun init-standalone (grid)
  (validite-grille grid)
  (format t "~%~%La grille créée se nomme *grid*~%~%")
   (loop for i from 0 to 8
     do
       (loop for j from 0 to 8
	  do
	    (setf (aref *grid* i j) (aref grid i j))))
)


;; Permet d'afficher la grille de sudoku.
(defun print-grille(grille)
  (format t " % | A B C | D E F | G H I |~%")
  (loop for i from 0 to 8
       do
       (loop for j from 0 to 8
	    do
	      (if (and (< 0 j) (= 0 (mod j 3)))
		  (format t " |"))
	      (if (and (= 0 (mod j 9)) (not (= i 0)))
		  (format t " |~%"))
	      (if (and (= 0 (mod i 3)) (= 0 j))
		  (format t "****************************~%"))
	      (if (= 0 j)
		  (format t " ~A |" (+ i 1)))
	      (if (= 0 (aref grille i j))
		  (format t "  ")
		(format t " ~A" (aref grille i j)))
	    )
       )
  (format t " |~%****************************~%")
  )


;; Place une nouvelle valeur dans la grille.
(defun inserer-valeur (grille j i v)
  (setf (aref grille i j) v)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Les fonctions de vérification ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verifier si les coordonnées choisie renvoie bien à une case vide, 
;; sinon c'est une coordonnée fausse.
(defun verifier-coordonnée (grille j i)
  (eq 0 (aref grille i j))
  )

;; Permet de savoir si la valeur que nous avons choisie et possible.
(defun verifier-valeur(grille j i val)
  (and (verifier-valeur-case grille j i val) (verifier-valeur-rectiligne grille j i val ))
  )

;; Verifie dans quels cas nous nous trouvons dans le sudoku, 
;; et si dans le carré dans lequel on veux jouer, la valeur n'est pas déjà presente.
(defun verifier-valeur-case(grille j i val)
  (cond
    ((and (< i 3) (< j 3))
     ;;;1er carré;;;
     (entree-dans-le-carre grille val 0 0)
     )
    
    ((and (and (< i 6) (> i 2)) (< j 3))
     ;;;2nd carré;;;
     (entree-dans-le-carre grille val 3 0)
     )
    
    ((and (> i 5) (< j 3))
     ;;;3ème carré;;;
     (entree-dans-le-carre grille val 6 0)
     )
    
    ((and (< i 3) (and (< j 6)(> j 2)))
     ;;;4ème carré;;;
     (entree-dans-le-carre grille val 0 3)
     )
    
    ((and (and (< i 6) (> i 2)) (and (< j 6)(> j 2)))
     ;;;5ème carré;;;
     (entree-dans-le-carre grille val 3 3)
     )
    
    ((and (> i 5) (and (< j 6)(> j 2)))
     ;;;6ème carré;
     (entree-dans-le-carre grille val 6 3)
     )
    
    ((and (< i 3) (> j 5))
     ;;;7ème carré;;;
     (entree-dans-le-carre grille val 0 6)
     )
    
    ((and (and (< i 6) (> i 2)) (> j 5))
     ;;;8ème carré;;;
     (entree-dans-le-carre grille val 3 6)
     )
    
    ((and (> i 5) (> j 5))
     ;;;9ème carré;
     (entree-dans-le-carre grille val 6 6)
     )
    
    )
  )


;; Permet d'éviter la répétition de code dans la fonction precedente,
;; elle sert à verifier la valeur dans le carré selectionné. 
(defun entree-dans-le-carre(grille val a b)
  (setf *varrandom* 0)
  (loop for x from a to (+ 2 a)
	do
	   (loop for y from b to (+ 2 b)
		 do
		    (if (eq val (aref grille a b))
		     (setf *varrandom* 1)
		     )))
  (if (eq 1 *varrandom*)
      nil
      t
      )
  )


;; Verifie si la valeur que l'on souhaite écrire dans la case vide n'est pas déjà presente
;; sur la meme colonne et sur la meme ligne.
(defun verifier-valeur-rectiligne (grille j i val)
  (setf *varrandom* 0)
  (loop for a from 0 to 8
	do
	   (if (eq val (aref grille a j))
	       (setf *varrandom* 1) 
	       )
	)
  (loop for b from 0 to 8
	do
	   (if (eq val (aref grille i b))
	       (setf *varrandom* 1) 
	       )
	)
  (if (eq 1 *varrandom*)
      nil
      t
      )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;       Partie IA       ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;  Stratégie Aléatoire  ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Cette strat n'est pas viable: pour chaque case vide, elle fera des coups aléatoires, allant même jusqu'à une défaite.
(defun start-strat-aleatoire (grid)
  (setf ligne 0)
  (setf colonne 0)
  (strat-aleatoire grid ligne colonne)
  (print-grille grid)
  (format t " Nombre de cases vides: ~D~%" (case-vide grid))
  (if (eq 0 (case-vide grid))
      (format t " BRAVO, la stratégie aléatoire a fonctionné !~%")
      (format t " La stratégie aléatoire n'a pas résolue le sudoku !!~%"))
)

;; VOIR POUR FAIRE AVEC DES COND ET NON DES IF
(defun strat-aleatoire(grid x y)
  (if (eq 0 (aref grid x y))
      (if (not (eq 0 (length (liste-de-possibilite grid x y))))
	   (progn (setf *random* (random (length (liste-de-possibilite grid x y))))
		  (setf (aref grid x y) (nth *random* (liste-de-possibilite grid x y)))      ;;nth n list => prend le n element dans la list
		  )
	   )
      )
  (if (= x 8)
      (if (= y 8)
	  ()
	  (strat-aleatoire grid 0 (+ y 1))
	  )
      (strat-aleatoire grid (+ x 1) y)
      )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Intelligence Artificielle ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;   Version fonctionnelle 9x9  ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun start-ia (grille)
  (sudoku-ia grille 0 0)
  (print-grille grille)
  (format t " Nombre de cases vides: ~D~%" (case-vide grille))
  (if (eq 0 (case-vide grille))
      (format t " BRAVO, l'IA a résolue le SUDOKU !~%")
      (format t " L'IA n'a malheureusement pas résolue le SUDOKU !!~%"))
)

(defun sudoku-ia (grille x y)
  (cond
    ((eq x 9)
     grille)
    ((eq y 9)
     (sudoku-ia grille (+ x 1) 0))
    ((not (eq 0 (aref grille x y)))
     (sudoku-ia grille x (+ y 1)))
    (t (dolist (possibilite (liste-de-possibilite grille x y) (setf (aref grille x y) 0))
	 (setf (aref grille x y) possibilite)
	 (when (eq grille (sudoku-ia grille x (+ y 1)))
	   (return grille)
	   )
	 )
       )
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;   Version verifiant ligne, colonne, carré   ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun start-test-sudoku(grille)
  (test-sudoku grille 0 0)
  (print-grille grille)
  )

(defun test-sudoku(grille x y)
  (cond
    ((eq (case-vide grille) 0)
     grille)
    ((eq x 9)
     (test-sudoku grille 0 0))
    ((eq y 9)
     (test-sudoku grille (+ x 3) 0))
    ((and (eq x 0) (eq y 0))
     ;;;1er carré;;;
     (entree-dans-le-carre-ia grille 0 0)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 3) (eq y 0))
     ;;;2nd carré;;;
     (entree-dans-le-carre-ia grille 3 0)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 6) (eq y 0))
     ;;;3ème carré;;;
     (entree-dans-le-carre-ia grille 6 0)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 0) (eq y 3))
     ;;;4ème carré;;;
     (entree-dans-le-carre-ia grille 0 3)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 3) (eq y 3))
     ;;;5ème carré;;;
     (entree-dans-le-carre-ia grille 3 3)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 6 ) (eq y 3))
     ;;;6ème carré;
     (entree-dans-le-carre-ia grille 6 3)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 0) (eq y 6))
     ;;;7ème carré;;;
     (entree-dans-le-carre-ia grille 0 6)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 3) (eq y 6))
     ;;;8ème carré;;;
     (entree-dans-le-carre-ia grille 3 6)
     (test-sudoku grille x (+ y 3))
     )
    
    ((and (eq x 6) (eq y 6))
     ;;;9ème carré;
     (entree-dans-le-carre-ia grille 6 6)
     (test-sudoku grille x (+ y 3))
     )
    )
  )

(defun entree-dans-le-carre-ia (grille x y)
  (setf *list* '())
  (loop for a from x to (+ x 2)
	do
	   (loop for b from y to (+ y 2)
		 do
		    (if (not (eq 0 (aref grille a b)))
			(setf *list* (append *list* (list '(0) ) ) )
			(setf *list* (append *list* (list (liste-de-possibilite grille a b))))
			)
		 )
	)
  (loop for i from 1 to 9
	do
	   (setf cmpt 0)
	   (loop for j from 0 to 8
		 do
		    (if (member i (nth j *list*))
			(progn (setf cmpt (+ 1 cmpt))
			       (setf elem j)
			       )
			)
		 )
	   (if (= cmpt 1)
	       (cond
		 ((eq 0 elem)
		  (setf (aref grille x y) i)
		  )
		 ((eq 1 elem)
		  (setf (aref grille x (+ y 1)) i)
		  )
		 ((eq 2 elem)
		  (setf (aref grille x (+ y 2)) i)
		  )
		 ((eq 3 elem)
		  (setf (aref grille (+ x 1) y) i)
		  )
		 ((eq 4 elem)
		  (setf (aref grille (+ x 1) (+ y 1)) i)
		  )
		 ((eq 5 elem)
		  (setf (aref grille (+ x 1) (+ y 2)) i)
		  )
		 ((eq 6 elem)
		  (setf (aref grille (+ x 2) y) i)
		  )
		 ((eq 7 elem)
		  (setf (aref grille (+ x 2) (+ y 1)) i)
		  )
		 ((eq 8 elem)
		  (setf (aref grille (+ x 2) (+ y 2)) i)
	       )
		 )
	       )
	)
  )


(defun resolution-en-ligne (grille x)
  (setf *listL* '())
  (loop for a from 0 to 8
	do
       (if (not (eq 0 (aref grille x a)))
	   (setf *listL* (append *listL* (list '(0) ) ) )
	   (setf *listL* (append *listL* (list (liste-de-possibilite grille x a))))
	   )
       )
  )

(defun resolution-en-colonne (grille y)
  (setf *listC* '())
  (loop for a from 0 to 8
	do
       (if (not (eq 0 (aref grille a y)))
	   (setf *listC* (append *listC* (list '(0) ) ) )
	   (setf *listC* (append *listC* (list (liste-de-possibilite grille a y))))
	   )
       )
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;    Fonctions pour les IA     ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun liste-de-possibilite(grid x y)
  (set-difference '(1 2 3 4 5 6 7 8 9) (append (valeur-ligne grid x) (valeur-colonne grid y) (valeur-case grid x y))))
  	

(defun valeur-ligne(grid x)
  (setf *ll* '())
  (loop for a from 0 to 8
	do
	   (if (not (eq (aref grid x a) 0))
	       (setf *ll* (cons (aref grid x a) *ll*))
	       )
	)
  *ll*)


(defun valeur-colonne(grid y)
  (setf *lc* '())
  (loop for a from 0 to 8
	do
	   (if (not (eq (aref grid a y) 0))
	       (setf *lc* (cons (aref grid a y) *lc*))
	       )
	)
  *lc*)


(defun valeur-case(grille i j)
  (cond
    ((and (< i 3) (< j 3))
     ;;;1er carré;;;
     (valeur-case-aux grille 0 0)
     )
    
    ((and (and (< i 6) (> i 2)) (< j 3))
     ;;;2nd carré;;;
     (valeur-case-aux grille 3 0)
     )
    
    ((and (> i 5) (< j 3))
     ;;;3ème carré;;;
     (valeur-case-aux grille 6 0)
     )
    
    ((and (< i 3) (and (< j 6)(> j 2)))
     ;;;4ème carré;;;
     (valeur-case-aux grille 0 3)
     )
    
    ((and (and (< i 6) (> i 2)) (and (< j 6)(> j 2)))
     ;;;5ème carré;;;
     (valeur-case-aux grille 3 3)
     )
    
    ((and (> i 5) (and (< j 6)(> j 2)))
     ;;;6ème carré;
     (valeur-case-aux grille 6 3)
     )
    
    ((and (< i 3) (> j 5))
     ;;;7ème carré;;;
     (valeur-case-aux grille 0 6)
     )
    
    ((and (and (< i 6) (> i 2)) (> j 5))
     ;;;8ème carré;;;
     (valeur-case-aux grille 3 6)
     )
    
    ((and (> i 5) (> j 5))
     ;;;9ème carré;
     (valeur-case-aux grille 6 6)
     )
    )
  )


(defun valeur-case-aux (grille x y)
  (setf *lca* '())
  (loop for a from x to (+ x 2)
	do
	   (loop for b from y to (+ y 2)
		 do
		    (if (not (eq (aref grille a b) 0))
			(setf *lca* (cons (aref grille a b) *lca*))
			)
		 )
	)
  *lca*)
