;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CLISP Maze 20030311 by Joe Wingbermuehle - maze generator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Mod from CLISP to VisualLISP by CAD Studio, 2018 - www.cadstudio.cz  www.cadforum.cz
; - like us on Facebook - fb.com/CADstudio
;
; The width and height of the maze. Both must be odd!
(setq *width* 39) ; 139
(setq *height* 21) ; 77

(setq maze nil)
(vl-load-com)

(defun C:MAZE ( / x y)

 (defun random (rng / x) ; integer version
  (fix (* (/ (setq x 4294967296.0 seed (rem (1+ (* 1664525.0 (cond (seed) ((getvar 'DATE))))) x)) x) rng))
 )

(defun setf (arr x y new) ; put array
 (vlax-safearray-put-element arr x y new)
)
(defun aref (arr x y) ; get array
 (vlax-safearray-get-element arr x y)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Start carving the maze at a specific location.
(defun carve-maze (x y / d c cd dv x1 y1 x2 y2)
	(setq d (random 4))
		(setq c 0)
		(while (< c 4)
         (setq  cd (rem (+ c d) 4) ; mod
                dv (cond
                        ((= cd 0) (list 1 0))
                        ((= cd 1) (list 0 1))
                        ((= cd 2) (list -1 0))
                        (t        (list 0 -1)))
                x1 (+ x (car dv))
                y1 (+ y (cadr dv))
                x2 (+ x1 (car dv))
                y2 (+ y1 (cadr dv))
               )
            (if (and (and (> x2 0) (< x2 *width*))
                     (and (> y2 0) (< y2 *height*)))
               (if (and (= (aref maze x1 y1) 1)
                        (= (aref maze x2 y2) 1))
				  (progn
                     (setf maze x1 y1 0)
                     (setf maze x2 y2 0)
                     (carve-maze x2 y2)
                  )
               )
            )
		(setq c (1+ c))
      ); loop
   ;)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate a maze
(defun generate-maze ()
   (random 13)
   (setf maze 1 1 0) ; 1 1 0
   (carve-maze 1 1)
   (setf maze 1 0 0) ; maze entry
   (setf maze (- *width* 1) (- *height* 2) 0) ; maze exit
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Display the maze - textual
(defun display-maze ( / y x)
   (setq y 0)
   (while (< y *height*)
      (setq x 0)
      (while (< x *width*)
         (if (= (aref maze x y) 1)
            (princ "[]") ; \U+25AE
            (princ "  ")
         )
		 (setq x (1+ x))
      ) ; wh x
      (terpri)
	  (setq y (1+ y))
   ) ; wh y
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw the maze - graphics
(defun draw-maze ( / y x elem e oldc oldo)
   (setq oldc (getvar "CMDECHO")  oldo (getvar "OSMODE"))
   (setvar "CMDECHO" 0)(setvar "OSMODE" 0)
   (setq y 0)
   (while (< y *height*); HORIZONTALS ----
      (setq x 0  elem nil)
      (while (< x *width*)
         (if (= (aref maze x y) 1)
			(if elem (setq elem (cons (list x (- *height* y 1)) elem)) ; still horizontals
					 (setq elem (list (list x (- *height* y 1))))
			) ; else 0
			(if (and elem (> (length elem) 1)) ; any segments to draw?
				(progn (command "_LINE" (car (reverse elem)) (car elem) "") (setq elem nil)) ; start->end
				(setq elem nil)
			)
         ) ; maze draw
		 (setq x (1+ x))
		 (if (and (= x *width*) elem (> (length elem) 1))(progn (command "_LINE" (car (reverse elem)) (car elem) ""))) ;finish row
      ) ; wh x
	  (setq y (1+ y))
   ) ; wh y
   (setq x 0)
   (while (< x *width*); VERTICALS -------
      (setq y 0  elem nil)
      (while (< y *height*)
         (if (= (aref maze x y) 1)
			(if elem (setq elem (cons (list x (- *height* y 1)) elem)) ; still verticals
					 (setq elem (list (list x (- *height* y 1))))
			) ; else 0
			(if (and elem (> (length elem) 1)) ; any segments to draw?
				(progn (command "_LINE" (car (reverse elem)) (car elem) "") (setq elem nil)) ; start->end
				(setq elem nil)
			)
         ) ; maze
		 (setq y (1+ y))
		 (if (and (= y *height*) elem (> (length elem) 1))(progn (command "_LINE" (car (reverse elem)) (car elem) ""))) ;finish column
      ) ; wh y
	  (setq x (1+ x))
   ) ; wh x
  (setvar "OSMODE" oldo)
  (setvar "CMDECHO" oldc)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create and display the maze.

(setq maze (vlax-make-safearray vlax-vbInteger (cons 0 (1- *width*)) (cons 0 (1- *height*)))) ; create array
(setq y 0)
(while (< y *height*)
   (setq x 0)
   (while (< x *width*)
      (setf maze x y 1) ; prefill
      (setq x (1+ x))
   ) ; wh x
   (setq y (1+ y))
) ; wh y

(generate-maze) ; create
(display-maze)  ; list out
(draw-maze)     ; draw out

(princ)
)

(princ "\nMAZE command loaded.")
(princ)