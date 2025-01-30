;Scale by ref. area
;2024, ARKANCE.world - www.cadforum.cz

(defun C:SCALEAREA ( / ss e area1 area2 q pt)
 (prompt "\nSelect objects to scale:")
 (setq ss (ssget))
 (if ss (progn
  (setq e (car (entsel "\nPick the source area object <ENTER to type area>: ")))
  (if (and e (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list e "Area")))))
   (setq area1 (getpropertyvalue e "Area")  e (princ area1))
   ;else
   (setq area1 (getdist "\nSpecify source area: "))
  )
  (setq e (car (entsel "\nPick the target area object <ENTER to type area>: ")))
  (if (and e (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list e "Area")))))
   (setq area2 (getpropertyvalue e "Area")  e (princ area2))
   ;else
   (setq area2 (getdist "\nSpecify target area: "))
  )
  (princ (strcat "\nThe scale ratio is: " (rtos (setq q (/ (sqrt area2) (sqrt area1))) 2 6)))
  (initget 1)
  (setq pt (getpoint "\nPick reference point: "))
  (command "_SCALE" ss "" pt q)
 ))
 (princ)
)
