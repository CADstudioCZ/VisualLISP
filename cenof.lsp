;Get common center of selected objects (COG, bounding box, XYZ)
;CAD Studio - www.cadstudio.cz  www.cadforum.cz
;
(defun C:CenOf ( / inline ss ssl pt LL UR bbox xmin ymin zmin xmax ymax zmax)
  (setq inline (not (zerop (getvar "CMDACTIVE"))))
  (and
    (princ (strcat "\n" (if inline ">> " "") "Get center of these objects (select): "))
    (setq ss (ssget)  ssl (sslength ss))
    (repeat ssl
      (vla-GetBoundingBox (vlax-ename->vla-object (ssname ss (setq ssl (1- ssl)))) 'LL 'UR)
      (setq LL (vlax-SafeArray->list LL)  UR (vlax-SafeArray->list UR))
      (setq bbox (cons LL (cons UR bbox)))
    ) ; rep
    (setq xmin (apply 'min (mapcar 'car bbox))
          ymin (apply 'min (mapcar 'cadr bbox))
          zmin (apply 'min (mapcar 'caddr bbox))
          xmax (apply 'max (mapcar 'car bbox))
          ymax (apply 'max (mapcar 'cadr bbox))
          zmax (apply 'max (mapcar 'caddr bbox))
    )
    (setq pt (mapcar '* '(0.5 0.5 0.5)(list (+ xmin xmax)(+ ymin ymax)(+ zmin zmax))))
    (if (not inline) (progn (entmakex (list '(0 . "POINT")(cons 10 pt))) (princ "\nCenter point created ")(princ pt)))
  )
  (if inline pt (princ))
)

(princ "\nCenOf and 'CenOf commands loaded.")
(princ)
