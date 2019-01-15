;COG3D - display center of gravity point for solids and regions
;CAD Studio a.s. - www.cadstudio.cz  www.cadforum.cz
;
(defun c:COG3D ( / ent coords objent objent-vl)
  (vl-load-com)
  (setvar "cmdecho" 0)
  (while (not (setq objent (car (entsel "\nSelect a compound 3D solid or region: "))))
  )
  (while (not (member (setq ent (cdr (assoc 0 (entget objent)))) '("3DSOLID" "REGION")))
    (prompt
      (strcat "\n*** Not a proper type of selected entity - " ent)
    )
    (while (not (setq objent (car (entsel "\nSelect a compound 3D solid or region: "))))
    )
  )
  (setq objent-vl (vlax-ename->vla-object objent))
  (setq coords (vlax-get objent-vl "centroid"))
  (command "_.circle" coords (/ (getvar "VIEWSIZE") 50));draw it
  (command "_.chprop" "_L" "" "_Col" "1" "")
  (princ (strcat "\nCenter of gravity:\nX=" (rtos(car coords)) "  Y=" (rtos(cadr coords)) "  Z=" (rtos(caddr coords))))
  (setvar "cmdecho" 1)
  (prin1)
)
(princ "CADstudio COG3D command loaded")
(prin1)