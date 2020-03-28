;Convert texts to block references of the same name (www.cadforum.cz)
; text "Lampa" -> insert block 'LAMPA'

(defun C:Txt2Ref ( / ss ssl n ent edata txt pt lay rot cmde)
 (princ "\nSelect texts to convert to block references ")
 (setq ss (ssget '((0 . "*TEXT"))))
 (if ss (progn
  (setq cmde (getvar "CMDECHO"))(setvar "CMDECHO" 0)
  (command "._UNDO" "_Group")
  (command "._UCS" "_World")
  (repeat (setq n (sslength ss))
   (setq edata (entget (setq ent (ssname ss (setq n (1- n))))))
   (setq txt (cdr (assoc 1 edata)))
   (setq pt (cdr (assoc 10 edata)))
   (setq lay (cdr (assoc 8 edata)))
   (setq rot (cdr (assoc 50 edata))) ; scale?
   (if (tblsearch "BLOCK" txt)(progn
     (command "._-INSERT" txt "_non" pt 1.0 "" rot)
	 (command "._CHPROP" (entlast) "" "_Lay" lay "")
	 (entdel ent)
    ) ; else
    (princ (strcat " Block *" txt "* not defined! "))
   )
  )
  (command "._UCS" "_Prev")
  (command "._UNDO" "_END")
  (setvar "CMDECHO" cmde)
 ))
 (princ)
)