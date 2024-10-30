;Multipage PDF import - www.cadforum.cz (www.ARKANCE.world)
;V1.2, 2024

(defun C:MPDFimport (/ fn pg pg1 pgN pgN0 pt initpt cell cmde imp cols row)
 (setq fn (getfiled "Select PDF file to import" "" "pdf" 8))
 (if fn (progn
  (setq cmde (getvar "cmdecho"))(setvar "cmdecho" 0)
  (if (/= (type pgN0) 'INT) (setq pgN0 999))
  (setq initpt (getpoint "\nInitial insertion point <view center>: "))
  (if (not initpt)(setq initpt (getvar "VIEWCTR")))
  (setq cell (getdist initpt "\nPage cell width <default A4/Letter>: "))
  (if (not cell)(setq cell (if (zerop (getvar "measurement")) 8.5 210.0)))
  (setq pg1 (getint "\nStart page # <1>: "))
  (setq pg (if pg1 pg1 1))
   (vl-catch-all-error-p (vl-catch-all-apply '(lambda ()
    (command "_-PDFIMPORT" "_Fi" fn "?" "*" nil)
   ) nil))
   (setq pgN0 (read (getvar "LASTPROMPT"))) ; get number od pages
  (setq pgN (getint (strcat "\nEnd page # <" (itoa pgN0) ">: ")))
  (if (not pgN) (setq pgN pgN0))
  (initget "Import Attach")
  (setq imp (getkword "\nImport or attach [Import/Attach] <Import>: "))
  (if (not imp)(setq imp "Import"))
  (initget "All")
  (setq cols (getint "\nNumber of side-by-side pages for a grid [All] <All>: "))
  (if (or (not cols)(= cols "All")) (setq cols 999))
  (setq col 0  row 0  pt initpt)

  (princ "\nWorking...\n")
  (while (and (not (wcmatch (getvar "LASTPROMPT") "*'`?'`.")) (<= pg pgN)) ; paging...
   (princ pg)(princ ": ")
   (if (= col cols)(setq row (1+ row)  col 0)) ; reset
   (setq pt (mapcar '+ initpt (list (* col cell) (* row cell -1.414) 0)))
    (vl-catch-all-error-p 
    (vl-catch-all-apply '(lambda ()
	 (if (= imp "Attach")
		(command "_-pdfattach" fn (itoa pg) pt (/ cell 8.5) 0.0) ; pt scale rot
		(command "_-pdfimport" "_Fi" fn (itoa pg) pt (/ cell 8.5) 0.0)
	 )
     )
     nil
    ))
   (setq pg (1+ pg)  col (1+ col))
  ) ; while
  (command) ;cancel
  (princ (strcat "\n" imp "ed " (itoa (1- pg)) " pages in " (itoa (1+ row)) " row(s)."))
  (setvar "cmdecho" cmde)
 ))
 (princ)
)
