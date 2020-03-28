;start stop CMDTIMER - reactor-based timing of AutoCAD commands
;(C)2019 CAD Studio - www.cadstudio.cz  www.cadforum.cz
;
;use the CMDTIMER command to start reactor, then invoke any AutoCAD command - will be time-measured

(defun callbackcmdtimer1 (a b)
 (setq *cmdtimerdate1* (getvar "MILLISECS"))
 (setq *cmdtimerdate2* (getvar "CPUTICKS"))
)
(defun callbackcmdtimer2 (a b / cmdtime1 cmdtime2)
 (setq cmdtime1 (getvar "MILLISECS"))
 (setq cmdtime2 (getvar "CPUTICKS"))
 (if (not *cmdtimerdate1*) (setq *cmdtimerdate1* 0.0))
 (if (not *cmdtimerdate2*) (setq *cmdtimerdate2* 0.0))
 (princ (strcat "\nCMDtimer> " (car b) ": " (rtos (/ (- cmdtime1 *cmdtimerdate1*) 1000.0) 2 6)
		" secs  (" (rtos (- cmdtime2 *cmdtimerdate2*) 2 0) " ticks)"))
)

(defun C:CMDtimer ()

  (if (not *cmdreact*)
    (progn
     (setq *cmdreact* (vlr-command-reactor "CMDtimerStart" (list (cons :vlr-commandwillstart 'callbackcmdtimer1)
								 (cons :vlr-commandended 'callbackcmdtimer2))))
	 (princ "\nCMDTIMER> is now ON")
	)
    (progn
      (vlr-remove *cmdreact*)
      (setq *cmdreact* nil)
	  (princ "\nCMDTIMER> is now OFF")
    )
  )
 (princ)
)

(princ "\nType CMDTIMER to start/stop command timing.")
(princ)
