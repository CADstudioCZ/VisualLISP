;Sets totalLayouts automatically - then use Field: %<\AcVar.17.0 Lisp.totallayouts>%
;CAD Studio, 2016
(defun _totalLayoutsReactor (a r)
 (setq totalLayouts (length (layoutlist)))
)
(vlr-command-reactor nil '((:vlr-commandWillStart . _totalLayoutsReactor)))
