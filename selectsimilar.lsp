;;; Select Similar - universal (combines SSX, _SELECTSIMILAR, SELSIM and other tools)
;;; (C)2024, ARKANCE CZ - www.cadforum.cz, supports also LT and Web AutoCADs
;;;-----------------------------------------------------------------------------------

;SELECTSIMILARMODE
;   0 Object type - _SelectAnyObject
;   1 Color
;   2 Layer
;   4 Linetype
;   8 LT Scale
;  16 Lineweight
;  32 Plot style
;  64 Object style, incl. text style, dim style, table style
; 128 Named object name, like blocks, xrefs and images
;4096 curve length (not used in AutoCAD)
;8192 area (not used in AutoCAD)

(if (not _SelectSimilarVar)(setq _SelectSimilarVar "SELECTSIMILARMODE"))
(if (not (getvar "SELECTSIMILARMODE"))(setq _SelectSimilarVar "USERI2")) ; Web + old ACADs
;(setq _SelectAnyObject T) ; preselect no object type filter
;(setq _SelectNegative T) ; preselect negative filter
;(setq _SelectSimDebug T) ; set Debug mode
(setq _SelectSimilarMode (getvar _SelectSimilarVar))


(defun SelectSimilar (cmddia / ss1 ssL i ent filter_list prop_list type-layer filter
                          oldSSL sstemp nfilter f count inWeb LengthF AreaF)

(defun ss:union (ss1 ss2 / ename ss-smaller ss-larger c)
 (cond 
  ((and ss1 ss2)
  (setq c 0)
  (if (< (sslength ss1) (sslength ss2))
   (setq ss-smaller ss1
         ss-larger ss2)
   (setq ss-larger ss1
         ss-smaller ss2))
  (while (< c (sslength ss-smaller))
   (setq ename (ssname ss-smaller c)
         c (1+ c))
   (if (not (ssmemb ename ss-larger))
   (ssadd ename ss-larger)))
   ss-larger)
 (ss1 ss1)
 (ss2 ss2)
 (T nil)
 )
)

(defun appendif (lst ent grp / ass) ; append, handle nil + dyn, helps to create SSGET filter
 (if (= (type grp) 'STR)(progn
  (setq ent (cdr (assoc -1 ent))) ; ent
  (if (= grp "Length")
   (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ent grp))))
	(setq LengthF (getpropertyvalue ent grp)) ;else
	(if (= ssL 1)(princ "*property Length missing* "))
  ))
  (if (= grp "Area")
   (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ent grp))))
	(setq AreaF (getpropertyvalue ent grp)) ;else
	(if (= ssL 1)(princ "*property Area missing* "))
  ))
 ) ;else
 (progn
 (setq ass (assoc grp ent)) ; (2 . "BLK") - true prop. value
 (if (and ass (= grp 2)(= (cdr (assoc 0 en)) "INSERT")(not inWeb)) ; dyn.blk
  (if T ; (wcmatch (cdr ass) "`*U*") ; always
   (setq ass (cons 2 (strcat (vla-get-effectivename (vlax-ename->vla-object (cdr (assoc -1 ent)))) ",`*U*")))
  )
 )
 (if ass
  (setq lst (append lst (list ass)))
  (if (= ssL 1)(princ (strcat "*property " (itoa grp) " missing* "))) ; warn! no property set - will select all values!
 )
 )) ; if
 lst
)

(defun dynblkrem (ss name / n idx ent enx) ; delete not matching anon blocks
 (setq n name)
 (if (wcmatch n "*`*U*")(progn
  (setq n (substr n 1 (- (strlen n) 5))) ; trim ",`*U*"
  (repeat (setq idx (sslength ss))
    (setq ent (ssname ss (setq idx (1- idx)))  enx (entget ent))
	(if _SelectNegative
     (if (and (= (cdr (assoc 0 enx)) "INSERT")
			  (/= (strcase n)(strcase (vla-get-effectivename (vlax-ename->vla-object ent)))))
		(ssdel ent ss)) ; remove
     (if (and (= (cdr (assoc 0 enx)) "INSERT")
			  (= (strcase n)(strcase (vla-get-effectivename (vlax-ename->vla-object ent)))))
		(ssdel ent ss)) ; remove
   )
  ) ;rep
 ))
 ss
)

(defun proprem (ss prop / n idx ent) ; delete not matching spec.properties
 (repeat (setq idx (sslength ss))
  (setq ent (ssname ss (setq idx (1- idx))))
  (if (= prop "Length")
   (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ent prop)))) ; if exists
	(if _SelectNegative
	 (if (= (getpropertyvalue ent prop) LengthF)(ssdel ent ss)) ; if same, remove
	 (if (/= (getpropertyvalue ent prop) LengthF)(ssdel ent ss)) ; if differs, remove
	)
	(if _SelectNegative (ssdel ent ss)) ; if not exists, also
  ))
  (if (= prop "Area")
   (if (not (vl-catch-all-error-p (vl-catch-all-apply 'getpropertyvalue (list ent prop)))) ; if exists
	(if _SelectNegative
	 (if (= (getpropertyvalue ent prop) AreaF)(ssdel ent ss)) ; if same remove 
	 (if (/= (getpropertyvalue ent prop) AreaF)(ssdel ent ss)) ; if differs remove 
	)
	(if _SelectNegative (ssdel ent ss)) ; if not exists, also
  ))
 )
 ss
)

(defun mode2txt ( / v s) ; aux labeler of active Modes for prompt
 (setq v _SelectSimilarMode) ; (getvar _SelectSimilarVar))
 (setq s (if _SelectNegative "!NEG! " ""))
 (setq s (strcat s (if _SelectAnyObject "*anyObj" "Objtyp")))
 (if (not (zerop (logand v 1)))(setq s (strcat s "+Col")))
 (if (not (zerop (logand v 2)))(setq s (strcat s "+Layer")))
 (if (not (zerop (logand v 4)))(setq s (strcat s "+LTyp")))
 (if (not (zerop (logand v 8)))(setq s (strcat s "+LtScl")))
 (if (not (zerop (logand v 16)))(setq s (strcat s "+LWeig")))
 (if (not (zerop (logand v 32)))(setq s (strcat s "+PltSt")))
 (if (not (zerop (logand v 64)))(setq s (strcat s "+Styl")))
 (if (not (zerop (logand v 128)))(setq s (strcat s "+Name")))
 (if (not (zerop (logand v 4096)))(setq s (strcat s "+lengtH")))
 (if (not (zerop (logand v 8192)))(setq s (strcat s "+Area")))
 s
)

(defun mode2prop (ent / v lst) ; create selectlist for SSGET by Mode
  (setq v _SelectSimilarMode ; (getvar _SelectSimilarVar)
		lst (if _SelectAnyObject '() (list (assoc 0 ent))))
  (if (not (zerop (logand v 1)))(setq lst (appendif lst ent 62))) ;color
  (if (not (zerop (logand v 2)))(setq lst (appendif lst ent 8))) ;layer
  (if (not (zerop (logand v 4)))(setq lst (appendif lst ent 6))) ;ltype
  (if (not (zerop (logand v 8)))(setq lst (appendif lst ent 48))) ;ltscale
  (if (not (zerop (logand v 16)))(setq lst (appendif lst ent 370))) ;lweight
  (if (not (zerop (logand v 32)))(setq lst (appendif lst ent 390))) ;plotstyle
  (if (not (zerop (logand v 64)))(setq lst (appendif lst ent 7)  lst (appendif lst ent 3))) ;textstyle + dimstyle
  (if (not (zerop (logand v 128)))(setq lst (appendif lst ent 2))) ;name
  (if (not (zerop (logand v 4096)))(setq lst (appendif lst ent "Length"))) ;length - different filter
  (if (not (zerop (logand v 8192)))(setq lst (appendif lst ent "Area"))) ;area - different filter
lst
)

(defun prop2txt (f / s) ; ((0 . "INSERT") (8 . "m_text") (2 . "XXX")) ; labeler of active Modes
 (setq s "Selecting ")
 (if (setq f1 (assoc 0 f))(setq s (strcat s "all " (if _SelectNegative "but " "") (cdr f1) "s "))(setq s (strcat s "*EVERYTHING* ")))
 (if (setq f1 (assoc 8 f))(setq s (strcat s "on layer " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (setq f1 (assoc 2 f))(setq s (strcat s "named " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (setq f1 (assoc 62 f))(setq s (strcat s "colored " (if _SelectNegative "but " "") (itoa (cdr f1)) " ")))
 (if (setq f1 (assoc 7 f))(setq s (strcat s "styled " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (setq f1 (assoc 3 f))(setq s (strcat s "styled " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (setq f1 (assoc 6 f))(setq s (strcat s "w. linetype " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (setq f1 (assoc 48 f))(setq s (strcat s "w. ltscale " (if _SelectNegative "but " "") (rtos (cdr f1) 2 2) " ")))
 (if (setq f1 (assoc 37 f))(setq s (strcat s "w. lweight " (if _SelectNegative "but " "") (rtos (cdr f1) 2 2) " ")))
 (if (setq f1 (assoc 390 f))(setq s (strcat s "plotstyled " (if _SelectNegative "but " "") (strcase (cdr f1)) " ")))
 (if (and LengthF (not (zerop (logand _SelectSimilarMode 4096)))) (setq s (strcat s "w. length " (if _SelectNegative "but " "") (rtos LengthF 2 2) " ")))
 (if (and AreaF (not (zerop (logand _SelectSimilarMode 8192)))) (setq s (strcat s "w. area " (if _SelectNegative "but " "") (rtos AreaF 2 2) " ")))
 (setq s (strcat s "...\n"))
 s
)

(defun do_dia ( / dcl des dch x dd) ; dialog version

 (defun set_tiles ( / v)
  (setq v _SelectSimilarMode) ; (getvar _SelectSimilarVar))
  (set_tile "obj" (if _SelectAnyObject "0" "1"))
  (set_tile "neg" (if _SelectNegative "1" "0"))
  (set_tile "col" (if (zerop (logand v 1)) "0" "1"))
  (set_tile "lay" (if (zerop (logand v 2)) "0" "1"))
  (set_tile "lty" (if (zerop (logand v 4)) "0" "1"))
  (set_tile "lts" (if (zerop (logand v 8)) "0" "1"))
  (set_tile "lwg" (if (zerop (logand v 16)) "0" "1"))
  (set_tile "plt" (if (zerop (logand v 32)) "0" "1"))
  (set_tile "sty" (if (zerop (logand v 64)) "0" "1"))
  (set_tile "nam" (if (zerop (logand v 128)) "0" "1"))
  (set_tile "len" (if (zerop (logand v 4096)) "0" "1"))
  (set_tile "are" (if (zerop (logand v 8192)) "0" "1"))
 )
 (defun set_results ( / v)
  (setq v 0)
  (if (= (get_tile "obj") "1")(setq _SelectAnyObject nil)(setq _SelectAnyObject T))
  (if (= (get_tile "neg") "1")(setq _SelectNegative T)(setq _SelectNegative nil))
  (if (= (get_tile "col") "1")(setq v (+ v 1)))
  (if (= (get_tile "lay") "1")(setq v (+ v 2)))
  (if (= (get_tile "lty") "1")(setq v (+ v 4)))
  (if (= (get_tile "lts") "1")(setq v (+ v 8)))
  (if (= (get_tile "lwg") "1")(setq v (+ v 16)))
  (if (= (get_tile "plt") "1")(setq v (+ v 32)))
  (if (= (get_tile "sty") "1")(setq v (+ v 64)))
  (if (= (get_tile "nam") "1")(setq v (+ v 128)))
  (if (= (get_tile "len") "1")(setq v (+ v 4096)))
  (if (= (get_tile "are") "1")(setq v (+ v 8192)))
  (setq _SelectSimilarMode v) ; (setvar _SelectSimilarVar v)
 )
 
 (setq dcl (vl-filename-mktemp nil nil ".dcl"))
 (setq des (open dcl "w"))
 (foreach x
  '(
     "sels : dialog"
     "{"
     "    key = \"title\"; label = \"Select Similar Settings\"; spacer;"
     "    : row {"
     "        : column {"
     "        : boxed_column {"
     "         alignment = left; label = \"Similar Based On\"; fixed_width = true; width = 24;"
     "         : toggle { key = \"obj\"; label = \"O&bject type\"; value = \"1\"; } spacer;"
     "         : toggle { key = \"col\"; label = \"&Color\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"lay\"; label = \"&Layer\"; value = \"1\"; } spacer;"
     "         : toggle { key = \"lty\"; label = \"L&inetype\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"lts\"; label = \"Line&type scale\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"lwg\"; label = \"Line&weight\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"plt\"; label = \"&Plotstyle\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"sty\"; label = \"Object &style\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"nam\"; label = \"&Name\"; value = \"0\"; } spacer;"
	 " spacer;"
     "         : toggle { key = \"len\"; label = \"Lengt&h\"; value = \"0\"; } spacer;"
     "         : toggle { key = \"are\"; label = \"&Area\"; value = \"0\"; } spacer;"
     "        }"
     "        spacer;"
     "         : toggle { key = \"neg\"; label = \"Ne&gative selection (all but)\"; value = \"0\"; } spacer;"
     "        }"
     "    spacer;"
     "        : column"
     "        { children_fixed_width = true; width = 16;"
     "          : ok_button {width = 12;} spacer;"
     "          : cancel_button {width = 12;} spacer;"
     ;"          : help_button {width = 12;}"
     "         : spacer { height = 18; }"
     "        }"
     "    }"
     "}"
   )
   (write-line x des)
 )
 (setq des (close des))
 (setq dch (load_dialog dcl))
 (new_dialog "sels" dch)
 (set_tiles)
 (action_tile "accept" "(set_results)(done_dialog 1)")
 (setq dd (start_dialog))
 (unload_dialog dch)
) ; do_dia



;-------------------- MAIN -----------------------------------------------------
 (setq inWeb (wcmatch (getvar "PLATFORM") "* 0.0 *"))
 (if (not inWeb)(vl-load-com))
 (if (not _SelectSimilarMode)(setq _SelectSimilarMode 0))
 (princ "\nSource object(s): ")
 (if (not (setq ss1 (cadr (ssgetfirst))))(setq ss1 (ssget)))
 (if ss1 (progn
  (if (or inWeb (= cmddia 0)) ; web app
  (while (setq i (initget "Object Layer Color lType ltScale lineWeight Plotstyle stYle Name neGative lengtH Area")
               i (getkword (strcat "\nSimilarity by " (mode2txt) " [Object/Layer/Name/Color/lType/ltScale/lineWeight/Plotstyle/stYle/lengtH/Area/neGative] <apply>: ")))
   (cond ; nastav proměnnou dle Mode
    ((= i "Object")(setq _SelectAnyObject (not _SelectAnyObject)))
    ((= i "neGative")(setq _SelectNegative (not _SelectNegative)))
    ((= i "Color")(setq _SelectSimilarMode (boole 6 1 _SelectSimilarMode))) ; was: (setvar _SelectSimilarVar (boole 6 1 (getvar _SelectSimilarVar)))
    ((= i "Layer")(setq _SelectSimilarMode (boole 6 2 _SelectSimilarMode)))
    ((= i "lType")(setq _SelectSimilarMode (boole 6 4 _SelectSimilarMode)))
    ((= i "ltScale")(setq _SelectSimilarMode (boole 6 8 _SelectSimilarMode)))
    ((= i "lineWeight")(setq _SelectSimilarMode (boole 6 16 _SelectSimilarMode)))
    ((= i "Plotstyle")(setq _SelectSimilarMode (boole 6 32 _SelectSimilarMode)))
    ((= i "stYle")(setq _SelectSimilarMode (boole 6 64 _SelectSimilarMode)))
    ((= i "lengtH")(setq _SelectSimilarMode (boole 6 4096 _SelectSimilarMode)))
    ((= i "Area")(setq _SelectSimilarMode (boole 6 8192 _SelectSimilarMode)))
    ((= i "Name")(setq _SelectSimilarMode (boole 6 128 _SelectSimilarMode)))
   )
  ) ;while ELSE:
  (progn
   (initget "Settings")(setq i (getkword (strcat "\nSimilarity by " (mode2txt) " [Settings] <apply>: ")))
   (if (= i "Settings")(do_dia))
  )
  ) ; if

 (setq i 0  count 0
       filter_list '()  prop_list '())
 (repeat (setq ssL (sslength ss1)) ; make filter_list from selected
  (setq ent (entget (ssname ss1 i))  i (1+ i))
  (setq type-layer (mode2prop ent))
  (if (not (member type-layer filter_list)) ; not yet?
    (setq filter_list (cons type-layer filter_list)))
 ) ;rep

 (terpri)
 (foreach filter filter_list ; selection of similars
  (princ (prop2txt filter))
  (if _SelectNegative (progn
   ;(setq filter (append filter (list (cons -4 "not>"))) ; single cond only!
   ;      filter (append (reverse filter) (list (cons -4 "<not")))
   ;      filter (reverse filter))
   (setq nfilter '())
   (foreach f filter
    (setq nfilter (append nfilter (list (cons -4 "<not") f (cons -4 "not>"))))
   )
   (setq filter nfilter)
  ))
  (if _SelectSimDebug (print filter))
  (setq sstemp (ssget "_X" filter)) ; across layouts!
  (if _SelectSimDebug (getkword "\nPress ENTER to continue: "))
  (if (and (assoc 2 filter)(not inWeb))(setq sstemp (dynblkrem sstemp (cdr (assoc 2 filter)))))
  (if (not (zerop (logand _SelectSimilarMode 4096))) (setq sstemp (proprem sstemp "Length")))
  (if (not (zerop (logand _SelectSimilarMode 8192))) (setq sstemp (proprem sstemp "Area")))
  (if sstemp
   (setq oldSSL (sslength ss1)
         ss1 (ss:union ss1 sstemp)
         count (+ count (- (sslength ss1) oldSSL))
         sstemp nil))
 ) ; foreach

 (setvar _SelectSimilarVar (logand 4095 _SelectSimilarMode))
 (princ (strcat "\nAdditional " (rtos count 2 0) " object" (if (> count 1) "s" "") " selected."))
 (sssetfirst nil ss1) ; grip-select
 )) ;if ss1
 (princ)
)

(defun C:SELECTSIMILAR ( / ) ; main
 (SELECTSIMILAR (getvar "CMDDIA"))
)
(defun C:SELS ( / ) ; alias
 (SELECTSIMILAR (getvar "CMDDIA"))
)
(defun C:-SELECTSIMILAR ( / ) ; main, commandline
 (SELECTSIMILAR 0)
)
(defun C:-SELS ( / ) ; alias, commandline
 (SELECTSIMILAR 0)
)

(princ "\nSelectSimilar (SelS) command loaded, by ARKANCE, CADforum.cz.")(princ)
