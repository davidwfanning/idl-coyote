PRO CleanUp, ALL = all, CG = cg, DG = dg
  ; ALL - set to clean up all windows (this is the default)
  ; CG - set to clean up CG windows
  ; DG - set to clean up direct graphics windows
  

  ; by default clear out all of the windows
  anySet = KEYWORD_SET(cg) + KEYWORD_SET(dg) 
  doAll = ( N_ELEMENTS(all) EQ 0 && anySet EQ 0) ? 1 :  KEYWORD_SET(all)

  IF (doAll || KEYWORD_SET(cg) ) THEN BEGIN
    ; Widget windows or Coyote Graphics windows.
    Widget_Control, /Reset
  ENDIF
  
  IF (doAll || KEYWORD_SET(dg) ) THEN BEGIN
    ; IDL direct graphics windows.
    WHILE !D.Window GT -1 DO WDelete, !D.Window
  ENDIF
  
END