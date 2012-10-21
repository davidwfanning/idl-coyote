PRO CleanUp, ALL = all, CG = cg, DG = dg, FG=fg
  ; ALL - set to clean up all windows (this is the default)
  ; CG - set to clean up Coyote Graphics windows
  ; DG - set to clean up direct graphics windows
  ; FG - set to cleanup up function graphics windows
  

  ; by default clear out all of the windows
  anySet = KEYWORD_SET(cg) + KEYWORD_SET(dg) + KEYWORD_SET(fg)
  doAll = ( N_ELEMENTS(all) EQ 0 && anySet EQ 0) ? 1 : KEYWORD_SET(all)

  IF ((doAll || KEYWORDSET(fg)) && (Float(!Version.Release)) GE 8.0 ) THEN BEGIN
    ; Function graphics windows.
    FOREACH w, GetWindows() DO w.Close
  ENDIF

  IF (doAll || KEYWORD_SET(cg) ) THEN BEGIN
    ; Widget windows or Coyote Graphics windows.
    Widget_Control, /Reset
  ENDIF
  
  IF (doAll || KEYWORD_SET(dg) ) THEN BEGIN
    ; IDL direct graphics windows.
    WHILE !D.Window GT -1 DO WDelete, !D.Window
  ENDIF
  
END