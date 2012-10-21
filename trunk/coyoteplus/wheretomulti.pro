;************************ Beginning of wheretomulti.pro ************************

PRO WhereToMulti, Array, Indices, Col, Row, Frame
;+
; NAME:		wheretomulti.pro
;
; FUNCTION:	Convert WHERE output to 2d or 3d indices
;
; USAGE:	WhereToMulti, Array, Indices, Col, Row, Frame
;
; INPUT ARGUMENTS: 
;   Array: the array that was WHERE'd
;   Indices: the indices returned by WHERE
;
; OUTPUT ARGUMENTS: 
;   Col:     Indices to first dimension.
;   Row:     Indices to second dimension.
;   Frame:   Indices to third dimension. Returned only for 3-d array.
;
; OPTIONAL ARGUMENTS: 
;
; KEYWORDS: 
;
; REQUIRED MODULES: 
;
; SIDE EFFECTS: 
;
; ERROR HANDLING:
;   If Array is not a vector or matrix, all return values are set to zero
;   and a message is written to the screen.
;
; NOTES:
;
; HISTORY:
; 1998 Sept 15	J.L.Saba	Developed based on code from David Fanning's
;                               web site.
;
;- End of prologue -------------------------------------------------------------

   s    = SIZE ( Array )

   NCol = s[1]
   Col  = Indices MOD NCol

   IF s[0] EQ 2 THEN BEGIN              ; 2-d array
      Row = Indices / NCol
   ENDIF ELSE IF s[0] EQ 3 THEN BEGIN   ; 3-d array
      NRow  = s(2)
      Row   = ( Indices / NCol ) MOD NRow
      Frame = Indices / ( NRow * NCol )
   ENDIF ELSE BEGIN                     ; neither 2d or 3d
      Col   = 0
      Row   = 0
      Frame = 0
      PRINT, 'WhereToMulti called with bad input. Array not a vector or matrix.'
      HELP, Array
   ENDELSE

   RETURN
END

;*************************** End of wheretomulti.pro ***************************
