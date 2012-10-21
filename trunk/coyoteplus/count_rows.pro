FUNCTION Count_Rows, filename, MaxRows = maxrows

; This utility routine is used to count the number of
; rows in an ASCII data file.

IF N_Elements(maxrows) EQ 0 THEN maxrows = 500L
IF N_Elements(filename) EQ 0 THEN BEGIN
   filename = Dialog_Pickfile()
   IF filename EQ "" THEN RETURN, -1
ENDIF

OpenR, lun, filename, /Get_Lun

Catch, error
IF error NE 0 THEN BEGIN
   count = count-1
   Free_Lun, lun
   RETURN, count
ENDIF

RESTART:

count = 1L
line = ''
FOR j=count, maxrows DO BEGIN
   ReadF, lun, line
   count = count + 1

      ; Try again if you hit MAXROWS without encountering the
      ; end of the file. Double the MAXROWS parameter.

   IF j EQ maxrows THEN BEGIN
      maxrows = maxrows * 2
      Point_Lun, lun, 0
      GOTO, RESTART
   ENDIF

ENDFOR

RETURN, -1
END