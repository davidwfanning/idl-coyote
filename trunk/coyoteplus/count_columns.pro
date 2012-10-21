FUNCTION Count_Columns, filename, MaxColumns = maxcolumns

; This utility routine is used to count the number of
; columns in an ASCII data file. It uses the first row.
; as the count example.

IF N_Elements(maxcolumns) EQ 0 THEN maxcolumns = 500

OpenR, lun, filename, /Get_Lun

Catch, error
IF error NE 0 THEN BEGIN
   count = count-1
   RETURN, count
ENDIF

count = 1
line = ''
ReadF, lun, line
FOR j=count, maxcolumns DO BEGIN
   text = FltArr(j)
   ReadS, line, text
   count = count + 1
ENDFOR

RETURN, -1
END