FUNCTION DOY, day, year

   ; Converts a Day-of-Year Number to a day-month-year string.
   
   On_Error, 2 ; Return to caller of function.
   
   ; Need a DOY value.
   IF N_Elements(day) EQ 0 THEN Message, 'Must specify a Day-of-Year number.'
   day = 0 > day < 366
   
   IF N_Elements(year) EQ 0 THEN BEGIN
      timeStr = Systime()
      year = Fix(StrMid(timeStr, 20, 4))
   ENDIF
   
   ; Find the date.
   CalDat, Julday(1, day, year), month, day
   theMonth = theMonths(month)
   theDay = StrTrim(day, 2)
   theYear = StrTrim(year, 2)
   
   RETURN, theDay + ' ' + theMonth + ' ' + theYear
END