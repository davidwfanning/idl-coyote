FUNCTION HTML_TIME

   ;; Produces a time stamp suitable for including in coyote.xml.
   
   time = Systime(/UTC)
   day = Strmid(time, 0, 3)
   date = Strmid(time, 8, 2)
   month = Strmid(time, 4, 3)
   year = Strmid(time, 20, 4)
   stamp = Strmid(time, 11, 8)
   
   RETURN, day + ', ' + date + ' ' + month + ' ' + year + ' ' + stamp + ' GMT'
   
END