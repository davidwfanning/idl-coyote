PRO whichendian
   little_endian = (BYTE(1, 0, 1))[0]
   IF (little_endian) THEN $
      ok = Dialog_Message("This machine is LITTLE endian.") ELSE $
      ok = Dialog_Message("This machine BIG endian.")

END