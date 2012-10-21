data = LoadData(1)
ok = Dialog_PrinterSetup()
IF ok THEN BEGIN
   thisDevice = !D.Name
   Set_Plot, 'PRINTER'
   Plot, data
   Device, /Close_Document
   Set_Plot, thisDevice
ENDIF
END
