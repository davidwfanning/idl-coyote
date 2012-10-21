PRO PS_Plotter, image, $
   European=european, $   ; Set this keyword if you want European measurements.
   Font=font, $           ; Set this keyword if you want font information.
   Object=object          ; Output variable to return FSC_PSConfig object.

   ; Get an image if one is not passed in.

IF N_Elements(image) EQ 0 THEN BEGIN
   image = BytArr(360, 360)
   file = Filepath(SubDirectory=['examples', 'data'], 'worldelv.dat')
   OpenR, lun, file, /Get_Lun
   ReadU, lun, image
   Free_Lun, lun
ENDIF

   ; Create the PostScript configuration object.

object = Obj_New('FSC_PSConfig', European=Keyword_Set(european))

   ; We want hardware fonts.

thisFontType = !P.Font
!P.Font = 1

   ; Get user input to PostScript configuration.

IF Keyword_Set(font) THEN object->GUI, /Font ELSE object->GUI

   ; Configure the PostScript Device.

thisDevice = !D.Name
Set_Plot, 'PS'
keywords = object->GetKeywords(FontType=fonttype)
Device, _Extra=keywords

   ; Draw the example plots.

!P.Multi = [ 0, 1, 2]
TVImage, image
Plot, Histogram(image), Title='Example Histogram Plot', XTitle='Pixel Value', $
   YTitle='Number of Pixels', XStyle=1, Max_Value=5000
!P.Multi = 0

   ; Clean up.

Device, /Close_File
Set_Plot, thisDevice
!P.Font = thisfontType

   ; Return the PS_Configuration object or destroy it.

IF Arg_Present(object) EQ 0 THEN Obj_Destroy, object
END ;--------------------------------------------------------------------------------







