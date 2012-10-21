PRO Print_It, wid, Landscape=landscape, NoScale=noscale, $
   NoCenter=nocenter

; This program sends the contents of the specified
; window to the default printer. The current window
; is used if a window index number is not provided.

; This program was originally written by David Fanning
; using advice from RSI. It was extensively modified and
; improved by Armand J.L. Jongen and others on the IDL newsgroup.

; KEYWORD PARAMETERS:
;
;   LANDSCAPE  If this keyword is set, the output is in Landscape
;              mode. Otherwise, Portrait mode is used.
;
;   NOSCALE    If this keyword is set, the output is not scaled
;              to fill the page, but is left in its actual
;              device coordinate size.
;
;   NOCENTER   If this keyword is set, the output is positioned
;              in the lower-left corner of the display. Otherwise,
;              the output is centered on the page.
;
;  Updated to check for 24-bit devices. 9 Nov 99. DWF.

   ; Check parameters.

IF N_Params() EQ 0 THEN wid = !D.Window
landscape = Keyword_Set(landscape)
noscale = Keyword_Set(noscale)
nocenter = Keyword_Set(nocenter)

   ; Are we running on a 24-bit device?

Catch, theError
IF theError NE 0 THEN BEGIN
   theDepth = 8
   GOTO, testDepth
ENDIF
Device, Get_Visual_Depth=theDepth
testDepth:
IF theDepth GT 8 THEN truecolor = 1 ELSE truecolor = 0
Catch, /Cancel

   ; Make the window current. Get contents.

WSet, wid
contents = TVRD(True=truecolor)

   ; Get the sizes of the window.

xsize = !D.X_Size
ysize = !D.Y_Size

   ; Change the current device to PRINTER. Copy color table.

thisDevice = !D.Name
Set_Plot, 'PRINTER', /Copy

   ; Reset the PRINTER for proper calculations.

Device, Scale_Factor=1, Portrait=1

   ; Get the sizes of the PRINTER device.
   ; If the Landscape keyword is set, swap PRINTER sizes.

IF landscape THEN BEGIN

   pxsize = !D.Y_Size
   pysize = !D.X_Size

ENDIF ELSE BEGIN

   pxsize = !D.X_Size
   pysize = !D.Y_Size

ENDELSE

   ; Calculate a scale factor for the printer.

IF noscale THEN scalefactor = 1 ELSE $
       scalefactor = 1.0 / ((Float(xsize)/pxsize) > (Float(ysize)/pysize))

   ; Do you want the output centered?

IF nocenter THEN BEGIN

   xoffset = 0
   yoffset = 0

ENDIF ELSE BEGIN

   ; Calculate offsets to center output. The offsets
   ; will be scaled later by the device so be sure to
   ; divide by the scale factor.

   xoffset = Fix((Float(pxsize)/scalefactor - xsize)/2.0)
   yoffset = Fix((Float(pysize)/scalefactor - ysize)/2.0)

ENDELSE

   ; Print it.

Device, Landscape=landscape, Scale_Factor=scalefactor
TV, contents, xoffset, yoffset, /Device, True=truecolor
Device, /Close_Document

   ; Clean up.

Set_Plot, thisDevice

END
