PRO Power_Spectrum, image

; Display an image next to its power spectrum.

IF N_Elements(image) EQ 0 THEN BEGIN
   image = BytArr(512,512)
   filename = Filepath('cereb.dat', SubDir=['examples', 'data'])
   OpenR, lun, filename, /Get_Lun
   ReadU, lun, image
   Free_Lun, lun
ENDIF

s = Size(image, /Dimensions)
xsize = s[0]
ysize = s[1]

Device, Decomposed=0, Get_Decomposed=theState
LoadCT, 4

Window, /Free, XSize=xsize, YSize=ysize, XPos=50, YPos=50, Title='Original Image'
TVScl, image

Window, /Free, XSize=xsize, YSize=ysize, XPos=55+xsize, YPos=50, Title='Log Power Spectrum'

TVScl, Shift(Alog(Abs(FFT(image, -1)) > 1.0e-10), xsize / 2, ysize / 2)
Device, Decomposed=theState
END
