PRO Make_MPEG_Movie, data, Color=color, Table=table

   ; Check keywords.

IF N_Elements(table) EQ 0 THEN table = 5
color = Keyword_Set(color)

   ; Load a 3D data set if none passed into program.

IF N_Elements(data) EQ 0 THEN BEGIN

      ; Open the MRI head data set.

   file = Filepath(SubDir=['examples', 'data'], 'head.dat')
   data = BytArr(80, 100, 57)
   OpenR, lun, file, /Get_Lun
   ReadU, lun, data
   Free_Lun, lun

   ; Rebin the data to make it a little bigger.

   data = Rebin(data, 80*3, 100*3, 57)

ENDIF

   ; Is this a 3D data set?

IF Size(data, /N_Dimensions) NE 3 THEN BEGIN
   ok = Dialog_Message('Data must have three dimensions.')
   RETURN
ENDIF

   ; Get the size of the data set.

s = Size(data, /Dimensions)
xsize = s[0]
ysize = s[1]
frames = s[2]

   ; Open the MPEG object.

mpegID = MPEG_Open([xsize, ysize], Filename='head.mpg')


   ; Need a color movie?

IF color THEN BEGIN

      ; Load a color table.

   LoadCT, 0 > table < 41, /Silent

      ; Create a 24-bit image for viewing. Get color table vectors.

   image24 = BytArr(3, xsize, ysize)
   TVLCT, r, g, b, /Get

   ; Load the frames.

   FOR j=0,frames-1 DO BEGIN
      image24[0,*,*] = r(data[*,*,j])
      image24[1,*,*] = g(data[*,*,j])
      image24[2,*,*] = b(data[*,*,j])
      MPEG_Put, mpegID, Image=image24, Frame=j
   ENDFOR

ENDIF ELSE BEGIN

   FOR j=0,frames-1 DO MPEG_Put, mpegID, Image=data[*,*,j], Frame=j

ENDELSE

   ; Save the MPEG sequence. Be patient is will take several seconds.

MPEG_Save, mpegID

   ; Close the MPEG sequence and file.

MPEG_Close, mpegID

END