FUNCTION DirExist, directory

   ; This function is written to tell if a directory exists or not.
   ; It returns 0 if the directory does NOT exist, 1 otherwise.

   ; Save the current directory.

CD, Current=currentDirectory

   ; Use the Catch error handler to catch the case where we
   ; try to CD to a directory that doesn't exist.

Catch, error
IF (error NE 0) THEN BEGIN

      ; Directory must not exist. Return 0.

   RETURN, 0
ENDIF

   ; Try to CD to the directory. If it doesn't exist, an error occurs.

CD, directory

   ; Well, the directory MUST exist if we are here! Change back to
   ; the current directory and return a 1.

CD, currentDirectory
RETURN, 1
END;**********************************************************


PRO CopyData, DEMO=demoDir

   ; This is a program to move the data files needed for the IDL training
   ; course into the current directory. The files are collected from the
   ; data subdirectories of the IDL example and demo directories if your
   ; are using IDL 4.x and from the data subdirectory if you are using IDL 5.0.

ON_ERROR, 1

mainDir = !Dir
thisRelease = StrMid(!Version.Release, 0, 1)
IF (N_Elements(demoDir) EQ 0) and (thisRelease EQ '4') THEN $
   Message, 'The IDL DEMO directory must be specified via the DEMO keyword.'

   ; Does the demo directory exist?

IF thisRelease EQ '4' THEN BEGIN
   demoExist = DirExist(demoDir)
   IF demoExist EQ 0 THEN BEGIN
      Message, 'The IDL DEMO directory apparently does not exist!' , /INFORMATIONAL
      Message, 'Please check spelling of the DEMO directory name.', /INFORMATIONAL
      Message, 'Be sure to pass the DEMO directory name in via the DEMO keyword.'
      RETURN
   ENDIF
ENDIF

   ; Come here if an error occured (e.g., user specified incorrect, but existing directory).

Catch, error
IF error NE 0 THEN BEGIN
   check = Widget_Message(['An error occured while reading data files.', $
      'Please check your directory names and try again!', $
       '', !Err_String], /Error)
   RETURN
ENDIF

   ; Get the current directory. This is where the files will go.

CD, Current=currentDirectory
answer = Widget_Message(['Your current directory is:', currentDirectory, 'Is this OK?'], /Question)
IF StrUpCase(answer) NE 'YES' THEN RETURN

   ; Set up arrays to hold the file names and the file sizes so you can read
   ; the files and write them out in the current directory.

IF thisRelease EQ '4' THEN BEGIN
   demoFilenames = ['ctscan.dat', 'head.dat', 'worldelv.dat', 'convec.dat', 'hurric.dat', 'm51.dat']
   demoXSizes      = [256,  80, 360, 248, 440, 340]
   demoYSizes      = [256, 100, 360, 248, 340, 440]
   demoZSizes      = [  1,  57,   1,   1,   1,   1 ]

      ; Read and write the demo files into the current directory.

   FOR j=0, 5 DO BEGIN

      file = Filepath(Root_Dir=demoDir, SubDir='data', demoFilenames(j) )
      data = BytArr(demoXsizes(j), demoYsizes(j), demoZsizes(j))
      OpenR, lun, file, /Get_Lun
      Print, 'Reading file: ', file
      ReadU, lun, data
      Free_Lun, lun

      file = Filepath(Root_Dir=currentDirectory, demoFilenames(j))
      OpenW, lun, file, /Get_Lun
      Print, 'Writing file: ', file
      WriteU, lun, data
      Free_Lun, lun
   ENDFOR

      ; Create a 24-bit image.

   file = Filepath(Root_Dir=demoDir, SubDir='data', demoFilenames(4))
   data = BytArr(demoXSizes(4), demoYSizes(4), demoZSizes(4))
   OpenR, lun, file, /Get_Lun
   Print, 'Creating 24-bit image... '
   ReadU, lun, data
   Free_Lun, lun
   thisDevice = !D.Name
   Set_Plot, 'Z'
   LoadCT, 4, /Silent
   TVLCT, r, g, b, /Get
   Set_Plot, thisDevice
   image24 = BytArr(3, demoXSizes(4), demoYSizes(4))
   image24(0,*,*) = r(data)
   image24(1,*,*) = g(data)
   image24(2,*,*) = b(data)
   file = Filepath(Root_Dir=currentDirectory, 'image24.dat')
   OpenW, lun, file, /Get_Lun
   WriteU, lun, image24
   Free_Lun, lun

   exampleFilenames = ['cereb.dat', 'abnorm.dat', 'galaxy.dat', 'jet.dat', 'people.dat']
   exampleXSizes      = [512,  64, 256,  81, 192]
   exampleYSizes      = [512,  64, 256,  40, 192]
   exampleZSizes      = [  1,  15,   1, 101,   2]

      ; Read and write the example files into the current directory.

   FOR j=0, 4 DO BEGIN

      file = Filepath(Root_Dir=mainDir, SubDir=['examples','data'], $
         exampleFilenames(j))
      data = BytArr(exampleXsizes(j), exampleYsizes(j), exampleZsizes(j))
      OpenR, lun, file, /Get_Lun
      Print, 'Reading file: ', file
      ReadU, lun, data
      Free_Lun, lun

      file = Filepath(Root_Dir=currentDirectory, exampleFilenames(j))
      OpenW, lun, file, /Get_Lun
      Print, 'Writing file: ', file
      WriteU, lun, data
      Free_Lun, lun
   ENDFOR
ENDIF

IF thisRelease EQ '5' THEN BEGIN
   files = ['cereb.dat', 'abnorm.dat', 'galaxy.dat', 'jet.dat', 'people.dat', $
               'ctscan.dat', 'head.dat', 'worldelv.dat', 'convec.dat', 'hurric.dat', $
               'm51.dat', 'nyny.dat']
   xSizes = [ 512,  64, 256,  81, 192, 256,  80, 360, 248, 440, 340, 768 ]
   ySizes = [ 512,  64, 256,  40, 192, 256, 100, 360, 248, 340, 440, 512 ]
   zSizes = [   1,  15,   1, 101,   2,   1,  57,   1,   1,    1,   1,  1 ]

      ; Read and write the data files into the current directory.

   FOR j=0, N_Elements(files)-1 DO BEGIN

      file = Filepath(Root_Dir=mainDir, SubDir=['examples', 'data'], files(j))
      data = BytArr(xSizes(j), YSizes(j), zSizes(j))
      OpenR, lun, file, /Get_Lun
      Print, 'Reading file: ', file
      ReadU, lun, data
      Free_Lun, lun

      file = Filepath(Root_Dir=currentDirectory, files(j))
      OpenW, lun, file, /Get_Lun
      Print, 'Writing file: ', file
      WriteU, lun, data
      Free_Lun, lun
   ENDFOR

     ; Create a 24-bit image.

   file = Filepath(Root_Dir=mainDir, SubDir=['examples', 'data'], files(7))
   data = BytArr(xSizes(7), YSizes(7), zSizes(7))
   OpenR, lun, file, /Get_Lun
   Print, 'Creating 24-bit image... '
   ReadU, lun, data
   Free_Lun, lun
   thisDevice = !D.Name
   Set_Plot, 'Z'
   LoadCT, 4, /Silent
   TVLCT, r, g, b, /Get
   Set_Plot, thisDevice
   image24 = BytArr(3, XSizes(7), YSizes(7))
   image24(0,*,*) = r(data)
   image24(1,*,*) = g(data)
   image24(2,*,*) = b(data)
   file = Filepath(Root_Dir=currentDirectory, 'image24.dat')
   OpenW, lun, file, /Get_Lun
   WriteU, lun, image24
   Free_Lun, lun
ENDIF

Print,''
Print,'Finished copying files!'
END;**********************************************************
