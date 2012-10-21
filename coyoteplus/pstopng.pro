PRO PStoPNG, directory

    ; This program finds all the PostScript files in a directory and
    ; converts them to PNG files with ImageMagick. You must have ImageMagick
    ; installed to run the program.
    
    On_Error, 2
    
    IF N_Elements(directory) EQ 0 THEN BEGIN
        directory = Dialog_Pickfile(/DIRECTORY, Title='Select Directory with PostScript Files...')
        IF directory EQ "" THEN RETURN
    ENDIF
    
    CD, directory, CURRENT=thisDirectory
    theFiles = File_Search('*.ps', COUNT=count)
    IF count EQ 0 THEN RETURN
    
    FOR j=0,count-1 DO BEGIN
         infileName = theFiles[j]
         root_name = FSC_Base_Filename(infilename, DIR=theDirectory)
         outfileName = root_name + '.png'
         cmd = 'convert -alpha off -density 300 ' +  '"' + infileName + '"' +  ' -resize 50% -flatten -rotate 90' + ' "PNG24:' + outfileName + '"'
         Print, cmd
         SPAWN, cmd
    ENDFOR
    
    CD, thisDirectory
END