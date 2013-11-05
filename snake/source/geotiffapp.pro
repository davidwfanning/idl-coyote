;*****************************************************************************************************
;+
; NAME:
;       GEOTIFFAPP
;
; PURPOSE:
;
;       This is a program that demonstrates how to use the
;       ACTIVECONTOUR object with GeoTiff files.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       IDL> GeoTiffApp
;
; INPUT ARGUMENTS:
;
;       filename:   The name of a GeoTiff file.
;       
; INPUT KEYWORDS:
;       
;       MAX_VALUE:  The maximum value for scaling the image data.
;       
;       MIN_VALUE:  The minimum value for scaling the image data.
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 24 OCT 2011.
;-
;*****************************************************************************************************
PRO GeoTiffApp_SaveROI, event

   Compile_Opt idl2 
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg(/Traceback)
      RETURN
   ENDIF
   
   Widget_Control, event.top, Get_UValue=info
   
   IF N_Elements(*(*info).roiData) EQ 0 THEN BEGIN
        ok = Dialog_Message('No current ROI to save. Returning.')
        RETURN
   ENDIF
   
   ; Get the ROI points and multiply by the pixels scales to get
   ; projected XY points for the ROI. In ENVI is offset one-half 
   ; pixel left and up from the TIFF file coodinates.
   s = Size((*info).image, /Dimensions)
   roiStruct = *(*info).roiData
   xpts = (*info).xorigin + roiStruct.x * (*info).xscale  ; - ((*info).xscale/2.0D)
   ypts = (*info).yorigin - roiStruct.y * (*info).yscale  ; + ((*info).yscale/2.0D)
   roiName = 'roi_' + StrTrim((*info).roinumber,2) + '.txt'
   
   ; Convert these points to lat/lon space.   
   lonlat = Map_Proj_Inverse(xpts, ypts, Map_Structure=(*info).mapStructure)
   lons = Transpose(lonlat[0,*])
   lats = Transpose(lonlat[1,*])
   color = cgColor('dodger blue', /Triple)
   id = Indgen(roiStruct.npts)
   
   outFile = cgPickfile(File=roiName, Title='Select output ROI file name...', /Write)
   IF outFile EQ "" THEN RETURN
   OpenW, lun, outFile, /Get_Lun
   
   ; Trying to output an ROI file that can be read into ENVI. ENVI colums and
   ; rows start in the upper-left corner and start counting at 1, rather than zero.
   PrintF, lun, '; IDL Output of ROIs (' + !Version.Release + ') [' + Systime() + ']'
   PrintF, lun, '; Number of ROIs: 1'
   PrintF, lun, '; File Dimensions: ' + StrTrim(s[0],2) + ' x ' + StrTrim(s[1],2)
   PrintF, lun, '; '
   PrintF, lun, '; ROI name: ROI ' + StrTrim((*info).roiNumber,2)
   PrintF, lun, '; ROI rgb value: {' + StrTrim(color[0],2) + ', ' + StrTrim(color[1],2) + ', ' + $
      StrTrim(color[2],2) + '}' 
   PrintF, lun, ': ROI npts:', StrTrim(roiStruct.npts,2)
   PrintF, lun, ';  ID    X     Y        Map X       Map Y        Lat          Lon      B1'
   FOR j=0,roiStruct.npts-1 DO BEGIN
      PrintF, lun, id[j]+1, roiStruct.x[j]+1, ((s[1]-1) - roiStruct.y[j])+1, $
         xpts[j], ypts[j], lats[j], lons[j], ((*(*info).roiData).values)[j], $
         Format='(I5, x, I5, x, I5, x, F12.2, x, F12.2, x, F12.6,x, F12.6, x, F0.4)' 
   ENDFOR
   Free_Lun, lun
   
   ; Update the ROI number.
   (*info).roiNumber = (*info).roiNumber + 1
   
END ; ---------------------------------------------------------------------------


PRO GeoTiffApp_SnakeEvents, event

   Compile_Opt idl2 
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg(/Traceback)
      RETURN
   ENDIF
   
   Widget_Control, event.top, Get_UValue=info

   ; Two types of events can come in here. POINTS_COLLECTED indicates that
   ; the original contour has been drawn by the user. CONTROLS_KILLED indicates
   ; the the ActiveContour control panel was killed. This may have implications
   ; for the application program, so a notification occurs.

   CASE event.type OF

      'POINTS_COLLECTED': BEGIN

            ; The user has now drawn the inital contour. It is time
            ; to apply the GVF Active Contour algoritm. Be sure to check
            ; for the user Cancelling out of application.
            roiInfo = (*info).snake -> ApplyGVFSnake(Cancel=cancelled)

            ; If the user cancelled, just redisplay the image.
            IF cancelled THEN BEGIN
               (*info).snake -> UpdateImage
               (*info).snake -> ResetDisplay
            ENDIF

         END

      'CONTROLS_KILLED': BEGIN

            ; The control panel was distroyed without a contour being run.
            ; We need to set things back as they were. Redisplay the image,
            ; get our draw widget back to its original state, and make the
            ; Active Contour Mode button sensitive.
            (*info).snake -> ResetDisplay
            Widget_Control, (*info).contourID, Sensitive=1
         END

      'ROI_COMPLETED': BEGIN

            ; The ROI has been completed. Print the required information and
            ; and draw the final contour.
            
               Widget_Control, (*info).plabel, Set_Value='   Perimeter: ' + $
                  String(event.data.perimeter, Format='(F0.2)')
               Widget_Control, (*info).alabel, Set_Value='   Area: ' + $
                  String(event.data.area, Format='(F0.2)')

               ; Redisplay the image and the final contour.
               WSet, (*info).wid
               (*info).snake -> UpdateImage
               WSet, (*info).wid
               cgPlotS, event.data.x, event.data.y, /Data, Color='sky blue', Thick=2
               
               ; Store the ROI data in case you want to write this to disk.
               *(*info).roiData = event.data
            END

      ELSE: ok = Dialog_Message('Unrecognized event.')

   ENDCASE


END ; ---------------------------------------------------------------------------


PRO GeoTiffApp_Contour, event

   Compile_Opt idl2 
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg(/Traceback)
      RETURN
   ENDIF
   
   ; Set up for contouring with the ActiveContour program.
   Widget_Control, event.top, Get_UValue=info

   ; Display the image.
   WSet, (*info).wid

   ; Set the display to allow ActiveContour to take over draw widget control.
   (*info).snake -> SetDisplay

   ; Display the Active Contour control panel. Killing the control panel
   ; will put the user back in normal mode.
   (*info).snake -> Controls

   ; Make the Active Contour Mode button insensitive.
   Widget_Control, (*info).contourID, Sensitive=0

   ; Get the labels set up for displaying perimeter and area of region contoured.
   Widget_Control, (*info).plabel, Set_Value='   Perimeter:       '
   Widget_Control, (*info).alabel, Set_Value='   Area:       '

END ; ---------------------------------------------------------------------------


PRO GeoTiffApp_DrawEvents, event

   Compile_Opt idl2 
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg(/Traceback)
      RETURN
   ENDIF
   
   ; Motion events are handled here. Display (x,y) location and value of image there.
   Widget_Control, event.top, Get_UValue=info

   ; Our events in device coordinates. We need to convert these into the pixel coordinates
   ; of the image. We do this by converting device coordinates to normalized coordinates,
   ; then converting these normalized coordinates into pixel coordinates, since we know
   ; the image fills up the window.
   normCoord = Convert_Coord(event.x, event.y, /TO_NORMAL, /DEVICE)
   xNorm = Transpose(normCoord[0,*])
   yNorm = Transpose(normCoord[1,*])
   
   s = Size((*info).image, /DIMENSIONS)
   xvec = Scale_Vector(Findgen(s[0]), 0, 1)
   yvec = Scale_Vector(Findgen(s[1]), 0, 1)
   xpixCoord = (Round(Value_Locate(xvec, xNorm)))[0]
   ypixCoord = (Round(Value_Locate(yvec, yNorm)))[0]
   Widget_Control, (*info).plabel, Set_Value='   X: ' + StrTrim(xpixCoord,2) + '   Y: ' + StrTrim((s[1]-1)-ypixCoord,2)
   Widget_Control, (*info).alabel, Set_Value='   Value: ' + StrTrim(((*info).image)[xpixCoord, ypixCoord],2)

END ; ---------------------------------------------------------------------------


PRO GeoTiffApp_Quit, event

   Widget_Control, event.top, /Destroy

END ; ---------------------------------------------------------------------------


PRO GeoTiffApp_Cleanup, tlb

   ; Be sure to destroy all objects, pointers, etc.
   Widget_Control, tlb, Get_UValue=info
   Obj_Destroy, (*info).snake
   Ptr_Free, info

END ; ---------------------------------------------------------------------------


PRO GeoTiffApp, dataFile, MIN_VALUE=min_value, MAX_VALUE=max_value

   Compile_Opt idl2 
   
   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = cgErrorMsg(/Traceback)
      RETURN
   ENDIF
   
   ; Need a data file.
   IF N_Elements(datafile) EQ 0 THEN BEGIN
       dataFile = cgPickfile(Filter='*.tif')
       IF dataFile EQ "" THEN RETURN
   ENDIF
   
   IF Query_Tiff(dataFile) NE 1 THEN Message, 'File is not a TIFF image file.
   mapCoord = cgGeoMap(dataFile, Image=geoImage, GeoTiff=geotags)
   IF N_Elements(geotags) EQ 0 THEN Message, 'File is not a GeoTiff image file.
   IF Size(geoImage, /TNAME) EQ 'BYTE' THEN geoImage = Fix(geoImage)
   mapStruct = mapCoord -> GetMapStruct()
   
   ; Can only work with 2D images.
   ndims = Size(geoImage, /N_Dimensions)
   IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'
 
   ; Get the size of the image.
   s = Size(geoImage, /Dimensions)
   
   ; Get the fields of the geotiff structure.
   fields = Tag_Names(geotags)

   ; Get the pixel scale values.
   xscale = (geotags.ModelPixelScaleTag)[0]
   yscale = (geotags.ModelPixelScaleTag)[1]
   
   ; Get the tie points and calculate the map projection range.
   x0 = (geotags.ModelTiePointTag)[3]
   y1 = (geotags.ModelTiePointTag)[4]
   x1 = x0 + (xscale * s[0])
   y0 = y1 - (yscale * s[1])
   xrange = [x0, x1]
   yrange = [y0, y1]
   limit = [x0,y0,x1,y1]

   ; We need a map projection code.
   index = Where(fields EQ 'PROJECTEDCSTYPEGEOKEY', projCount)
   IF projCount EQ 0 THEN BEGIN
      index = Where(fields EQ 'PROJECTIONGEOKEY', projCount)
      IF projCount GT 0 THEN projCode = geotags.PROJECTIONGEOKEY
   ENDIF ELSE projCode = geotags.PROJECTEDCSTYPEGEOKEY
   IF N_Elements(projCode) EQ 0 THEN Message, 'Cannot find a map projection Geokey in the GeoTiff Structure.'

   ; We are restricting this program to GeoTiff files with UTM map projections.
      IF (projCode GT 32600) AND (projCode LE 32660) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS 84
        zone = projCode - 32600
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotags.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotags.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotags.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotags.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotags.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotags.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotags.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapStruct = Map_Proj_Init(thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, LIMIT=limit)
        
 
   ENDIF

   IF (projCode GT 32700) AND (projCode LE 32760) THEN BEGIN
   
        thisProjection = 101 ; UTM
        thisDatum = 8        ; WGS 84
        zone = -(projCode - 32700)
        
        ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
        ; produces the wrong result when a UTM projection is used in conjunction
        ; with a WGS84 datum (the most common datum used in this projection). Here
        ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
        ; results in position errors of less than a meter typically.
        IF (Float(!version.release) LE 8.2) THEN BEGIN
              Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
              thisDatum = 12
        ENDIF   
        
        CASE 1 OF ; Longitude
              (Where(fields EQ 'PROJNATORIGINLONGGEOKEY'))[0] NE -1 :        lon = geotags.PROJNATORIGINLONGGEOKEY
              (Where(fields EQ 'PROJORIGINLONGGEOKEY'))[0] NE -1 :           lon = geotags.PROJORIGINLONGGEOKEY
              (Where(fields EQ 'PROJCENTERLONGGEOKEY'))[0] NE -1 :           lon = geotags.PROJCENTERLONGGEOKEY
              (Where(fields EQ 'PROJSTRAIGHTVERTPOLELONGGEOKEY'))[0] NE -1 : lon = geotags.PROJSTRAIGHTVERTPOLELONGGEOKEY
               ELSE:
        ENDCASE
            
        CASE 1 OF ; Latitude
               (Where(fields EQ 'PROJNATORIGINLATGEOKEY'))[0] NE -1 : lat = geotags.PROJNATORIGINLATGEOKEY
               (Where(fields EQ 'PROJORIGINLATGEOKEY'))[0] NE -1 :    lat = geotags.PROJORIGINLATGEOKEY
               (Where(fields EQ 'PROJCENTERLATGEOKEY'))[0] NE -1 :    lat = geotags.PROJCENTERLATGEOKEY
               ELSE:
        ENDCASE

        mapStruct = Map_Proj_Init(thisProjection, DATUM=thisDatum, $
            CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon, ZONE=zone, LIMIT=limit)
        
   ENDIF
   
   ; The window aspect must be the same as the image aspect. Maximum size
   ; of the window is 800 pixels.
   aspectRatio = Float(s[1]) / s[0]
   IF aspectRatio GE 1 THEN BEGIN
        ysize = (400 > s[1] < 800)
        xsize =  ysize / aspectRatio
   ENDIF ELSE BEGIN
        xsize = (400 > s[0] < 800)
        ysize = xsize * aspectRatio
   ENDELSE
   
   ; Create the GeoTiffApp widgets.
   tlb = Widget_Base(Title='GeoTiff ActiveContour Application', Column=1, MBar=menuID)
   fileID = Widget_Button(menuID, Value='File')
   button = Widget_Button(fileID, Value='Save ROI to File...', Event_Pro='GeoTiffApp_SaveROI')
   button = Widget_Button(fileID, Value='Quit', Event_Pro='GeoTiffApp_Quit', /Separator)
   drawWindow = Widget_Draw(tlb, XSize=xsize, YSize=ysize, Motion_Events=1, $
      Event_Pro='GeoTiffApp_DrawEvents')

   labelBase = Widget_Base(tlb, Row=1)
   contourID = Widget_Button(labelBase, Value='Active Contour Mode', $
      Event_Pro='GeoTiffApp_Contour')
   plabel = Widget_Label(labelBase, Value='', /Dynamic_Resize)
   alabel = Widget_Label(labelBase, Value='', /Dynamic_Resize)
   Widget_Control, tlb, /Realize

   ; Get the window index number and display the image.
   Widget_Control, drawWindow, Get_Value=wid
   WSet, wid
   LoadCT, 0, /Silent
   
   IF (N_Elements(min_value) NE 0)|| (N_Elements(max_value) NE 0) THEN BEGIN
      displayImage = BytScl(geoImage, MIN=min_value, MAX=max_value, /NAN)
   ENDIF ELSE BEGIN
      displayImage = ClipScl(geoImage)
   ENDELSE
   
   cgImage, displayImage

   ; Create the ActiveContour object. Object notification events will be sent
   ; to GEOTIFFAPP_SNAKEEVENTS. The SPATIAL_SCALE keyword sets up the
   ; appropriate scaling for this image (I've made the scaling up). Use the
   ; DRAWID keyword to indicate which draw widget should be taken over by the
   ; ActiveContour object.
   snake = Obj_New('ActiveContour', geoImage, $
      DrawID=drawWindow, Notify_Event='GeoTiffApp_SnakeEvents', $
      Spatial_Scale=[xscale,yscale], MIN_VALUE=min_value, MAX_VALUE=max_value, $
      Display_Image=displayImage)

   ; Was the ActiveContour object created successfully?
   IF Obj_Valid(snake) EQ 0 THEN BEGIN
      ok = Dialog_Message('Cannot create valid ActiveContour object. Returning.')
      Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Create an info structure and store it.
   info = { snake:snake, wid:wid, drawWindow:drawWindow, contourID:contourID, $
            plabel:plabel, alabel:alabel, image:geoImage, mapStructure:mapStruct, $
            roidata:Ptr_New(/Allocate_Heap), xscale:xscale, yscale:yscale, $
            xorigin:x0, yorigin:y1, xend:x1, yend:y0, roinumber:1 }

   Widget_Control, tlb, Set_UValue=Ptr_New(info, /No_Copy)

   ; Start the program.
   XManager, 'geotiffapp', tlb, /No_Block, Cleanup='GeoTiffApp_Cleanup'

END ; ---------------------------------------------------------------------------
