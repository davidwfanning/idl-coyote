; docformat = 'rst'
;
; NAME:
;  TestApplication
;
; PURPOSE:
;  This is a test program that demonstrates how to use the ACTIVECONTOUR object in 
;  an application environment.
;
;  The `Coyote Library <http://www.idlcoyote.com/documents/programs.php> is required 
;  to run this program.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2011, by Fanning Software Consulting, Inc. All rights reserved.           ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
;
;+--------------------------------------------------------------------------
;  This is a test program that demonstrates how to use the ACTIVECONTOUR object in 
;  an application environment.
;
;  The `Coyote Library <http://www.idlcoyote.com/documents/programs.php>` is required 
;  to run this program.
;
;  This program set can be purchased from the 
;  `Coyote Store <http://www.idlcoyote.com/store/index.php?act=viewCat&catId=3>`.
;
; :Categories:
;    Testing
;        
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive 
;       Fort Collins, CO 80526 USA 
;       Phone: 970-221-0438 
;       E-mail: david@idlcoyote.com 
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;    Modification History::
;       Written by David W. Fanning, 1 December 2003.
;        
; :Copyright:
;    Copyright (c) 2003-2011, Fanning Software Consulting, Inc.
;-

;+
; The event handler for events coming from the ActiveContour object.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;         The events handled here are passed to the program from the ActiveContour object.
;-
PRO TestApplication_SnakeEvents, event

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
                  String(event.data.perimeter, Format='(F8.2)')
               Widget_Control, (*info).alabel, Set_Value='   Area: ' + $
                  String(event.data.area, Format='(F8.2)')

               ; Redisplay the image and the final contour.
               WSet, (*info).wid
               (*info).snake -> UpdateImage
               WSet, (*info).wid
               cgPlotS, event.data.x, event.data.y, /Data, Color='sky blue', Thick=2

            END

      ELSE: ok = Dialog_Message('Unrecognized event.')

   ENDCASE


END ; ---------------------------------------------------------------------------


;+
; The event handler for events coming from the ActiveContour button in the
; text application. The ActiveContour object is set up here and the ActiveContour
; control panel is displayed.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;         The events handled here are passed to the program from TestApplication.
;-
PRO TestApplication_Contour, event

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


;+
; The event handler for events coming from the draw widget in the
; text application. We simply handle motion events and output
; the current location and image value at the image location.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;         The events handled here are passed to the program from the TestApplication
;         draw widget.
;-
PRO TestApplication_DrawEvents, event

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
   Widget_Control, (*info).plabel, Set_Value='   X: ' + StrTrim(xpixCoord,2) + '   Y: ' + StrTrim(ypixCoord,2)
   Widget_Control, (*info).alabel, Set_Value='   Value: ' + StrTrim(((*info).image)[xpixCoord, ypixCoord],2)

END ; ---------------------------------------------------------------------------


;+
; The event handler for events coming from the quit button in the
; text application. Simply destroy the program.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;         The events handled here are passed to the program from the TestApplication
;         Quit button.
;-
PRO TestApplication_Quit, event

   Widget_Control, event.top, /Destroy

END ; ---------------------------------------------------------------------------


;+
; The clean-up routine for the program. This program is called automatically
; when the top-level base widget is destroyed. Anything that may leak program
; memory is destroyed or freed up here.
; 
; :Params:
;     tlb: in, required, type=long
;         The identifier of the top-level base widget.
;-
PRO TestApplication_Cleanup, tlb

   ; Be sure to destroy all objects, pointers, etc.
   Widget_Control, tlb, Get_UValue=info
   Obj_Destroy, (*info).snake
   Ptr_Free, info

END ; ---------------------------------------------------------------------------


;+
; The main test application routine.
; 
; :Params:
;     imagefile: in, optional, type=string
;         The name of an image file to read with Read_Image. If not provided,
;         an image file, mineral.png, from the IDL distribution will be used.
; :Keywords:
;      min_value: in, optional
;         If provided the image will be byte scaled with this value as the MIN
;         value in the BYTSCL command.
;      max_value: in, optional
;         If provided the image will be byte scaled with this value as the MAX
;         value in the BYTSCL command.
;-
PRO TestApplication, imageFile, MIN_VALUE=min_value, MAX_VALUE=max_value

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF
   
   IF N_Elements(imageFile) EQ 0 THEN BEGIN
       imageFile = Filepath(Subdir=['examples', 'data'], 'mineral.png')
       testImage = Fix(Read_PNG(imageFile))
       spatial_scale = 1.0
   ENDIF ELSE BEGIN
       testImage = Read_Image(imageFile)
       IF Size(testImage, /TNAME) EQ 'BYTE' THEN testImage = Fix(testImage)
   ENDELSE
   
   ; Check positional parameters.
   ndims = Size(testImage, /N_Dimensions)
   IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'
   s = Size(testImage, /Dimensions)
   minImage = Min(testImage, MAX=maxImage, /NAN)
   
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
   
   ; Create the TestApplication widgets.
   tlb = Widget_Base(Title='Test Application', Column=1, MBar=menuID)
   fileID = Widget_Button(menuID, Value='File')
   button = Widget_Button(fileID, Value='Quit', Event_Pro='TestApplication_Quit')
   drawWindow = Widget_Draw(tlb, XSize=xsize, YSize=ysize, Motion_Events=1, $
      Event_Pro='TestApplication_DrawEvents')

   labelBase = Widget_Base(tlb, Row=1)
   contourID = Widget_Button(labelBase, Value='Active Contour Mode', $
      Event_Pro='TestApplication_Contour')
   plabel = Widget_Label(labelBase, Value='', /Dynamic_Resize)
   alabel = Widget_Label(labelBase, Value='', /Dynamic_Resize)
   Widget_Control, tlb, /Realize

   ; Get the window index number and display the image.
   Widget_Control, drawWindow, Get_Value=wid
   WSet, wid
   LoadCT, 0, /Silent
   IF (N_Elements(min_value) NE 0)|| (N_Elements(max_value) NE 0) THEN BEGIN
      IF N_Elements(min_value) EQ 0 THEN min_value = minImage
      IF N_Elements(max_value) EQ 0 THEN max_value = maxImage
      displayImage = BytScl(testImage, MIN=min_value, MAX=max_value, /NAN)
   ENDIF ELSE BEGIN
      displayImage = ClipScl(testImage)
   ENDELSE
   cgImage, displayImage
   
   ; Create the ActiveContour object. Object notification events will be sent
   ; to TESTAPPLICATION_SNAKEEVENTS. The SPATIAL_SCALE keyword sets up the
   ; appropriate scaling for this image (I've made the scaling up). Use the
   ; DRAWID keyword to indicate which draw widget should be taken over by the
   ; ActiveContour object.
   snake = Obj_New('ActiveContour', testImage, $
      DrawID=drawWindow, Notify_Event='TestApplication_SnakeEvents', $
      Spatial_Scale=spatial_scale, MIN_VALUE=min_value, MAX_VALUE=max_value, $
      Display_Image=displayImage)

   ; Was the ActiveContour object created successfully?
   IF Obj_Valid(snake) EQ 0 THEN BEGIN
      ok = Dialog_Message('Cannot create valid ActiveContour object. Returning.')
      Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Create an info structure and store it.
   info = { snake:snake, wid:wid, drawWindow:drawWindow, contourID:contourID, $
            plabel:plabel, alabel:alabel, image:testImage, displayImage:displayImage}

   Widget_Control, tlb, Set_UValue=Ptr_New(info, /No_Copy)

   ; Start the program.
   XManager, 'testapplication', tlb, /No_Block, Cleanup='TestApplication_Cleanup'

END ; ---------------------------------------------------------------------------
