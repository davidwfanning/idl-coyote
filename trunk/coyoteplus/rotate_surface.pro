;+
; NAME:
;       ROTATE_SURFACE
;
; PURPOSE:
;       The purpose of this program is to demonstrate how to
;       use object instancing in a simple surface plot with axes
;       and rotational capability. Rotations performed under the
;       "Rotation" menu item use object instancing. See the event
;       handler routine Rotate_Surface_Instancing for details.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       2642 Bradbury Court
;       Fort Collins, CO 80521 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Widgets, Object Graphics.
;
; CALLING SEQUENCE:
;       ROTATE_SURFACE, data, x, y
;
; REQUIRED INPUTS:
;       None. Fake data will be used if no data is supplied in call.
;
; OPTIONAL INPUTS
;
;       data: A 2D array of surface data.
;
;       x: A vector of X data values.
;
;       y: A vector of Y data values.
;
; OPTIONAL KEYWORD PARAMETERS:
;
;       EXACT:  Set this keyword to get exact axis scaling.
;
;       _EXTRA: This keyword collects otherwise undefined keywords that are
;        passed to the surface initialization routine.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       XTITLE: A string used as the X title of the plot.
;
;       YTITLE: A string used as the Y title of the plot.
;
;       ZTITLE: A string used as the Z title of the plot.
;
; COMMON BLOCKS:
;       None.
;
; EXAMPLE:
;       To use this program with your 2D data, type:
;
;        IDL> Rotate_Surface, data
;-


FUNCTION Normalize, range, Position=position

    ; This is a utility routine to calculate the scaling vector
    ; required to position a vector of specified range at a
    ; specific position given in normalized coordinates. The
    ; scaling vector is given as a two-element array like this:
    ;
    ;   scalingVector = [translationFactor, scalingFactor]
    ;
    ; The scaling vector should be used with the [XYZ]COORD_CONV
    ; keywords of a graphics object or model. For example, if you
    ; wanted to scale an X axis into the data range of -0.5 to 0.5,
    ; you might type something like this:
    ;
    ;   xAxis->GetProperty, Range=xRange
    ;   xScale = Normalize(xRange, Position=[-0.5, 0.5])
    ;   xAxis, XCoord_Conv=xScale

On_Error, 1
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
    position=Float(position)
range = Float(range)

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END
;-------------------------------------------------------------------------



PRO Rotate_Surface_Instancing, event

; This event handler illustrates the proper way to instance
; a changing part of the view.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What axis should rotation be about?

Widget_Control, event.id, Get_UValue=thisRotation
CASE thisRotation OF

   'X Axis': rotAxis = [1,0,0]
   'Y Axis': rotAxis = [0,1,0]
   'Z Axis': rotAxis = [0,0,1]

ENDCASE

   ; Put up hourglass.

Widget_Control, /Hourglass

   ; Hide the changing surface model.

info.changingModel->SetProperty, Hide=1

   ; Create an instance of the rest of the graphical display

info.thisWindow->Draw, info.thisView, /Create_Instance

   ; Hide the unchanging model and expose the changing model.

info.unchangingModel->SetProperty, Hide=1
info.changingModel->SetProperty, Hide=0

   ; Set the transparent property of the view so both the changing
   ; and the instance data can be seen at the same time.

info.thisView->SetProperty, Transparent=1

   ; Rotate the surface. And draw the unchanging portion as well.

FOR j=0,359, 3 DO BEGIN
   info.changingModel->Rotate, rotAxis, 3.0, /Premultiply
   info.thisWindow->Draw, info.thisView, /Draw_Instance
ENDFOR

   ; Set everything back to the way it was.

info.thisView->SetProperty, Transparent=0
info.unchangingModel->SetProperty, Hide=0

   ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



Pro Rotate_Surface_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Rotate_Surface_Draw_Events, event

     ; Draw widget events handled here: expose events and trackball
     ; events. The trackball uses RSI-supplied TRACKBALL oject.

Widget_Control, event.top, Get_UValue=info, /No_Copy

drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = drawTypes(event.type)

CASE thisEvent OF

   'EXPOSE':  ; Nothing required except to draw the view.
   'PRESS': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'RELEASE': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=0 ; Motion events OFF.
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'MOTION': BEGIN ; Trackball events
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'SCROLL': ; Nothing required except to draw the view.
ENDCASE

    ; Draw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Rotate_Surface_Style, event

     ; Event handler to select surface style.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What style is wanted?

Widget_Control, event.id, Get_UValue=newStyle
CASE newStyle OF

   'DOTS': info.thisSurface->SetProperty, Style=0
   'MESH': info.thisSurface->SetProperty, Style=1
   'SOLID': info.thisSurface->SetProperty, Style=2, Shading=1
   'XPARALLEL': info.thisSurface->SetProperty, Style=3
   'YPARALLEL': info.thisSurface->SetProperty, Style=4
   'HIDDEN': BEGIN
       Widget_Control, event.id, Get_Value=buttonValue
       IF buttonValue EQ 'Hidden Lines OFF' THEN BEGIN
          setting = 0
          hlvalue = 'Hidden Lines ON'
       ENDIF ELSE BEGIN
          setting = 1
          hlvalue = 'Hidden Lines OFF'
       ENDELSE
       Widget_Control, event.id, Set_Value=hlvalue
       info.thisSurface->SetProperty, Hidden_Lines=setting
       ENDCASE

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Rotate_Surface_Output, event

   ; This event handler creates GIF and JPEG files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

info.thisWindow->GetProperty, Image_Data=snapshot

   ; JPEG or GIF file wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='idl.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='idl.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------


PRO Rotate_Surface_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO Rotate_Surface_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Configure printer and print if user OKs.

result = Dialog_PrintJob(info.thisPrinter)
IF result EQ 1 THEN BEGIN
   info.thisPrinter->Draw, info.thisView
   info.thisPrinter->NewDocument
ENDIF

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Rotate_Surface_Resize, event

     ; The only events generated by this simple program are resize
     ; events, which are handled here.

     ; Get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget.

info.thisWindow->SetProperty, Dimension=[event.x, event.y]

    ; Redisplay the graphic.

info.thisWindow->Draw, info.thisView

    ; Update the trackball objects location in the center of the
    ; window.

info.thisTrackball->Reset, [event.x/2, event.y/2], $
    (event.y/2) < (event.x/2)

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Rotate_Surface, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Group_Leader=groupLeader, $
      Hidden_Lines=hidden_lines, Exact=exact

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
hidden_lines = Keyword_Set(hidden_lines)

   ; Need fake data?

IF N_Elements(data) EQ 0 THEN BEGIN
   data = BeselJ(Shift(Dist(40,40),20,20)/2,0)
ENDIF

   ; Get dimensions of data.

s = Size(data, /Dimensions)

   ; Fill out X and Y vectors if necessary.

IF N_Elements(x) EQ 0 THEN x = Findgen(s[0])
IF N_Elements(y) EQ 0 THEN y = Findgen(s[1])

    ; Create a view. Use RGB color. Charcoal background.
    ; The coodinate system is chosen so that (0,0,0) is in the
    ; center of the window. This will make rotations easier.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[-1.2,-1.1,2.3,2.1])

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Create a changing model to hold the surface and the surface lights.

changingModel = OBJ_New("IDLgrModel")

    ; Create an unchanging model to hold the surface axes.

unchangingModel = OBJ_New("IDLgrModel")

   ; Add these two models to the main model of the program.

thisModel->Add, changingModel
thisModel->Add, unchangingModel

    ; Create helper objects. First, create title objects
    ; for the axes and plot. Color them green.

xTitleObj = Obj_New('IDLgrText', xtitle, Color=[0,255,0])
yTitleObj = Obj_New('IDLgrText', ytitle, Color=[0,255,0])
zTitleObj = Obj_New('IDLgrText', ztitle, Color=[0,255,0])

    ; Create font objects.

helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=14)

    ; Create a trackball for surface rotations. Center it in
    ; the 400-by-400 window. Give it a 200 pixel diameter.

thisTrackball = Obj_New('Trackball', [200, 200], 200)

    ; Create a wire mesh surface object. Make it yellow. Add it
    ; to the changing model.

thisSurface = Obj_New('IDLgrSurface', data, x, y, Shading=1, Style=2, $
   Color=[255,255,0], _Extra=extra, Hidden_Lines=hidden_lines)
changingModel->Add, thisSurface

    ; Get the data ranges of the surface.

thisSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange

    ; Create axes objects for the surface. Color them green.
    ; Axes are created after the surface so the range can be
    ; set correctly. Note how I set the font to 10 point Helvetica
    ; by creating the axis with the title object, then getting the
    ; actual axis text from the axis object itself, and switching it.
    ; Set the RECOMPUTE_DIMENSIONS keyword on the axis text objects
    ; so the text doesn't go crazy when we change the data range.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=xtitleObj, Range=xrange, Exact=Keyword_Set(exact))
xAxis->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica10pt, Recompute_Dimensions=2

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ytitleObj, Range=yrange, Exact=Keyword_Set(exact))
yAxis->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt, Recompute_Dimensions=2

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ztitleObj, Range=zrange, Exact=Keyword_Set(exact))
zAxis->GetProperty, Ticktext=zAxisText
zAxisText->SetProperty, Font=helvetica10pt, Recompute_Dimensions=2

    ; The axes may not use exact axis scaling, so the ranges may
    ; have changed from what they were originally set to. Get
    ; and update the range variables.

xAxis->GetProperty, CRange=xrange
yAxis->GetProperty, CRange=yrange
zAxis->GetProperty, CRange=zrange

    ; Set scaling parameters for the surface and axes so that everything
    ; is scaled into the range -0.5 to 0.5. We do this so that when the
    ; surface is rotated we don't have to worry about translations. In
    ; other words, the rotations occur about the point (0,0,0).

xs = Normalize(xrange, Position=[-0.5,0.5])
ys = Normalize(yrange, Position=[-0.5,0.5])
zs = Normalize(zrange, Position=[-0.5,0.5])

    ; Scale the axes and place them in the coordinate space.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.

xAxis->SetProperty, Location=[9999.0, -0.5, -0.5], XCoord_Conv=xs
yAxis->SetProperty, Location=[-0.5, 9999.0, -0.5], YCoord_Conv=ys
zAxis->SetProperty, Location=[-0.5,  0.5, 9999.0], ZCoord_Conv=zs

    ; Scale the surface. Notice the surface is scaled *AFTER* the
    ; actual data range is obtained from the axes (CRANGE, above).
    ; Failure to do this can result in inaccurate results.

thisSurface->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Add the axes objects to the unchanging model.

unchangingModel->Add, xAxis
unchangingModel->Add, yAxis
unchangingModel->Add, zAxis

    ; Rotate the models to the standard surface view.

unchangingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
unchangingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
unchangingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

changingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
changingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
changingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.


    ; Shaded surfaces will not look shaded unless there is a light
    ; source. Create a positional light source for this surface.

thisLight = Obj_New('IDLgrLight', Type=1, $
    Location=[xrange[1], yrange[1], 4*zrange[1]], $
    Direction=[xrange[0], yrange[0], zrange[0]])
changingModel->Add, thisLight

    ; Create a fill light source so you can see the underside
    ; of the surface. Otherwise, just the top surface will be visible.

fillLight = Obj_New('IDLgrLight', Type=1, Intensity=0.5, $
   Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
   Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])
changingModel->Add, fillLight

    ; Scale the light sources.

thisLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Resizeable Window Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Rotate_Surface_Draw_Events', Button_Events=1)

    ; Create FILE menu buttons for printing and exiting.

fileID = Widget_Button(menubase, Value='File')

outputID = Widget_Button(fileID, Value='Save As...')
dummy = Widget_Button(outputID, Value='GIF File', $
   UValue='GIF', Event_Pro='Rotate_Surface_Output')
dummy = Widget_Button(outputID, Value='JPEG File', $
   UValue='JPEG', Event_Pro='Rotate_Surface_Output')

dummy = Widget_Button(fileID, Value='Print', Event_Pro='Rotate_Surface_Printing')

dummy = Widget_Button(fileID, /Separator, Value='Exit', $
   Event_Pro='Rotate_Surface_Exit')

   ; Create STYLE menu buttons for surface style.

styleID = Widget_Button(menubase, Value='Style', Event_Pro='Rotate_Surface_Style')
dummy = Widget_Button(styleID, Value='Dot Surface', UValue='DOTS')
dummy = Widget_Button(styleID, Value='Wire Mesh', UValue='MESH')
dummy = Widget_Button(styleID, Value='Solid', UValue='SOLID')
dummy = Widget_Button(styleID, Value='Parallel X Lines', UValue='XPARALLEL')
dummy = Widget_Button(styleID, Value='Parallel Y Lines', UValue='YPARALLEL')
IF hidden_lines THEN hlValue = 'Hidden Lines OFF' ELSE hlValue='Hidden Lines ON'
dummy = Widget_Button(styleID, Value=hlvalue, UValue='HIDDEN')

   ; Create ROTATION menu.

rotateID = Widget_Button(menubase, Value='Rotation', Event_Pro='Rotate_Surface_Instancing')
dummy = Widget_Button(rotateID, Value='About X Axis', UValue='X Axis')
dummy = Widget_Button(rotateID, Value='About Y Axis', UValue='Y Axis')
dummy = Widget_Button(rotateID, Value='About Z Axis', UValue='Z Axis')

   ; Realize the widgets.

Widget_Control, tlb, /Realize

    ; Get the window destination object, which is the value of
    ; an object draw widget. The view will be drawn in the window
    ; when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter')

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')

   ; Add created objects to the container. No need to add objects
   ; that have been added to the model, since a model object is
   ; a subclass of a container object. But helper objects that
   ; are NOT added to the model directly MUST be destroyed properly.

thisContainer->Add, thisView
thisContainer->Add, thisPrinter
thisContainer->Add, thisTrackball
thisContainer->Add, xTitleObj
thisContainer->Add, yTitleObj
thisContainer->Add, zTitleObj
thisContainer->Add, thisModel
thisContainer->Add, unchangingModel
thisContainer->Add, helvetica10pt
thisContainer->Add, helvetica14pt

   ; Create an INFO structure to hold needed program information.

info = { thisContainer:thisContainer, $ ; The object container.
         thisWindow:thisWindow, $       ; The window object.
         thisPrinter:thisPrinter, $     ; The printer object.
         thisSurface:thisSurface, $     ; The surface object.
         changingModel:changingModel, $   ; The surface model object.
         unchangingModel:unchangingModel, $   ; The surface model object.
         thisTrackball:thisTrackball, $ ; The trackball object.
         thisModel:thisModel, $         ; The model object.
         thisView:thisView }            ; The view object.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'Rotate_Surface', tlb, Cleanup='Rotate_Surface_Cleanup', /No_Block, $
   Event_Handler='Rotate_Surface_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------