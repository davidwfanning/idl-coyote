;+
; NAME:
;       REVERSE_AXES
;
; PURPOSE:
;
;       The purpose of this program is to extend the SIMPLE_SURFACE
;       program to demonstrate how to create reversible axes in
;       object graphics.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Widgets, Object Graphics.
;
; CALLING SEQUENCE:
;
;       REVERSE_AXES, data, x, y
;
; REQUIRED INPUTS:
;
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
;       LANDSCAPE: Set this keyword if you are printing in landscape mode. The
;       default is Portrait mode. The Landscape keyword on the PRINTER object
;       is set, but not all printers will honor this keyword setting. If yours
;       does not, set Landscape mode in the Printer Setup dialog.
;
;       VECTOR: Set this keyword if you want vector printing (as opposed to
;       the default bitmap printing).
;
;       XTITLE: A string used as the X title of the plot.
;
;       YTITLE: A string used as the Y title of the plot.
;
;       ZTITLE: A string used as the Z title of the plot.
;
; COMMON BLOCKS:
;
;       None.
;
; EXAMPLE:
;
;       To use this program with your 2D data, type:
;
;        IDL> Reverse_Axes, data
;
; MODIFICATION HISTORY:
;
;  Written by: David Fanning, October 2001.
;-
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
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
Pro Reverse_Axes_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Reverse_Axes_Draw_Events, event

     ; Draw widget events handled here: expose events and trackball
     ; events. The trackball uses RSI-supplied TRACKBALL oject.

Widget_Control, event.top, Get_UValue=info, /No_Copy

drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = drawTypes(event.type)

CASE thisEvent OF

   'EXPOSE':  ; Nothing required except to draw the view.
   'PRESS': Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
   'RELEASE': Widget_Control, event.id, Draw_Motion_Events=0 ; Motion events OFF.
   'MOTION':  ; Just update trackball.
   ELSE:

ENDCASE

   ; Does the trackball need updating? If so, update.

needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
IF needUpdate THEN BEGIN
   info.thisModel->GetProperty, Transform=modelTransform
   info.thisModel->SetProperty, Transform=modelTransform # thisTransform
ENDIF

    ; Draw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Reverse_Axes_Style, event

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



PRO Reverse_Axes_AxisRotation, event

     ; Event handler allows the axes to be reversed.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Which axis should be reversed?

Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF

      ; The algorithm for all three axes are similar, although the
      ; details of setting the text baseline and up direction
      ; vectors vary for each axis.

      ; The basic algorithm is this. Get the current range of the axis.
      ; Based on the status of the axis, you will find new scaling
      ; and translation factors for the lights, surface and the axis. (I use
      ; the NORMALIZE function for this purpose, but you can use whatever
      ; method you understand.) In addition, you will apply axis-specific
      ; text orientation information.

      ; For the X axis, you change the text baseline in a positive
      ; or negative orientation.

      ; For the Y axis, you change the text up direction in a positive
      ; or negative orientation.

      ; For the Z axis, you also change the text up direction in a positive
      ; or negative orientation.

   'Reverse X Axis': BEGIN
      info.xAxis->GetProperty, CRange=range
      info.xstatus = 1-info.xstatus
      IF info.xstatus THEN BEGIN
         xs = FSC_Normalize(range, Position=[-0.5,0.5])
         info.thisSurface->SetProperty, XCoord_Conv=xs
         info.xAxis->SetProperty, XCoord_Conv=xs, TextBaseline=[1, 0, 0]
         info.fillLight->SetProperty, XCoord_Conv=zs
         info.rotatingLight->SetProperty, XCoord_Conv=zs
      ENDIF ELSE BEGIN
         xs = FSC_Normalize(Reverse(range), Position=[-0.5,0.5])
         info.thisSurface->SetProperty, XCoord_Conv=xs
         info.xAxis->SetProperty, XCoord_Conv=xs, TextBaseline=[-1, 0, 0]
         info.fillLight->SetProperty, XCoord_Conv=zs
         info.rotatingLight->SetProperty, XCoord_Conv=zs
      ENDELSE
      END

   'Reverse Y Axis': BEGIN
      info.yAxis->GetProperty, CRange=range
      info.ystatus = 1-info.ystatus
      IF info.ystatus THEN BEGIN
         ys = FSC_Normalize(range, Position=[-0.5,0.5])
         info.thisSurface->SetProperty, YCoord_Conv=ys
         info.yAxis->SetProperty, YCoord_Conv=ys, TextUpDir=[0, 1, 0]
         info.fillLight->SetProperty, YCoord_Conv=zs
         info.rotatingLight->SetProperty, YCoord_Conv=zs
      ENDIF ELSE BEGIN
         ys = FSC_Normalize(Reverse(range), Position=[-0.5,0.5])
         info.thisSurface->SetProperty, YCoord_Conv=ys
         info.yAxis->SetProperty, YCoord_Conv=ys, TextUpDir=[0, -1, 0]
         info.fillLight->SetProperty, YCoord_Conv=zs
         info.rotatingLight->SetProperty, YCoord_Conv=zs
      ENDELSE
      END

   'Reverse Z Axis': BEGIN
      info.zAxis->GetProperty, CRange=range
      info.zstatus = 1-info.zstatus
      IF info.zstatus THEN BEGIN
         zs = FSC_Normalize(range, Position=[-0.5,0.5])
         info.thisSurface->SetProperty, ZCoord_Conv=zs
         info.zAxis->SetProperty, ZCoord_Conv=zs, TextUpDir=[0, 0, 1]
         info.fillLight->SetProperty, ZCoord_Conv=zs
         info.rotatingLight->SetProperty, ZCoord_Conv=zs
      ENDIF ELSE BEGIN
         zs = FSC_Normalize(Reverse(range), Position=[-0.5,0.5])
         info.thisSurface->SetProperty, ZCoord_Conv=zs
         info.zAxis->SetProperty, ZCoord_Conv=zs, TextUpDir=[0, 0, -1]
         info.fillLight->SetProperty, ZCoord_Conv=zs
         info.rotatingLight->SetProperty, ZCoord_Conv=zs
      ENDELSE
      END

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Reverse_Axes_Output, event

   ; This event handler creates GIF and JPEG files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

info.thisWindow->GetProperty, Image_Data=snapshot

   ; What kind of file is wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='snapshot.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='snapshot.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END


   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='snapshot.tif')
      IF filename NE '' THEN BEGIN

         ; TIFF files should have their Y direction reversed for
         ; compatibility with most other software.

         Write_TIFF, filename, Reverse(snapshot,3)
      ENDIF
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------


PRO Reverse_Axes_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO Reverse_Axes_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Configure printer and print if user OKs.

result = Dialog_PrinterSetup(info.thisPrinter)
IF result EQ 1 THEN BEGIN

      ; Background colors can use a lot of toner. Change background
      ; color to white and axes to black before printing.

   info.xaxis->GetProperty, Color=axisColor
   info.thisView->GetProperty, Color=backgroundColor
   info.thisSurface->GetProperty, Color=surfaceColor

   info.xaxis->SetProperty, Color=[0,0,0]
   info.yaxis->SetProperty, Color=[0,0,0]
   info.zaxis->SetProperty, Color=[0,0,0]
   info.thisView->SetProperty, Color=[255, 255, 255]
   info.thisSurface->SetProperty, Color=[70,70,70]

   ; I want the output on the page to have the same aspect ratio
   ; (ratio of height to width) as I see in the display window.
   ; I use the Aspect function to calculate the correct viewport
   ; position in normalized coordinates. The return value of
   ; Aspect is the position of the viewport on the output page.

info.thisWindow->GetProperty, Dimensions=wdims
info.thisPrinter->GetProperty, Dimensions=pdims
plotAspect = Float(wdims[1]) / wdims[0]
printerAspect = Float(pdims[1]) / pdims[0]
position = Aspect(plotAspect, WindowAspect=printerAspect, Margin=0)

   ; Change the dimensions and postion of the viewport on the output page.
   ; Be sure to use normalized coordinates (units=3).

info.thisView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
   Location=[position[0], position[1]], Units=3

      ; Print the document.

   Widget_Control, Hourglass=1
   info.thisPrinter->Draw, info.thisView, Vector=info.vector
   info.thisPrinter->NewDocument
    Widget_Control, Hourglass=0

      ; Set colors and the view back to original values.

   info.xaxis->SetProperty, Color=axisColor
   info.yaxis->SetProperty, Color=axisColor
   info.zaxis->SetProperty, Color=axisColor
   info.thisView->SetProperty, Color=backgroundColor, Location=[0,0], Dimensions=[0,0]
   info.thisSurface->SetProperty, Color=surfaceColor

ENDIF

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Reverse_Axes_Resize, event

     ; The only events generated by this simple program are resize
     ; events, which are handled here.

     ; Get the info structure.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; Resize the draw widget. This is the proper way to do this
    ; in object graphics, but it does not always work in UNIX
    ; versions of IDL. If it doesn't work for you, comment the
    ; first line out and try the second. The second line is more
    ; portable, but not exactly the proper "object" way. :-(

info.thisWindow->SetProperty, Dimension=[event.x, event.y]
;Widget_Control, info.drawID, Draw_XSize=event.x, Draw_YSize=event.y


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



PRO Reverse_Axes, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Group_Leader=groupLeader, $
   Hidden_Lines=hidden_lines, Vector=vector, Exact=exact, $
   Landscape=landscape

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
IF N_Elements(hidden_lines) EQ 0 THEN hidden_lines = 1
hidden_lines = Keyword_Set(hidden_lines)
landscape = Keyword_Set(landscape)
vector = Keyword_Set(vector)

   ; Need fake data?

IF N_Elements(data) EQ 0 THEN BEGIN
   data = BeselJ(Shift(Dist(40,40),10,15)/2,0)
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
   Viewplane_Rect=[-1.2,-1.1,2.3,2.3], ZClip=[1.15, -1.15] )

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Create font objects.

helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=14)

    ; Create helper objects. First, create title objects
    ; for the axes and plot. Color them green. Title objects
    ; will need to be located by hand for reversible axes.
    ; Be sure you DON'T add the title objects directly to
    ; the axis objects.

xTitleObj = Obj_New('IDLgrText', xtitle, Color=[0,255,0], $
   Font=helvetica10pt, /Enable_Formatting)
yTitleObj = Obj_New('IDLgrText', ytitle, Color=[0,255,0], $
   Font=helvetica10pt, /Enable_Formatting)
zTitleObj = Obj_New('IDLgrText', ztitle, Color=[0,255,0], $
  Font=helvetica10pt, /Enable_Formatting)

    ; Create a trackball for surface rotations. Center it in
    ; the 400-by-400 window. Give it a 200 pixel diameter.

thisTrackball = OBJ_NEW('Trackball', [200, 200], 200)

    ; Create a wire mesh surface object. Make it yellow.

thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
   Color=[255,255,0], _Extra=extra, Hidden_Lines=hidden_lines)

    ; Get the data ranges of the surface.

thisSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange

   ; I create a tick length variable, because I find it
   ; convenient to use a unit of tick length in positioning
   ; the axis titles.

ticklen = 0.1

    ; Create axes objects for the surface. Color them green.
    ; Axes are created after the surface so the range can be
    ; set correctly. Note how I set the axis annotation to 10 point
    ; Helvetica. Another reason for positioning the axis title
    ; independently of the axis is that the tile can have other
    ; colors and sizes than the axis annotation. This is NOT possible
    ; if the title is part of the axis.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=ticklen, $
   Minor=4, Range=xrange, Exact=Keyword_Set(exact))
xAxis->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica10pt

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=ticklen, $
   Minor=4, Range=yrange, Exact=Keyword_Set(exact))
yAxis->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=ticklen, $
   Minor=4, Range=zrange, Exact=Keyword_Set(exact))
zAxis->GetProperty, Ticktext=zAxisText
zAxisText->SetProperty, Font=helvetica10pt

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

xs = FSC_Normalize(xrange, Position=[-0.5,0.5])
ys = FSC_Normalize(yrange, Position=[-0.5,0.5])
zs = FSC_Normalize(zrange, Position=[-0.5,0.5])

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

    ; Add the surface and axes objects to the model.

thisModel->Add, thisSurface
thisModel->Add, xAxis
thisModel->Add, yAxis
thisModel->Add, zAxis

   ; Add the title objects to the model.

thisModel->Add, xTitleObj
thisModel->Add, yTitleObj
thisModel->Add, zTitleObj

    ; Rotate the surface model to the standard surface view.

thisModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
thisModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
thisModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

; Create some lights to view the surface. Surfaces will look
; best if there is some ambient lighting to illuminate them
; uniformly, and some positional lights to give the surface
; definition. We will create three positional lights: one,
; non-rotating light will provide overhead definition. Two
; rotating lights will provide specific surface definition.

    ; First create the ambient light. Don't turn it on too much,
    ; or the surface will appear washed out.

ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2)
thisModel->Add, ambientLight

    ; Shaded surfaces will not look shaded unless there is a
    ; positional light source to give the surface edges definition.
    ; This light will rotate with the surface.

rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.60, $
    Location=[xrange[1], yrange[1], 4*zrange[1]], $
    Direction=[xrange[0], yrange[0], zrange[0]])
thisModel->Add, rotatingLight

    ; Create a fill light source so you can see the underside
    ; of the surface. Otherwise, just the top surface will be visible.
    ; This light will also rotate with the surface.

fillLight = Obj_New('IDLgrLight', Type=1, Intensity=0.4, $
   Location=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, -2*Abs(zrange[0])], $
   Direction=[(xrange[1]-xrange[0])/2.0, (yrange[1]-yrange[0])/2.0, zrange[1]])
thisModel->Add, fillLight

    ; Create a non-rotating overhead side light.

nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.8, $
    Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
    Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])
nonrotatingModel = Obj_New('IDLgrModel')
nonrotatingModel->Add, nonrotatingLight

   ; Be sure to add the non-rotating model to the view, or it won't be visualized.

thisView->Add, nonrotatingModel

    ; Scale the light sources.

rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Rotate the non-rotating model to the standard surface view.

nonrotatingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
nonrotatingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
nonrotatingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

   ; Check for availability of GIF files.

thisVersion = Float(!Version.Release)
IF thisVersion LT 5.4 THEN haveGif = 1 ELSE haveGIF = 0

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Resizeable Window Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Reverse_Axes_Draw_Events', Button_Events=1)

    ; Create FILE menu buttons for printing and exiting.

fileID = Widget_Button(menubase, Value='File')

outputID = Widget_Button(fileID, Value='Save As...', /Menu)
IF havegif THEN dummy = Widget_Button(outputID, Value='GIF File', $
   UValue='GIF', Event_Pro='FSC_Surface_Output')
dummy = Widget_Button(outputID, Value='JPEG File', $
   UValue='JPEG', Event_Pro='FSC_Surface_Output')
dummy = Widget_Button(outputID, Value='TIFF File', $
   UValue='TIFF', Event_Pro='FSC_Surface_Output')

dummy = Widget_Button(fileID, Value='Print', Event_Pro='Reverse_Axes_Printing')

dummy = Widget_Button(fileID, /Separator, Value='Exit', $
   Event_Pro='Reverse_Axes_Exit')

   ; Create STYLE menu buttons for surface style.

styleID = Widget_Button(menubase, Value='Style', Event_Pro='Reverse_Axes_Style', /Menu)
dummy = Widget_Button(styleID, Value='Dot Surface', UValue='DOTS')
dummy = Widget_Button(styleID, Value='Wire Mesh', UValue='MESH')
dummy = Widget_Button(styleID, Value='Solid', UValue='SOLID')
dummy = Widget_Button(styleID, Value='Parallel X Lines', UValue='XPARALLEL')
dummy = Widget_Button(styleID, Value='Parallel Y Lines', UValue='YPARALLEL')
IF hidden_lines THEN hlValue = 'Hidden Lines OFF' ELSE hlValue='Hidden Lines ON'
dummy = Widget_Button(styleID, Value=hlvalue, UValue='HIDDEN')

   ; Create the AXES menu buttons.

axisID = Widget_Button(menubase, Value='Axis', Event_Pro='Reverse_Axes_AxisRotation', /Menu)
dummy = Widget_Button(axisID, Value='Reverse X Axis')
dummy = Widget_Button(axisID, Value='Reverse Y Axis')
dummy = Widget_Button(axisID, Value='Reverse Z Axis')

   ; Realize the widgets.

Widget_Control, tlb, /Realize

    ; Get the window destination object, which is the value of
    ; an object draw widget. The view will be drawn in the window
    ; when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

   ; Once we have a window, find the size of the character box surrounding the
   ; axis annotation, and calculate a location for the axis titles. Note that the Y
   ; dimension of the X axis text box is always about 75% of what it *should* be. This
   ; is the reason the X axis title always appears too close to the axis compared
   ; to the Y and Z axis in the normal default placement. That is why you see that
   ; number multiplied by 1.5 for the XTitleObj below. (The values -0.5, 0.5, and 0
   ; are the endpoints and middle, respectively, of my axis in my viewport rectangle.)

   ; To orient the text properly, you must supply the proper baseline and up direction
   ; vectors to the Y and Z titles. The X title does not need this, since the X "situation"
   ; is the default case. For example, read the Y title orientation like this: draw the
   ; text parallel to the Y axis (Baseline=[0,1,0]), with the up direction in the -X direction
   ; (UpDir=[-1,0,0]).

d = thisWindow->GetTextDimensions(xAxisText)
xTitleObj->SetProperty, Location=[0, -0.5 - d[1]*1.5-ticklen, -0.5], $
   Alignment=0.5

d = thisWindow->GetTextDimensions(yAxisText)
yTitleObj->SetProperty, Location=[-0.5 - d[0]-ticklen, 0, -0.5], $
   Baseline=[0,1,0], UpDir=[-1,0,0], Alignment=0.5

d = thisWindow->GetTextDimensions(zAxisText)
zTitleObj->SetProperty, Location=[-0.5 - d[0]-ticklen, 0.5, 0], $
   Baseline=[0,0,1], UpDir=[-1,0,0], Alignment=0.5

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter', Print_Quality=2, Landscape=landscape)

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
thisContainer->Add, helvetica10pt
thisContainer->Add, helvetica14pt

   ; Create an INFO structure to hold needed program information.

info = { thisContainer:thisContainer, $ ; The object container.
         thisWindow:thisWindow, $       ; The window object.
         thisPrinter:thisPrinter, $     ; The printer object.
         thisSurface:thisSurface, $     ; The surface object.
         thisTrackball:thisTrackball, $ ; The trackball object.
         thisModel:thisModel, $         ; The model object.
         thisView:thisView, $           ; The view object.
         xaxis:xaxis, $                 ; The X axis object.
         yaxis:yaxis, $                 ; The Y axis object.
         zaxis:zaxis, $                 ; The Z axis object.
         xstatus:1, $                   ; The status of the X axis: 1=normal orientation, 0=reversed orientation.
         ystatus:1, $                   ; The status of the Y axis: 1=normal orientation, 0=reversed orientation.
         zstatus:1, $                   ; The status of the Z axis: 1=normal orientation, 0=reversed orientation.
         rotatingLight:rotatingLight, $ ; The rotating light object.
         fillLight:fillLight, $         ; The fill light object.
         xTitleObj:xTitleObj, $         ; The X axis title object.
         yTitleObj:yTitleObj, $         ; The Y axis title object.
         zTitleObj:zTitleObj, $         ; The Z axis title object.
         drawID:drawID, $               ; The widget identifier of the draw widget.
         landscape:landscape, $         ; A flag for landscape output.
         vector:vector }                ; A flag for vector output.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'reverse_axes', tlb, Cleanup='Reverse_Axes_Cleanup', /No_Block, $
   Event_Handler='Reverse_Axes_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------