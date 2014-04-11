;+
; NAME:
;       SURF_CONTOUR
;
; PURPOSE:
;       The purpose of this program is to demonstrate how to
;       create a shaded surface and coutour plot in the same
;       object graphics window. The contour plot should be
;       able to be translated independently of the surface.
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
;       Widgets, IDL 5 Object Graphics.
;
; CALLING SEQUENCE:
;       SURF_CONTOUR, data, x, y
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


; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;       To use this program with your data, type:
;
;        IDL> Surf_Contour, data
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 20 September 98.
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



Pro Surf_Contour_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Surf_Contour_Draw_Events, event

     ; Draw widget events handled here: expose events and trackball
     ; events. The trackball uses RSI-supplied TRACKBALL_DEFINE.PRO
     ; from the IDL50/examples/object directory.

Widget_Control, event.top, Get_UValue=info, /No_Copy

drawTypes = ['PRESS', 'RELEASE', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = drawTypes(event.type)

CASE thisEvent OF

   'EXPOSE':  ; Nothing required except to draw the view.
   'PRESS': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=1 ; Motion events ON.
       info.thisWindow->SetProperty, Quality=info.dragQuality ; Drag Quality to Low.
       needUpdate = info.thisTrackball->Update(event, Transform=thisTransform)
       IF needUpdate THEN BEGIN
          info.thisModel->GetProperty, Transform=modelTransform
          info.thisModel->SetProperty, Transform=modelTransform # thisTransform
       ENDIF
       END
   'RELEASE': BEGIN
       Widget_Control, event.id, Draw_Motion_Events=0 ; Motion events OFF.
       info.thisWindow->SetProperty, Quality=2 ; Drag Quality to High.
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
   ELSE:

ENDCASE

    ; Draw the view.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Surf_Contour_XColors, event

     ; Event handler to select surface style.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure_Name)
CASE thisEvent OF

   'WIDGET_BUTTON': XColors, Group_Leader=event.top, $
      NotifyID=[event.id, event.top]

   'XCOLORS_LOAD': BEGIN

         ; Get color vectors.

      red = event.r
      green = event.g
      blue = event.b

         ; Calculate and change contour colors.

      nlevels = 12
      dataColors = BytScl(info.data)
      colorInterval = Fix(255/Float(nlevels))
      cLevelColor = (BIndGen(nlevels) + 1) * colorInterval
      contourColors = BIndGen(3, nlevels)
      FOR j=0,nlevels-1 DO contourColors[*,j] = [red(cLevelColor[j]), $
         green(cLevelColor[j]), blue(cLevelColor[j])]
      info.thisContour->SetProperty, C_Color=contourColors

         ; Update the colorbar colors.

      info.colorbar->SetProperty, Red=red, Green=green, Blue=blue

         ; Redisplay the view.

      info.thisWindow->Draw, info.thisView
      ENDCASE

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------


PRO Surf_Contour_Style, event

     ; Event handler to select surface style.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What style is wanted?

Widget_Control, event.id, Get_UValue=newStyle
CASE newStyle OF

   'FILLED': info.thisContour->SetProperty, Fill=1
   'UNFILLED': info.thisContour->SetProperty, Fill=0
   'ON': IF NOT info.thisModel->ISContained(info.contourModel) THEN $
         info.thisModel->Add, info.contourModel
   'OFF': IF info.thisModel->ISContained(info.contourModel) THEN $
      info.thisModel->Remove, info.contourModel

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------


PRO Surf_Contour_Surface_Style, event

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

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------




PRO Surf_Contour_Slider_Events, event

     ; Event handler to handle contour slider events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; The slider value is the new GeomZ value after it is
    ; scaled into the coordinate system.

val = event.value - info.sliderValue
zrange = info.zrange
info.sliderValue = event.value

scaledVal = Float(val) / (zrange[1] - zrange[0])
info.ContourModel->Translate, 0, 0, scaledVal

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Surf_Contour_Properties, event

     ; Event handler to set graphic properties.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What property is wanted?

Widget_Control, event.id, Get_UValue=newProperty
CASE newProperty OF

   'ORIGINAL_T3D': info.thisModel->SetProperty, Transform=info.origTransform

       ; Surface color.

   'SBLACK': info.thisSurface->SetProperty, Color=[0,0,0]
   'SWHITE': info.thisSurface->SetProperty, Color=[255,255,255]
   'SGREEN': info.thisSurface->SetProperty, Color=[0,255,0]
   'SCHARCOAL': info.thisSurface->SetProperty, Color=[80,80,80]
   'SYELLOW': info.thisSurface->SetProperty, Color=[255,255,0]

       ; Background color.

   'BBLACK': info.thisView->SetProperty, Color=[0,0,0]
   'BWHITE': info.thisView->SetProperty, Color=[255,255,255]
   'BCHARCOAL': info.thisView->SetProperty, Color=[80,80,80]

       ; Axes colors.

   'ABLACK': BEGIN
      info.xAxis->SetProperty, Color=[0,0,0]
      info.yAxis->SetProperty, Color=[0,0,0]
      info.zAxis->SetProperty, Color=[0,0,0]
      END
   'AWHITE': BEGIN
      info.xAxis->SetProperty, Color=[255,255,255]
      info.yAxis->SetProperty, Color=[255,255,255]
      info.zAxis->SetProperty, Color=[255,255,255]
      END
   'AGREEN': BEGIN
      info.xAxis->SetProperty, Color=[0,255,0]
      info.yAxis->SetProperty, Color=[0,255,0]
      info.zAxis->SetProperty, Color=[0,255,0]
      END
   'AYELLOW': BEGIN
      info.xAxis->SetProperty, Color=[255,255,0]
      info.yAxis->SetProperty, Color=[255,255,0]
      info.zAxis->SetProperty, Color=[255,255,0]
      END

      ; Color schemes.

   'B/W': BEGIN
      info.thisView->SetProperty, Color=[255,255,255]
      info.thisSurface->SetProperty, Color=[0,0,0]
      info.xAxis->SetProperty, Color=[0,0,0]
      info.yAxis->SetProperty, Color=[0,0,0]
      info.zAxis->SetProperty, Color=[0,0,0]
      END
   'W/B': BEGIN
      info.thisView->SetProperty, Color=[0,0,0]
      info.thisSurface->SetProperty, Color=[255,255,255]
      info.xAxis->SetProperty, Color=[255,255,255]
      info.yAxis->SetProperty, Color=[255,255,255]
      info.zAxis->SetProperty, Color=[255,255,255]
      END
   'ORIGINAL_COLORS': BEGIN
      info.thisView->SetProperty, Color=[80,80,80]
      info.thisSurface->SetProperty, Color=[255,255,0]
      info.xAxis->SetProperty, Color=[0,255,0]
      info.yAxis->SetProperty, Color=[0,255,0]
      info.zAxis->SetProperty, Color=[0,255,0]
      END

   'DRAG_LOW': BEGIN
      info.dragQuality = 0
      Widget_Control, info.dragLowID, Sensitive=0
      Widget_Control, info.dragMedID, Sensitive=1
      Widget_Control, info.dragHighID, Sensitive=1
      END

   'DRAG_MEDIUM': BEGIN
      info.dragQuality = 1
      Widget_Control, info.dragMedID, Sensitive=0
      Widget_Control, info.dragLowID, Sensitive=1
      Widget_Control, info.dragHighID, Sensitive=1
      END

   'DRAG_HIGH': BEGIN
      info.dragQuality = 2
      Widget_Control, info.dragMedID, Sensitive=1
      Widget_Control, info.dragLowID, Sensitive=1
      Widget_Control, info.dragHighID, Sensitive=0
      END

ENDCASE

    ; Redraw the graphic.

info.thisWindow->Draw, info.thisView

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Surf_Contour_Output, event

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


PRO Surf_Contour_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO Surf_Contour_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button?

Widget_Control, event.id, Get_UValue=ButtonValue
CASE buttonValue OF
   'PRINT': BEGIN
      result = Dialog_PrintJob(info.thisPrinter)
      IF result EQ 1 THEN BEGIN
         info.thisPrinter->Draw, info.thisView
         info.thisPrinter->NewDocument
      ENDIF
      END
   'SETUP': BEGIN
      result = Dialog_PrinterSetup(info.thisPrinter)
      IF result EQ 1 THEN BEGIN
         info.thisPrinter->Draw, info.thisView
         info.thisPrinter->NewDocument
      ENDIF
      END
ENDCASE

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO Surf_Contour_Resize, event

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



PRO Surf_Contour, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Group_Leader=groupLeader

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'

    ; Need some data.

Catch, error
IF error NE 0 THEN BEGIN  ; Can't find cgDemoData.
   data = DIST(41)
   x = Findgen(41)
   y = Findgen(41)
   IF !Error NE -154 THEN Print, !Err_String ELSE Print, 'Skipping cgDemoData call.'
ENDIF

IF N_Elements(data) EQ 0 THEN BEGIN
   data = cgDemoData(2)
ENDIF

s = Size(data)

IF s(0) NE 2 THEN Message,'Must pass 2D argument. Using fake data.'
IF N_Elements(x) EQ 0 THEN x = Findgen(s(1))
IF N_Elements(y) EQ 0 THEN y = Findgen(s(2))

Catch, /Cancel

    ; Create a view. Use RGB color. Charcoal background.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[-1.2,-1.1,2.3,2.3])

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

    ; Create a separate model for the colorbar that doesn't rotate.

cbarModel = Obj_New('IDLgrModel')
thisView->Add, cbarModel

    ; Create a shaded surface object. Make it yellow.

thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, Style = 2, $
   Shading=1, Color=[255,255,0], _Extra=extra)

    ; Colors are the only way to distinguish contour intervals.
    ; Here I load a color table and use colors from the table
    ; for the different contour lines.

thisDevice = !D.Name
Set_Plot, 'Z'
LoadCT, 4
TVLCT, red, green, blue, /Get
Set_Plot, thisDevice
nlevels = 12
dataColors = BytScl(data)
colorInterval = Fix(255/Float(nlevels))
cLevelColor = (BIndGen(nlevels) + 1) * colorInterval
contourColors = BIndGen(3, nlevels)
FOR j=0,nlevels-1 DO contourColors[*,j] = [red(cLevelColor[j]), $
   green(cLevelColor[j]), blue(cLevelColor[j])]

    ; Create a contour plot object.

xrange = [Min(x), Max(x)]
yrange = [Min(y), Max(y)]
zrange = [Min(data), Max(data)+ 0.2*Max(data)]
xs = Normalize(xrange, Position=[-0.5, 0.5])
ys = Normalize(yrange, Position=[-0.5, 0.5])
zs = Normalize(zrange)

thisContour = Obj_New('IDLgrContour', data, GeomZ=0, N_Levels=nlevels, $
   Color=[255, 255, 0], /Planar, XCoord_Conv=xs, YCoord_Conv=ys, $
   ZCoord_Conv=zs, C_Color=contourColors, Fill=1)

   ; Create a contour model.

contourModel = Obj_New('IDLgrModel')

    ; Create axes for the contour plot. Color them yellow.

xAxis1 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=xrange, XCoord_Conv=xs, $
   Location=[1000, -0.5, 0.0], /Exact)
 xAxis2 = Obj_New("IDLgrAxis", 0, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=xrange, XCoord_Conv=xs, $
   Location=[1000, 0.5, 0.0], TickDir=1, /Exact)

yAxis1 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=yrange, YCoord_Conv=ys, $
   Location=[-0.5, 1000, 0.0], /Exact)
yAxis2 = Obj_New("IDLgrAxis", 1, Color=[255,255,0], Ticklen=0.05, $
   Minor=4, /NoText, Range=yrange, YCoord_Conv=ys, $
   Location=[0.5, 1000, 0.0], TickDir=1, /Exact)

    ; Add the axes to the model.

contourModel->Add, xAxis1
contourModel->Add, xAxis2
contourModel->Add, yAxis1
contourModel->Add, yAxis2

    ; Add the contour plot to the model and the model to the main model.

contourModel->Add, thisContour
thisModel->Add, contourModel

    ; Create title objects for the axes and plot. Color them green.

xTitle = Obj_New('IDLgrText', xtitle, Color=[0,255,0])
yTitle = Obj_New('IDLgrText', ytitle, Color=[0,255,0])
zTitle = Obj_New('IDLgrText', ztitle, Color=[0,255,0])

    ; Create axes objects for the surface. Color them green.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=xtitle, /Exact)
xAxis->GetProperty, Ticktext=xAxisText
helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
xAxisText->SetProperty, Font=helvetica10pt

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ytitle, /Exact)
yAxis->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ztitle, /Exact)
zAxis->GetProperty, Ticktext=zAxisText
zAxisText->SetProperty, Font=helvetica10pt

    ; Add the surface and axes objects to the model.

thisModel->Add, thisSurface
thisModel->Add, xAxis
thisModel->Add, yAxis
thisModel->Add, zAxis

    ; Create a trackball for surface rotations. Center it in
    ; the window.

thisTrackball = OBJ_NEW('Trackball', [200, 200], 200)

    ; Get the data ranges for the surface.

thisSurface->GetProperty,XRange=xrange,YRange=yrange;,ZRange=zrange

    ; Create a positional light source for this surface.
    ; This will make the solid surface look shaded and not flat.

thisLight = Obj_New('IDLgrLight', Type=1, $
    Location=[xrange[1], yrange[1], 4*zrange[1]])
thisModel->Add, thisLight

   ; Create a fill light underneath the surface. Make it red.

fillLight = Obj_New('IDLgrLight',Type=1, Intensity=0.5, $
   Location=[xrange[0], yrange[0], -4*zrange[1]], Color=[255,0,0])
thisModel->Add, fillLight

    ; Set scaling parameters for the surface and axes so that everything
    ; is scaled into the range -0.5 to 0.5. We do this so that when the
    ; surface is rotated we don't have to worry about translations. In
    ; other words, the rotations occur about the point (0,0,0).

xs = Normalize(xrange, Position=[-0.5,0.5])
ys = Normalize(yrange, Position=[-0.5,0.5])
zs = Normalize(zrange, Position=[-0.5,0.5])

    ; Set the range, location, and scaling factors for the axes.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.

xAxis->SetProperty, Range=xrange, Location=[9999.0, -0.5, -0.5], $
    XCoord_Conv=xs
yAxis->SetProperty, Range=yrange, Location=[-0.5, 9999.0, -0.5], $
    YCoord_Conv=ys
zAxis->SetProperty, Range=zrange, Location=[-0.5, 0.5, 9999.0], $
    ZCoord_Conv=zs

    ; Scale the surface and light source.

thisSurface->SetProperty,XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
thisLight->SetProperty,XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Rotate the surface model to the standard surface view.

thisModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
thisModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
thisModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

    ; Create a horizontal colorbar for the plot.

cbarFont = Obj_New('IDLgrFont', Name='Helvetica', Size=9)
cbarTitle = Obj_New('IDLgrText', 'Meters', Color=[255,255,0],Font=cbarFont)

    ; Create the tick labels for the colorbar.

nDivisions = 5
step = (zRange[1] - zRange[0]) / (nDivisions - 1)
cbarTicksValues = StrArr(nDivisions)
FOR j=0,nDivisions-1 DO cbarTicksValues[j] = String(zrange[0] + j*step, Format='(I4)')
cbarTicks = Obj_New('IDLgrText', cbarTicksValues, Color=[255,255,0], Font=cbarFont)

colorbar = Obj_New('IDLgrColorbar', red, green, blue, $
   Dimensions=[256, 40], YCoord_Conv=Normalize([0,39],Position=[0.80, 0.90]), $
   XCoord_Conv=Normalize([0,255],Position=[-0.5,0.5]),$
   Title=cbarTitle, Major=nDivisions, Color=[255,255,0], TickLen=5, $
   TickText=cbarTicks, /Show_Axis)

cbarModel->Add, colorbar

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Resizeable Window Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)

drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Surf_Contour_Draw_Events', Button_Events=1)

zmin = Fix(Floor(Min(data)))
zmax = Fix(Ceil(Max(data) + 0.2*(Max(data))))
zmid = (zmax - zmin) / 2
zrange = [zmin, zmax]

contourSlider = Widget_Slider(tlb, Value=zmid, Max=zmax, Min=zmin, $
   Scr_XSize=400, Event_Pro='Surf_Contour_Slider_Events', $
   Title='Contour Position')

    ; Create FILE menu buttons for printing and exiting.

filer = Widget_Button(menubase, Value='File', /Menu)
pnt = Widget_Button(filer, Value='Print', $
   Event_Pro='Surf_Contour_Printing', UValue='PRINT')
pntset = Widget_Button(filer, Value='Print Setup', $
   Event_Pro='Surf_Contour_Printing', UValue='SETUP')
quitter = Widget_Button(filer, /Separator, Value='Exit', $
   Event_Pro='Surf_Contour_Exit')

   ; Create STYLE menu buttons for surface style.

style = Widget_Button(menubase, Value='Surface Style', /Menu)
dummy = Widget_Button(style, Value='Dot Surface', $
   Event_Pro='Surf_Contour_Surface_Style', UValue='DOTS')
dummy = Widget_Button(style, Value='Wire Mesh', $
   Event_Pro='Surf_Contour_Surface_Style', UValue='MESH')
dummy = Widget_Button(style, Value='Solid', $
   Event_Pro='Surf_Contour_Surface_Style', UValue='SOLID')
dummy = Widget_Button(style, Value='Parallel X Lines', $
   Event_Pro='Surf_Contour_Surface_Style', UValue='XPARALLEL')
dummy = Widget_Button(style, Value='Parallel Y Lines', $
   Event_Pro='Surf_Contour_Surface_Style', UValue='YPARALLEL')

   ; Create STYLE menu buttons for contour plot.

style = Widget_Button(menubase, Value='Contour Controls', /Menu)
dummy = Widget_Button(style, Value='Filled Contour', $
   Event_Pro='Surf_Contour_Style', UValue='FILLED')
dummy = Widget_Button(style, Value='Unfilled Contour', $
   Event_Pro='Surf_Contour_Style', UValue='UNFILLED')
dummy = Widget_Button(style, Value='Contour ON', $
   Event_Pro='Surf_Contour_Style', UValue='ON')
dummy = Widget_Button(style, Value='Contour OFF', $
   Event_Pro='Surf_Contour_Style', UValue='OFF')
dummy = Widget_Button(style, Value='Change Colors', $
   Event_Pro='Surf_Contour_XCOLORS', /Separator)


   ; Create PROPERTIES menu buttons for surface properties.

properties = Widget_Button(menubase, Value='Properties', /Menu)

   ; Surface Color

scolor = Widget_Button(properties, Value='Surface Color', /Menu)
dummy = Widget_Button(scolor, Value='Black', $
   Event_Pro='Surf_Contour_Properties', UValue='SBLACK')
dummy = Widget_Button(scolor, Value='White', $
   Event_Pro='Surf_Contour_Properties', UValue='SWHITE')
dummy = Widget_Button(scolor, Value='Charcoal', $
   Event_Pro='Surf_Contour_Properties', UValue='SCHARCOAL')
dummy = Widget_Button(scolor, Value='Yellow', $
   Event_Pro='Surf_Contour_Properties', UValue='SYELLOW')
dummy = Widget_Button(scolor, Value='Green', $
   Event_Pro='Surf_Contour_Properties', UValue='SGREEN')

   ; Background Color

bcolor = Widget_Button(properties, Value='Background Color', /Menu)
dummy = Widget_Button(bcolor, Value='Black', $
   Event_Pro='Surf_Contour_Properties', UValue='BBLACK')
dummy = Widget_Button(bcolor, Value='White', $
   Event_Pro='Surf_Contour_Properties', UValue='BWHITE')
dummy = Widget_Button(bcolor, Value='Charcoal', $
   Event_Pro='Surf_Contour_Properties', UValue='BCHARCOAL')

   ; Axes Color

acolor = Widget_Button(properties, Value='Axes Color', /Menu)
dummy = Widget_Button(acolor, Value='Black', $
   Event_Pro='Surf_Contour_Properties', UValue='ABLACK')
dummy = Widget_Button(acolor, Value='White', $
   Event_Pro='Surf_Contour_Properties', UValue='AWHITE')
dummy = Widget_Button(acolor, Value='Yellow', $
   Event_Pro='Surf_Contour_Properties', UValue='AYELLOW')
dummy = Widget_Button(acolor, Value='Green', $
   Event_Pro='Surf_Contour_Properties', UValue='AGREEN')

   ; Color Schemes.

dummy = Widget_Button(properties, Value='Black on White', /Separator, $
   Event_Pro='Surf_Contour_Properties', UValue='B/W')
dummy = Widget_Button(properties, Value='White on Black', $
   Event_Pro='Surf_Contour_Properties', UValue='W/B')
dummy = Widget_Button(properties, Value='Original Colors', $
   Event_Pro='Surf_Contour_Properties', UValue='ORIGINAL_COLORS')

   ; Original Axis rotation.

dummy = Widget_Button(properties, Value='Original Rotation', /Separator, $
   Event_Pro='Surf_Contour_Properties', UValue='ORIGINAL_T3D')

   ; Drag Quality.

dragID = Widget_Button(properties, Value='Drag Quality', /Separator, /Menu)
   dragLowID = Widget_Button(dragID, Value='Low', $
      Event_Pro='Surf_Contour_Properties', UValue='DRAG_LOW')
   dragMedID = Widget_Button(dragID, Value='Medium', $
      Event_Pro='Surf_Contour_Properties', UValue='DRAG_MEDIUM')
   dragHighID = Widget_Button(dragID, Value='High', $
      Event_Pro='Surf_Contour_Properties', UValue='DRAG_HIGH')
Widget_Control, dragHighID, Sensitive=0

   ; Create OUTPUT menu buttons for formatted output files.

output = Widget_Button(menubase, Value='Output')
gif = Widget_Button(output, Value='GIF File', $
   UValue='GIF', Event_Pro='Surf_Contour_Output')
jpeg = Widget_Button(output, Value='JPEG File', $
   UValue='JPEG', Event_Pro='Surf_Contour_Output')

Widget_Control, tlb, /Realize

    ; Get the window destination object. The view will
    ; be drawn when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

   ; Get a printer object for this graphic.

thisPrinter = Obj_New('IDLgrPrinter')

   ; Create a container object to hold all the other
   ; objects. This will make it easy to free all the
   ; objects when we are finished with the program.

thisContainer = Obj_New('IDL_Container')

   ; Add created objects to the container.

thisContainer->Add, thisView
thisContainer->Add, thisPrinter
thisContainer->Add, thisTrackball
thisContainer->Add, xTitle
thisContainer->Add, yTitle
thisContainer->Add, zTitle
thisContainer->Add, xAxis
thisContainer->Add, yAxis
thisContainer->Add, zAxis
thisContainer->Add, thisSurface
thisContainer->Add, thisLight
thisContainer->Add, fillLight
thisContainer->Add, contourModel
thisContainer->Add, thisContour
thisContainer->Add, thisModel
thisContainer->Add, colorbar
thisContainer->Add, helvetica10pt
thisContainer->Add, cbarTitle
thisContainer->Add, cbarFont
thisContainer->Add, cbarTicks

   ; Get the current transformation matrix, so it can be restored.

thisModel->GetProperty, Transform=origTransform

   ; Create an INFO structure to hold needed program information.

info = { origTransform:origTransform, $ ; The transformation matrix.
         thisContainer:thisContainer, $ ; The object container.
         thisWindow:thisWindow, $       ; The window object.
         thisPrinter:thisPrinter, $     ; The printer object.
         thisSurface:thisSurface, $     ; The surface object.
         thisTrackball:thisTrackball, $ ; The trackball object.
         thisModel:thisModel, $         ; The model object.
         thisContour:thisContour, $     ; The contour object.
         contourModel:contourModel, $   ; The contour model.
         sliderValue:zmid, $            ; The current contour slider value.
         zrange:zrange, $               ; The range of the Z axis.
         colorbar:colorbar, $           ; The colorbar object.
         xAxis:xAxis, $                 ; The X Axis object.
         yAxis:yAxis, $                 ; The Y Axis object.
         zAxis:zAxis, $                 ; The Z Axis object.
         data:data, $                   ; The Z data that is being surfaced.
         dragLowID:dragLowID, $         ; ID of Drag Quality Low button.
         dragMedID:dragMedID, $         ; ID of Drag Quality Medium button.
         dragHighID:dragHighID, $       ; ID of Drag Quality High button.
         dragQuality:2, $               ; The current drag quality.
         thisView:thisView }            ; The view object.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'surf_contour', tlb, Cleanup='Surf_Contour_Cleanup',  $
   Event_Handler='Surf_Contour_Resize', Group_Leader=groupLeader, /No_Block
END
;-------------------------------------------------------------------