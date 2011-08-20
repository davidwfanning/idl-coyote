;+
; NAME:
;       TEXTURE_SURFACE
;
; PURPOSE:
;
;       The purpose of this program is to demonstrate how to
;       create a simple surface plot with an image applied as
;       a texture in object graphics.
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
;       TEXTURE_SURFACE, data, x, y, Image=image
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
;       BORDERCOLOR : A three element array [R, G, B] describing the color
;       used to draw the non-textured part of the surface if POSITION is
;       specified.
;
;       COLORTABLE: The number of an IDL color table to use for the image
;       texture. Used only if the supplied image is 2D. Ignored otherwise.
;
;       EXACT:  Set this keyword to get exact axis scaling.
;
;       _EXTRA: This keyword collects otherwise undefined keywords that are
;        passed to the surface initialization routine.
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       IMAGE: An 8-bit or 24-bit image you wish to use for the image texture.
;
;       LANDSCAPE: Set this keyword if you are printing in landscape mode. The
;       default is Portrait mode. The Landscape keyword on the PRINTER object
;       is set, but not all printers will honor this keyword setting. If yours
;       does not, set Landscape mode in the Printer Setup dialog.
;
;       POSITION: A four element array of the form [x1, y1, x2, y2] that will
;       position the image with its lower-left corner at (x1,y1) and its upper-
;       right corner at (x2,y2) in the device coordinate system of the surface.
;       In other words, if my surface is a 41 by 41 array, and I want the image
;       positioned with lower-left at (5,10) and upper-right at (25,18), then
;       I call the program like this: Texture_Surface, Position=[5, 10, 25, 18].
;
;       VECTOR: Set this keyword if you want vector printing (as opposed to
;       the default bitmap printing).
;
;       XTITLE: A string used as the X title of the plot.
;
;       YTITLE: A string used as the Y title of the plot.
;
;       ZSCALE: A number larger than or equal to 0.001 and less than or equal to 1.0 that affects Z scaling.
;
;       ZTITLE: A string used as the Z title of the plot.
;
; COMMON BLOCKS:
;
;       None.
;
; EXAMPLE:
;
;       To use this program with your surface data and 2D image, type:
;
;        IDL> data = Loaddata(2)
;        IDL> image = Loaddata(7)
;        IDL> Texture_Surface, data, Image=image, Colortable=33
;
; RESTRICTIONS:
;
;        Requires the ASPECT program from the Coyote Library:
;
;           http://www.dfanning.com/programs/aspect.pro
;
; MODIFICATION HISTORY
;
;       Written by David W. Fanning, 1 Nov 2001, from previous Simple_Surface code.
;       Modifications suggested by Karl Shultz added to allow surface color
;          specification and improved resolution about image edges when
;          positioning images. BORDERCOLOR keyword added. DWF. 4 Nov 2001.
;       The surface now maintains the same X/Y aspect ratio as the surface data. DWF. 8 April 2002.
;       Added ZSCALE keyword. DWF. 8 April 2002.
;-
;
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
Pro Texture_Surface_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Texture_Surface_Draw_Events, event

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



PRO Texture_Surface_Style, event

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



PRO Texture_Surface_Output, event

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
      filename = Dialog_Pickfile(/Write, File='texture_surface.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='texture_surface.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END


   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='texture_surface.tif')
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


PRO Texture_Surface_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO Texture_Surface_Printing, event

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



PRO Texture_Surface_Resize, event

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



PRO Texture_Surface, surfaceData, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Group_Leader=groupLeader, $
   Hidden_Lines=hidden_lines, Vector=vector, Exact=exact, $
   Landscape=landscape, Image=image, Colortable=colortable, $
   Position=position, BorderColor=borderColor, ZScale=zscale

On_Error, 2

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
IF N_Elements(zscale) EQ 0 THEN zscale = 1.0 ELSE zscale = (0.001 > zscale < 1.0)
hidden_lines = Keyword_Set(hidden_lines)
landscape = Keyword_Set(landscape)
vector = Keyword_Set(vector)

   ; Need fake data?

IF N_Elements(surfaceData) EQ 0 THEN BEGIN
   a = Shift(-Dist(41, 41), 25, 15)
   FOR j=0,4 DO a = Smooth(a, 5)
   surfaceData = a - Min(a)
ENDIF

   ; Need an image for the texture map?

IF N_Elements(image) EQ 0 THEN BEGIN
   filename = Filepath(SubDir=['examples','data'], 'rose.jpg')
   Read_JPEG, filename, image
ENDIF

   ; Check image size.

CASE size(image, /N_Dimensions) OF
   2: IF N_Elements(colortable) EQ 0 THEN colortable = 0
   3:
   ELSE: Message, 'Image data must be either 8-bit or 24-bit.'
ENDCASE

   ; Add a border around the image when positioning image on surface.
   ; This is necessary to be able to specify the color of the rest
   ; of the surface. Note that this will displace the original image
   ; slightly. This could potentially be a problem in some applications.

IF N_Elements(position) NE 0 THEN BEGIN
    imageDims = Image_Dimensions(image, XSize=xsize, YSize=ysize, TrueIndex=trueindex)
    xsize = xsize + 1
    ysize = ysize + 1
    IF trueindex GE 0 THEN BEGIN
      imageBorder = BytArr(imageDims[trueindex], xsize, ysize)
      IF N_Elements(borderColor) NE 0 THEN BEGIN
         imageBorder[0,*,*] = Replicate(borderColor[0], xsize, ysize)
         imageBorder[1,*,*] = Replicate(borderColor[1], xsize, ysize)
         imageBorder[2,*,*] = Replicate(borderColor[2], xsize, ysize)
      ENDIF
      imageBorder[*,1:xsize-1, 1:ysize-1] = image
    ENDIF ELSE BEGIN
      temp = BytArr(3, xsize-1, ysize-1)
      IF N_Elements(colortable) NE 0 THEN LoadCT, colortable, /Silent
      TVLCT, r, g, b, /Get
      temp[0,*,*] = r[image]
      temp[1,*,*] = g[image]
      temp[2,*,*] = b[image]
      imageBorder = BytArr(3, xsize, ysize)
      IF N_Elements(borderColor) NE 0 THEN BEGIN
         imageBorder[0,*,*] = Replicate(borderColor[0], xsize, ysize)
         imageBorder[1,*,*] = Replicate(borderColor[1], xsize, ysize)
         imageBorder[2,*,*] = Replicate(borderColor[2], xsize, ysize)
      ENDIF
      imageBorder[*,1:xsize-1, 1:ysize-1] = Temporary(temp)
   ENDELSE
   border = 1
ENDIF ELSE border = 0

   ; Get dimensions of the surface data.

s = Size(surfaceData, /Dimensions)

   ; Fill out X and Y vectors if necessary.

IF N_Elements(x) EQ 0 THEN x = Findgen(s[0])
IF N_Elements(y) EQ 0 THEN y = Findgen(s[1])

    ; Create a view. Use RGB color. Charcoal background.
    ; The coodinate system is chosen so that (0,0,0) is in the
    ; center of the window. This will make rotations easier.

thisView = OBJ_NEW('IDLgrView', Color=[80,80,80], $
   Viewplane_Rect=[-1.2,-1.1,2.3,2.1])

   ; I want the surface to have the same aspect ratio as the data itself
   ; in the X and Y directions.

surfaceAspect = Float(s[1]) / s[0]
windowAspect = 1.0
pos = Aspect(surfaceAspect, WindowAspect=windowAspect, Margin=0)
pos = pos - 0.5

    ; Create a model for the surface and axes and add it to the view.
    ; This model will rotate under the direction of the trackball object.

thisModel = OBJ_NEW('IDLgrModel')
thisView->Add, thisModel

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

thisTrackball = OBJ_NEW('Trackball', [200, 200], 200)

   ; Add a palette object to the image, if the image is 2D.

dim = Size(image, /N_Dimensions)
IF dim EQ 2 THEN BEGIN
   imgpal = Obj_New('IDLgrPalette')
   imgpal->LoadCT, colortable
ENDIF ELSE imgpal = Obj_New()

   ; Create the image object.

IF border THEN $
   thisImage = Obj_New('IDLgrImage', imageBorder, Palette=imgpal) ELSE $
   thisImage = Obj_New('IDLgrImage', image, Palette=imgpal)

   ; Create the texture map coordinates. Use a sub-set if the POSITION keyword is used.

s = Size(surfaceData, /Dimensions)
IF N_Elements(position) NE 0 THEN BEGIN

      ; Normal texcoords positions.

   texcoords = FltArr(2, s[0], s[1])
   texcoords[0,position[0]:position[2],position[1]:position[3]] = (Findgen(position[2]-position[0]+1) # $
      Replicate(1,position[3]-position[1]+1)) / (position[2]-position[0])
   texcoords[1,position[0]:position[2],position[1]:position[3]] = (Replicate(1,position[3]-position[1]+1)) # $
      Findgen(position[2]-position[0]+1) / (position[2]-position[0])

      ; Extend texcoords in unused areas to prevent interpolation problems
      ; at image boundaries.

      ; Bottom (and Y for LL and LR corners).

   texcoords[0,position[0]:position[2],0:position[1]-1] = $
      (Findgen(position[2]-position[0]+1) # $
       Replicate(1,position[1])) $
       / (position[2]-position[0])
   texcoords[1,*,0:position[1]-1] = 0.0

      ; Top (and Y for UL and UR corners).

   texcoords[0,position[0]:position[2],position[3]+1:*] = $
      (Findgen(position[2]-position[0]+1) # $
       Replicate(1,s[1]-position[3]-1)) $
       / (position[2]-position[0])
   texcoords[1,*,position[3]+1:*] = 1.0

      ; Left (and X for LL and UL corners).

   texcoords[0,0:position[0]-1,*] = 0.0
   texcoords[1,0:position[0]-1,position[1]:position[3]] = $
      (Findgen(position[3]-position[1]+1) # $
       Replicate(1,position[0])) $
       / (position[3]-position[1])

      ; Right (and X for LR and UR corners).

   texcoords[0,position[2]+1:*,*] = 1.0
   texcoords[1,position[2]+1:*,position[1]:position[3]] = $
      (Findgen(position[3]-position[1]+1) # $
       Replicate(1,s[0]-position[2]-1)) $
       / (position[3]-position[1])

ENDIF ELSE BEGIN
   texcoords = FltArr(2, s[0], s[1])
   texcoords[0,*,*] = (Findgen(s[0])#Replicate(1,s[1])) / (s[0]-1)
   texcoords[1,*,*] = (Replicate(1,s[1])#Findgen(s[0])) / (s[0]-1)
ENDELSE

   ; Create a surface object. Add the image to it as a texture map.
   ; The image is positioned with the Texture_Coord keyword and the
   ; texcoords values.

thisSurface = OBJ_NEW('IDLgrSurface', surfaceData, x, y, Style=2, $
   Color=[255,255,255], _Extra=extra, Hidden_Lines=hidden_lines)
 thisSurface->SetProperty, Texture_Map=thisImage, Texture_Coord=texcoords

    ; Get the data ranges of the surface.

thisSurface->GetProperty, XRange=xrange, YRange=yrange, ZRange=zrange

    ; Create axes objects for the surface. Color them green.
    ; Axes are created after the surface so the range can be
    ; set correctly. Note how I set the font to 10 point Helvetica
    ; by creating the axis with the title object, then getting the
    ; actual axis text from the axis object itself, and switching it.

xAxis = Obj_New("IDLgrAxis", 0, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=xtitleObj, Range=xrange, Exact=Keyword_Set(exact))
xAxis->GetProperty, Ticktext=xAxisText
xAxisText->SetProperty, Font=helvetica10pt

yAxis = Obj_New("IDLgrAxis", 1, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ytitleObj, Range=yrange, Exact=Keyword_Set(exact))
yAxis->GetProperty, Ticktext=yAxisText
yAxisText->SetProperty, Font=helvetica10pt

zAxis = Obj_New("IDLgrAxis", 2, Color=[0,255,0], Ticklen=0.1, $
   Minor=4, Title=ztitleObj, Range=zrange, Exact=Keyword_Set(exact))
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

xs = FSC_Normalize(xrange, Position=[pos[0], pos[2]])
ys = FSC_Normalize(yrange, Position=[pos[1], pos[3]])
zs = FSC_Normalize(zrange, Position=[-0.5,0.5]*zscale)

    ; Scale the axes and place them in the coordinate space.
    ; Note that not all values in the Location keyword are
    ; used. (I've put really large values into the positions
    ; that are not being used to demonstate this.) For
    ; example, with the X axis only the Y and Z locations are used.

xAxis->SetProperty, Location=[9999.0, pos[1], -0.5*zscale], XCoord_Conv=xs
yAxis->SetProperty, Location=[pos[0], 9999.0, -0.5*zscale], YCoord_Conv=ys
zAxis->SetProperty, Location=[pos[0],  pos[3], 9999.0], ZCoord_Conv=zs

    ; Scale the surface. Notice the surface is scaled *AFTER* the
    ; actual data range is obtained from the axes (CRANGE, above).
    ; Failure to do this can result in inaccurate results.

thisSurface->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Add the surface and axes objects to the model.

thisModel->Add, thisSurface
thisModel->Add, xAxis
thisModel->Add, yAxis
thisModel->Add, zAxis

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
    ; or the surface will appear washed out. If you have an image
    ; as a texture map, turn it on full.

ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.2)
IF N_Elements(image) NE 0 THEN ambientLight->SetProperty, Intensity=1.0
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

tlb = Widget_Base(Title='Texture Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Texture_Surface_Draw_Events', Button_Events=1)

    ; Create FILE menu buttons for printing and exiting.

fileID = Widget_Button(menubase, Value='File')

outputID = Widget_Button(fileID, Value='Save As...', /Menu)
IF havegif THEN dummy = Widget_Button(outputID, Value='GIF File', $
   UValue='GIF', Event_Pro='FSC_Surface_Output')
dummy = Widget_Button(outputID, Value='JPEG File', $
   UValue='JPEG', Event_Pro='FSC_Surface_Output')
dummy = Widget_Button(outputID, Value='TIFF File', $
   UValue='TIFF', Event_Pro='FSC_Surface_Output')

dummy = Widget_Button(fileID, Value='Print', Event_Pro='Texture_Surface_Printing')

dummy = Widget_Button(fileID, /Separator, Value='Exit', $
   Event_Pro='Texture_Surface_Exit')

   ; Create STYLE menu buttons for surface style.

styleID = Widget_Button(menubase, Value='Style', Event_Pro='Texture_Surface_Style', /Menu)
dummy = Widget_Button(styleID, Value='Dot Surface', UValue='DOTS')
dummy = Widget_Button(styleID, Value='Wire Mesh', UValue='MESH')
dummy = Widget_Button(styleID, Value='Solid', UValue='SOLID')
dummy = Widget_Button(styleID, Value='Parallel X Lines', UValue='XPARALLEL')
dummy = Widget_Button(styleID, Value='Parallel Y Lines', UValue='YPARALLEL')
IF hidden_lines THEN hlValue = 'Hidden Lines OFF' ELSE hlValue='Hidden Lines ON'
dummy = Widget_Button(styleID, Value=hlvalue, UValue='HIDDEN')

   ; Realize the widgets.

Widget_Control, tlb, /Realize

    ; Get the window destination object, which is the value of
    ; an object draw widget. The view will be drawn in the window
    ; when the window is exposed.

Widget_Control, drawID, Get_Value=thisWindow

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
IF Obj_Valid(thisImage) THEN thisContainer->Add, thisImage
IF Obj_Valid(imgpal) THEN thisContainer->Add, imgpal

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
         drawID:drawID, $               ; The widget identifier of the draw widget.
         landscape:landscape, $         ; A flag for landscape output.
         vector:vector }                ; A flag for vector output.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'texture_surface', tlb, Cleanup='Texture_Surface_Cleanup', /No_Block, $
   Event_Handler='Texture_Surface_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------