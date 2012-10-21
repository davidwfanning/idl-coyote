;+
; NAME:
;       CHGCOLOR
;
; PURPOSE:
;       The purpose of this routine is to allow the user to change
;       the color at a particular color index. The user is able to
;       mix their own color by manipulating red, green, and blue
;       sliders. This routine is ideal for changing axes or background
;       colors of a plot, for example. The routine works on 8-bit,
;       16-bit, and 24-bit displays.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Widgets, Colors.
;
; CALLING SEQUENCE:
;       CHGCOLOR, index
;
; REQUIRED INPUTS:
;       INDEX: The color index to be changed. It must be a value
;       between 0 and 255.
;
; KEYWORD PARAMETERS:
;
;       LABEL: Text that goes next to the color window. The default is
;       "Resulting Color".
;
;       GROUP_LEADER: The group leader for this program. When the group leader
;       is destroyed, this program will be destroyed.
;
;       NOTIFYID: A a 2 column by n row array that contains the IDs of widgets
;       that should be notified when CHGCOLOR changes a color. The first
;       column of the array contains widgets that should be notified. The
;       second column contains IDs of widgets that are at the top of the
;       hierarch in which the corresponding widgets in the first column
;       are located. (The purpose of the top widget IDs is to make it
;       possible for the widget in the first column to get the "info"
;       structure of the widget program.) An CHGCOLOR_LOAD event will be
;       sent to the widget identified in the first column. The event
;       structure is defined like this:
;
;       event = {CHGCOLOR_LOAD, ID:0L, TOP:0L, HANDLER:0L, $
;          r:(!D.N_COLORS < 256), g:(!D.N_COLORS < 256), b:(!D.N_COLORS < 256)}
;
;       The ID field will be filled out with NOTIFYID(0, n) and the TOP
;       field will be filled out with NOTIFYID(1, n).
;
;       TITLE: This is the window title. It is "Modify Drawing Color" by
;       default. The program is registered with the name "chgcolor " plus
;       the TITLE string. The register name is checked before the widgets
;       are defined. This gives you the opportunity to have multiple copies
;       of CHGCOLOR operating simultaneously. (For example, one will change
;       the background color and one will change the plotting color.)
;
;       XOFFSET: This is the X offset of the program on the display. The
;       program will be placed approximately in the middle of the display
;       by default.
;
;       YOFFSET: This is the Y offset of the program on the display. The
;       program will be placed approximately in the middle of the display
;       by default.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Color at the specified index is changed. Events are sent to widgets
;       if the NOTIFYID keyword is used.
;
; RESTRICTIONS:
;       None.
;
; EXAMPLE:
;       To change the background color of a plot, type:
;
;       CHGCOLOR, !P.Background
;
;       To see a more complete example, look at the program SLICE
;       in the Coyote Software Library:
;
;          ftp://ftp.frii.com/pub/dfanning/outgoing/idl_examples
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 23 April 97.
;       12 May 97, Fixed a bug in the way colors were loaded when
;          a tracking event occurred. DWF
;       13 May 97, Added a JUST_REGISTER keyword and set it up for
;          running in IDL 5.0 as a non-blocking widget.
;       27 Sept 98. Fixed problems caused by IDL 5.1 color changes.
;       27 Sept 98. Removed JUST_REGISTER keyword. Made widget non-blocking.
;       03 Nov 98. Modified layout and added slider ganging. DWF.
;-


PRO CHGCOLOR_GANG, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY
CASE info.gang OF
   0: info.gang = 1
   1: info.gang = 0
ENDCASE
WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of CHGCOLOR_GANG event handler *************************************



PRO CHGCOLOR_SLIDERS, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; If sliders are ganged, move all of them.

IF info.gang THEN BEGIN
   WIDGET_CONTROL, event.id, GET_VALUE=thisValue
   WIDGET_CONTROL, info.redID, SET_VALUE=thisValue
   WIDGET_CONTROL, info.greenID, SET_VALUE=thisValue
   WIDGET_CONTROL, info.blueID, SET_VALUE=thisValue
ENDIF

   ; Get the values from the sliders and load them.

WIDGET_CONTROL, info.redID, GET_VALUE=rThis
WIDGET_CONTROL, info.greenID, GET_VALUE=gThis
WIDGET_CONTROL, info.blueID, GET_VALUE=bThis
TVLCT, rThis, gThis, bThis, info.index
TVLCT, r, g, b, /Get

Device, Get_Visual_Depth=thisDepth
IF thisDepth GT 8 THEN BEGIN
   WSET, info.wid
   TV, info.colorimage
ENDIF

   ; Are there widgets to notify?

s = SIZE(info.notifyID)
IF s(0) EQ 1 THEN count = 0 ELSE count = s(2)-1
FOR j=0,count DO BEGIN
   colorEvent = { CHGCOLOR_LOAD, $
                  ID:info.notifyID(0,j), $
                  TOP:info.notifyID(1,j), $
                  HANDLER:0L, $
                  R:r, $
                  G:g, $
                  B:b }
   IF Widget_Info(info.notifyID(0,j), /Valid_ID) THEN $
      Widget_Control, info.notifyID(0,j), Send_Event=colorEvent
ENDFOR

   ; Make REVERT button sensitive.

WIDGET_CONTROL, info.original, SENSITIVE=1

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of CHGCOLOR_SLIDERS event handler *********************


PRO CHGCOLOR_PROTECT_COLORS, event

   ; Get the info structure from storage location.

IF event.enter NE 1 THEN RETURN

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Load the current color on the sliders.

WIDGET_CONTROL, info.redID, GET_VALUE=rThis
WIDGET_CONTROL, info.greenID, GET_VALUE=gThis
WIDGET_CONTROL, info.blueID, GET_VALUE=bThis
TVLCT, rThis, gThis, bThis, info.index

   ; Put the info structure back in storage location.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; ******************************************************************



PRO CHGCOLOR_QUIT, event
WIDGET_CONTROL, event.top, /DESTROY
END ; of CHGCOLOR_QUIT event handler ***********************************


PRO CHGCOLOR_ORIGINAL, event
WIDGET_CONTROL, event.top, GET_UVALUE=info, /NO_COPY

   ; Load original color.

TVLCT, info.rThis, info.gThis, info.bThis, info.index
TVLCT, r, g, b, /Get
Device, Get_Visual_Depth=thisDepth
IF thisDepth GT 8 THEN BEGIN
   WSET, info.wid
   TV, info.colorimage
ENDIF

   ; Are there widgets to notify?

s = SIZE(info.notifyID)
IF s(0) EQ 1 THEN count = 0 ELSE count = s(2)-1
FOR j=0,count DO BEGIN
   colorEvent = { CHGCOLOR_LOAD, $
                  ID:info.notifyID(0,j), $
                  TOP:info.notifyID(1,j), $
                  HANDLER:0L, $
                  R:r, $
                  G:g, $
                  B:b }
   IF Widget_Info(info.notifyID(0,j), /Valid_ID) THEN $
      Widget_Control, info.notifyID(0,j), Send_Event=colorEvent
ENDFOR

   ; Update the sliders.

WIDGET_CONTROL, info.redID, SET_VALUE=info.rThis
WIDGET_CONTROL, info.greenID, SET_VALUE=info.gThis
WIDGET_CONTROL, info.blueID, SET_VALUE=info.bThis

   ; Make REVERT button insensitive.

WIDGET_CONTROL, info.original, SENSITIVE=0

WIDGET_CONTROL, event.top, SET_UVALUE=info, /NO_COPY
END ; of CHGCOLOR_ORIGINAL event handler ******************************



PRO CHGCOLOR, index, LABEL=labeltext, GROUP_LEADER=group, $
   XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, NOTIFYID=notifyid, $
   JUST_REGISTER=just_register

   ; Procedure to change the color of a specified index.

ON_ERROR, 1
np = N_PARAMS()
IF np EQ 0 THEN MESSAGE, 'Must pass a color index as argument.'
IF index LT 0 OR index GT 255 THEN $
   MESSAGE, 'Color index parameter must be in range 0-255.'
IF N_ELEMENTS(labeltext) EQ 0 THEN labeltext = 'Resulting Color'
IF N_ELEMENTS(title) EQ 0 THEN title = 'Modify Drawing Color'
IF N_ELEMENTS(notifyID) EQ 0 THEN notifyid = [-1L, -1L]
registerTitle = 'chgcolor ' + title

   ; Find the center of the display.

DEVICE, GET_SCREEN_SIZE=screenSize
xCenter = FIX(screenSize(0) / 2.0)
yCenter = FIX(screenSize(1) / 2.0)

IF N_ELEMENTS(xoffset) EQ 0 THEN xoffset = xCenter - 75
IF N_ELEMENTS(yoffset) EQ 0 THEN yoffset = yCenter - 50

   ; Get the current color.

TVLCT, r, g, b, /GET
rThis = r(index)
gThis = g(index)
bThis = b(index)

   ; Make sure only one program with this register title on display.

IF XREGISTERED(registerTitle) THEN RETURN


tlb = WIDGET_BASE(COLUMN=1, TLB_FRAME_ATTR=1, BASE_ALIGN_CENTER=1,$
   XOFFSET=xoffset, YOFFSET=yoffset, TITLE=title, TRACKING=1)
colorbase = WIDGET_BASE(COLUMN=1, tlb,SCR_XSIZE=256, /FRAME)
label = WIDGET_LABEL(colorbase, VALUE=labeltext, ALIGN_CENTER=1)
drawID = WIDGET_DRAW(colorbase, XSIZE=50, YSIZE=30, ALIGN_CENTER=1)

sliderbase = WIDGET_BASE(tlb, COLUMN=1, FRAME=1, BASE_ALIGN_CENTER=1, $
   EVENT_PRO='CHGCOLOR_SLIDERS')
label = WIDGET_LABEL(sliderbase, VALUE='Mix Primary Colors')

redID = WIDGET_SLIDER(sliderbase, SCR_XSIZE=250, VALUE=rThis, $
   MAX=255, MIN=0, Title='Red')
greenID = WIDGET_SLIDER(sliderbase, SCR_XSIZE=250, VALUE=gThis, $
   MAX=255, MIN=0, Title='Green')
blueID = WIDGET_SLIDER(sliderbase, SCR_XSIZE=250, VALUE=bThis, $
   MAX=255, MIN=0, Title='Blue')

exbutbase = WIDGET_BASE(sliderbase, SCR_XSIZE=250, /NonExclusive)
exbutton = WIDGET_BUTTON(exbutbase, Value='Gang Sliders', $
   EVENT_PRO='CHGCOLOR_GANG', ALIGN_CENTER=1)

buttonbase = WIDGET_BASE(tlb, ROW=1, Align_Center=1)
original = WIDGET_BUTTON(buttonbase, VALUE='Cancel', $
   EVENT_PRO='CHGCOLOR_ORIGINAL')
quitter = WIDGET_BUTTON(buttonbase, VALUE='Accept', $
   EVENT_PRO='CHGCOLOR_QUIT')

WIDGET_CONTROL, tlb, /REALIZE
WIDGET_CONTROL, original, SENSITIVE=0
WIDGET_CONTROL, drawID, GET_VALUE=wid
WSET, wid

   ; Draw the color in the draw widget.

colorimage = REPLICATE(1B, 50, 30) * BYTE(index)
TV, colorimage

info = { wid:wid, $           ; Draw widget window index number.
         redID:redID, $       ; ID of red slider.
         greenID:greenID, $   ; ID of green slider.
         blueID:blueID, $     ; ID of blue slider.
         notifyID:notifyID, $ ; Widgets to notify of color change.
         original:original, $ ; ID of the "Revert" button.
         colorimage:colorimage, $
         index:index, $       ; Index of color you want to change.
         gang:0, $            ; Flag to indicate if sliders are ganged.
         rThis:rthis, $       ; Current red value of indexed color.
         gThis:gThis, $       ; Current green value of indexed color.
         bThis:bThis}         ; Current blue value of indexed color.

WIDGET_CONTROL, tlb, SET_UVALUE=info, /NO_COPY
XMANAGER, registerTitle, tlb, GROUP_LEADER=group, $
      EVENT_HANDLER='CHGCOLOR_PROTECT_COLORS', /NO_BLOCK
END


