;+
; NAME:
;       LIGHT_SURFACE
;
; PURPOSE:
;
;       The purpose of this program is to demonstrate how to
;       create a simple surface plot with axes and rotational
;       capability in object graphics. Four lights are used and
;       the ability to manipulate the lights is provided via the
;       Light Controls menu item under the File menu.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       2642 Bradbury Court
;       Fort Collins, CO 80521 USA
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
;       Light_Surface, data, x, y
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
;       SOLID: Set this keyword if you wish the surface to be a shaded surface
;       when the program starts up. It will be a wire-mesh surface by default.
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
; DEPENDENCIES:
;
;       Requires the ASPECT program from the Coyote Library:
;
;           http://www.dfanning.com/programs/aspect.pro
;
; EXAMPLE:
;
;       To use this program with your 2D data, type:
;
;           IDL> Light_Surface, data
;-


FUNCTION COLOR24, number

   ; This FUNCTION accepts a [red, green, blue] triple that
   ; describes a particular color and returns a 24-bit long
   ; integer that is equivalent to that color. The color is
   ; described in terms of a hexidecimal number (e.g., FF206A)
   ; where the left two digits represent the blue color, the
   ; middle two digits represent the green color, and the right
   ; two digits represent the red color.
   ;
   ; The triple can be either a row or column vector of 3 elements.

ON_ERROR, 1

IF N_ELEMENTS(number) NE 3 THEN $
   MESSAGE, 'Augument must be a three-element vector.'

IF MAX(number) GT 255 OR MIN(number) LT 0 THEN $
   MESSAGE, 'Argument values must be in range of 0-255'

base16 = [[1L, 16L], [256L, 4096L], [65536L, 1048576L]]

num24bit = 0L

FOR j=0,2 DO num24bit = num24bit + ((number(j) MOD 16) * base16(0,j)) + $
   (Fix(number(j)/16) * base16(1,j))

RETURN, num24bit
END ; ************************  of COLOR24  ******************************



FUNCTION GETCOLOR, thisColor, index, TRUE=truecolor, $
   NAMES=colornames, LOAD=load, START=start, INDEXED=indexedcolor

   ; Set up the color vectors.

names  = ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
rvalue = [  0,        255,       0,      255,       0  ]
gvalue = [  0,          0,     255,      255,     255  ]
bvalue = [  0,        255,     255,        0,       0  ]
names  = [names,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
rvalue = [rvalue,  255,     0,      0,    255,    112]
gvalue = [gvalue,    0,     0,      0,    127,    219]
bvalue = [bvalue,    0,   255,    115,    127,    147]
names  = [names,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray','White']
rvalue = [rvalue,   219,      0,     255,       80,      135,    255  ]
gvalue = [gvalue,   112,    163,     171,       80,      135,    255  ]
bvalue = [bvalue,   219,    255,     127,       80,      135,    255  ]

   ; Did the user ask for a specific color? If not, return
   ; all the colors. If the user asked for a specific color,
   ; find out if a 24-bit value is required. Return to main
   ; IDL level if an error occurs.

ON_Error, 1
np = N_Params()
IF Keyword_Set(start) EQ 0 THEN start = !D.TABLE_SIZE - 17

   ; User ask for the color names?

IF Keyword_Set(colornames) THEN RETURN, names ELSE names = StrUpCase(names)

   ; If no positional parameter, return all colors.

IF np EQ 0 THEN BEGIN

   ; Did the user want a 24-bit value? If so, call COLOR24.

   IF Keyword_Set(trueColor) THEN BEGIN
      returnColor = LonArr(16)
      FOR j=0,15 DO returnColor[j] = Color24([rvalue[j], gvalue[j], bvalue[j]])

         ; If LOAD keyword set, return a color structure.

      IF Keyword_Set(load) THEN BEGIN
         returnValue = Create_Struct('black', returnColor[0])
         FOR j=1,15 DO returnValue = Create_Struct(returnValue, names[j], returnColor[j])
         returnColor = returnValue
      ENDIF

      RETURN, returnColor
   ENDIF

   ; If color decomposition is ON and INDEXED is not set, return 24-bit values.

   IF Float(!Version.Release) GE 5.2 THEN BEGIN
      IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
         Device, Get_Decomposed=decomposedState
      ENDIF ELSE decomposedState = 0
      IF Keyword_Set(indexedcolor) THEN decomposedState = 0
      IF decomposedState EQ 1 THEN BEGIN
         returnColor = LonArr(16)
         FOR j=0,15 DO returnColor[j] = Color24([rvalue[j], gvalue[j], bvalue[j]])
         IF Keyword_Set(load) THEN BEGIN
            returnValue = Create_Struct('black', returnColor[0])
            FOR j=1,15 DO returnValue = Create_Struct(returnValue, names[j], returnColor[j])
            RETURN, returnValue
         ENDIF
         RETURN, returnColor
      ENDIF

      IF Keyword_Set(load) THEN BEGIN
         TVLCT, Reform([rvalue, gvalue, bvalue], 16, 3), start
         returnValue = Create_Struct('black', start)
         FOR j=1,15 DO returnValue = Create_Struct(returnValue, names[j], start+j)
         RETURN, returnValue
      ENDIF

      returnColor = REFORM([rvalue, gvalue, bvalue], 16, 3)
      RETURN, returnColor

   ENDIF

   IF Keyword_Set(load) THEN BEGIN
      TVLCT, Reform([rvalue, gvalue, bvalue], 16, 3), start
      returnValue = Create_Struct('black', start)
      FOR j=1,15 DO returnValue = Create_Struct(returnValue, names[j], start+j)
      RETURN, returnValue
   ENDIF

   returnColor = REFORM([rvalue, gvalue, bvalue], 16, 3)
   RETURN, returnColor

ENDIF

   ; Check synonyms of colors.

IF StrUpCase(thisColor) EQ 'GREY' THEN thisColor = 'GRAY'
IF StrUpCase(thisColor) EQ 'AQUAMARINE' THEN thisColor = 'AQUA'
IF StrUpCase(thisColor) EQ 'SKYBLUE' THEN thisColor = 'SKY'

   ; Make sure the parameter is an uppercase string.

varInfo = SIZE(thisColor)
IF varInfo(varInfo(0) + 1) NE 7 THEN $
   MESSAGE, 'The color name must be a string.'
thisColor = STRUPCASE(thisColor)

   ; Get the color triple for this color.

colorIndex = WHERE(names EQ thisColor)

   ; If you can't find it. Issue an infomational message,
   ; set the index to a YELLOW color, and continue.

IF colorIndex(0) LT 0 THEN BEGIN
   MESSAGE, "Can't find color. Returning YELLOW.", /INFORMATIONAL
   colorIndex = 3
ENDIF

   ; Get the color triple.

r = rvalue(colorIndex)
g = gvalue(colorIndex)
b = bvalue(colorIndex)
returnColor = REFORM([r, g, b], 1, 3)

   ; Did the user want a 24-bit value? If so, call COLOR24.

IF KEYWORD_SET(trueColor) THEN BEGIN
   returnColor = COLOR24(returnColor)
   RETURN, returnColor
ENDIF

   ; If color decomposition is ON and INDEXED is OFF,, return 24-bit value.

IF Float(!Version.Release) GE 5.2 THEN BEGIN

   IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
      Device, Get_Decomposed=decomposedState
   ENDIF ELSE decomposedState = 0
   IF Keyword_Set(indexedcolor) THEN decomposedState = 0

   IF decomposedState EQ 1 THEN BEGIN

         ; Before you change return color, load index if requested.

      IF N_Elements(index) NE 0 THEN BEGIN
         index = 0 > index < (!D.Table_Size-1)
         TVLCT, returnColor, index
      ENDIF

      returnColor = COLOR24(returnColor)
      RETURN, returnColor
   ENDIF
ENDIF

   ; Did the user specify a color index? If so, load it.

IF N_Elements(index) NE 0 THEN BEGIN
   index = 0 > index < (!D.Table_Size-1)
   TVLCT, returnColor, index
   returnColor = index
   RETURN, returnColor
ENDIF

   ; Did the user specify INDEXED color? If so, load it.

IF Keyword_Set(indexedColor) THEN BEGIN
   TVLCT, returnColor, !P.Color
   returnColor = !P.Color < (!D.Table_Size -1)
   RETURN, returnColor
ENDIF

RETURN, returnColor
END ;-------------------------------------------------------------------------



PRO PickColor_CenterTLB, tlb

Device, Get_Screen_Size=screenSize
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize
END ;---------------------------------------------------------------------------



PRO PickColor_Select_Color, event

; This event handler permits color selection by clicking on a color window.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the color names from the window you clicked on.

Widget_Control, event.id, Get_UValue=thisColorName

   ; Get the color value and load it as the current color.

WSet, info.currentWID
thisColor = GetColor(thisColorName)
info.currentName = thisColorName
TVLCT, thisColor, info.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=info.currentColor

IF info.needSliders THEN BEGIN

      ; Update the slider values to this color value.

   Widget_Control, info.redID, Set_Value=thisColor[0,0]
   Widget_Control, info.greenID, Set_Value=thisColor[0,1]
   Widget_Control, info.blueID, Set_Value=thisColor[0,2]

ENDIF

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------


PRO PickColor_Sliders, event

; This event handler allows the user to mix their own color.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the color slider values.

Widget_Control, info.redID, Get_Value=red
Widget_Control, info.greenID, Get_Value=green
Widget_Control, info.blueID, Get_Value=blue

   ; Load the new color as the current color.

WSet, info.currentWID
TVLCT, red, green, blue, info.currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=info.currentColor

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;---------------------------------------------------------------------------



PRO PickColor_Buttons, event

; This event handler responds to CANCEL and ACCEPT buttons.

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF

   'Cancel': BEGIN
      TVLCT, info.r_old, info.g_old, info.b_old ; Restore old color table.
      Widget_Control, event.top, /Destroy       ; Exit.
      ENDCASE

   'Accept': BEGIN
      TVLCT, r, g, b, /Get ; Get the new color table.
      TVLCT, info.r_old, info.g_old, info.b_old ; Restore old color table.

         ; Save the new color in the form info pointer.

      *(info.ptr) = {cancel:0.0, r:r[info.currentColor], g:g[info.currentColor], $
         b:b[info.currentColor], name:info.currentName}
      Widget_Control, event.top, /Destroy ; Exit

      ENDCASE
ENDCASE
END ;---------------------------------------------------------------------------



FUNCTION PickColor, currentColor, StartIndex=startIndex, Title=title, $
   Group_Leader=groupLeader, Cancel=cancelled, Name=name

   ; Device must support windows.

IF (!D.FLAGS AND 256) EQ 0 THEN BEGIN
   Message, 'Device must support windows. Returning...', /Continue
   cancelled = 1
   RETURN, -1
ENDIF

   ; Working in decomposed color space with defined number of colors.
   ; Save decomposed state and restore it, if possible.

IF Float(!Version.Release) GE 5.2 THEN BEGIN
   Device, Get_Decomposed=decomposedState
ENDIF ELSE decomposedState = 0

Device, Decomposed=0
NCOLORS = 16

   ; Check parameters.

IF N_Elements(title) EQ 0 THEN title = 'Pick a Color'
IF N_Elements(startIndex) EQ 0 THEN startIndex = !D.Table_Size - (NCOLORS + 1)
startIndex = startIndex < (!D.Table_Size - (NCOLORS + 1))
IF N_Elements(currentColor) EQ 0 THEN currentColor = (startIndex + NCOLORS - 1)
currentColor = currentColor < 255
IF Keyword_Set(name) THEN needSliders = 0 ELSE needSliders = 1

   ; Get the current color tables so they can be restored on exit.

TVLCT, r_old, g_old, b_old, /Get

   ; Load the new drawing colors and get their names.

names =  ['Black', 'Magenta', 'Cyan', 'Yellow', 'Green']
red =    [  0,        255,       0,      255,       0  ]
green =  [  0,          0,     255,      255,     255  ]
blue =   [  0,        255,     255,        0,       0  ]
names =  [names,  'Red', 'Blue', 'Navy', 'Pink', 'Aqua']
red =    [red,     255,     0,      0,    255,    112]
green =  [green,     0,     0,      0,    127,    219]
blue =   [blue,      0,   255,    115,    127,    147]
names =  [names,  'Orchid', 'Sky', 'Beige', 'Charcoal', 'Gray', 'White']
red =    [red,      219,      0,     255,       80,      135,     255  ]
green =  [green,    112,    163,     171,       80,      135,     255  ]
blue =   [blue,     219,    255,     127,       80,      135,     255  ]
colorNames = names
nameIndex = currentColor - startIndex
IF nameIndex GE 0 AND nameIndex LE N_Elements(colorNames) THEN BEGIN
   currentName = colorNames[nameIndex]
ENDIF ELSE currentName = ""
TVLCT, red, green, blue, startIndex
TVLCT, r, g, b, /Get

   ; Create the widgets. TLB is MODAL or BLOCKING.

IF N_Elements(groupLeader) EQ 0 THEN BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=title, Column=1, /Base_Align_Center, /Modal, $
      Group_Leader=groupLeader)
ENDELSE

colorbaseID = Widget_Base(tlb, Row=2, Event_Pro='PickColor_Select_Color')
drawID = LonArr(16)
FOR j=0,NCOLORS-1 DO BEGIN
   drawID[j] = Widget_Draw(colorbaseID, XSize=20, YSize=15, $
      UValue=colorNames[j], Button_Events=1)
ENDFOR
currentID = Widget_Base(tlb, Column=1, Base_Align_Center=1)
labelID = Widget_Label(currentID, Value='Current Color')
currentColorID = Widget_Draw(currentID, XSize=60, YSize=15)

IF needSliders THEN BEGIN

   sliderbase = Widget_Base(tlb, COLUMN=1, FRAME=1, BASE_ALIGN_CENTER=1, $
      EVENT_PRO='PickColor_Sliders')
   label = Widget_Label(sliderbase, Value='Specify a Color')

      ; Set the current color values in sliders.

   redID = Widget_Slider(sliderbase, Scr_XSize=200, Value=r[currentColor], $
      Max=255, Min=0, Title='Red')
   greenID = Widget_Slider(sliderbase, Scr_XSize=200, Value=g[currentColor], $
      Max=255, Min=0, Title='Green')
   blueID = Widget_Slider(sliderbase, Scr_XSize=200, Value=b[currentColor], $
      Max=255, Min=0, Title='Blue')

ENDIF ELSE BEGIN

   redID = 0L
   greenID = 0L
   blueID = 0L

ENDELSE

buttonbase = Widget_Base(tlb, ROW=1, Align_Center=1, Event_Pro='PickColor_Buttons')
cancelID = Widget_Button(buttonbase, VALUE='Cancel')
acceptID = Widget_Button(buttonbase, VALUE='Accept')

   ; Center the TLB.

PickColor_CenterTLB, tlb
Widget_Control, tlb, /Realize

   ; Load the drawing colors.

wids = IntArr(NCOLORS)
FOR j=0, NCOLORS-1 DO BEGIN
   Widget_Control, drawID[j], Get_Value=thisWID
   wids[j] = thisWID
   WSet, thisWID
   PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=startIndex + j
ENDFOR

   ; Load the current color.

Widget_Control, currentColorID, Get_Value=currentWID
WSet, currentWID
TVLCT, r[currentColor], g[currentColor], b[currentColor], currentColor
PolyFill, [0,0,1,1,0], [0,1,1,0,0], /Normal, Color=currentColor

   ; Pointer to hold the form information.

ptr = Ptr_New({cancel:1.0, r:r_old[currentColor], g:g_old[currentColor], $
         b:b_old[currentColor], name:currentName})

   ; Info structure for program information.

info = { ptr:ptr, $                    ; The pointer to the form information.
         r_old:r_old, $                ; The old color table.
         g_old:g_old, $
         b_old:b_old, $
         r:r, $                        ; The new color table.
         g:g, $
         b:b, $
         needSliders:needSliders, $    ; A flag that indicates if sliders are needed.
         redID:redID, $                ; The IDs of the color sliders.
         greenID:greenID, $
         blueID:blueID, $
         currentName:currentName, $    ; The current color name.
         currentColor:currentColor, $  ; The current color index.
         currentWID:currentWID, $      ; The current color window index number.
         wids:wids $                   ; The window index number of the drawing colors.
       }

Widget_Control, tlb, Set_UValue=info, /No_Copy
XManager, 'pickcolor', tlb ; Block here until widget program is destroyed.

   ; Retrieve the color information.

colorInfo = *ptr
Ptr_Free, ptr
cancelled = colorInfo.cancel

   ; Restore decomposed state if possible.

IF Float(!Version.Release) GE 5.2 THEN Device, Decomposed=decomposedState

   ; Return the color triple.

IF Keyword_Set(name) THEN RETURN, colorInfo.name ELSE $
   RETURN, Reform([colorInfo.r, colorInfo.g, colorInfo.b], 1, 3)
END ;---------------------------------------------------------------------------------



FUNCTION ASPECT, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

ON_ERROR, 1

   ; Check for aspect ratio parameter and possibilities.

IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0

IF aspectRatio EQ 0 THEN BEGIN
   MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
   aspectRatio = 1.0
ENDIF

s = SIZE(aspectRatio)
IF s(s(0)+1) NE 4 THEN $
   MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational

   ; Check for margins.

IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15

   ; Error checking.

IF margin LT 0 OR margin GE 0.5 THEN $
   MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'

   ; Calculate the aspect ratio of the current window.

IF N_Elements(wAspectRatio) EQ 0 THEN wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE

   ; Calculate normalized positions in window.

IF (aspectRatio LE wAspectRatio) THEN BEGIN
   xstart = margin
   ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
   xend = 1.0 - margin
   yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
ENDIF ELSE BEGIN
   xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
   ystart = margin
   xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
   yend = 1.0 - margin
ENDELSE

position = [xstart, ystart, xend, yend]

RETURN, position
END


Function COYOTE_Field_ReturnValue, inputValue, dataType

; This utility routine takes a string and turns it into a number,
; depending upon the required data type.

ON_IOERROR, CatchIt

IF (Byte(inputValue))[0] EQ 32B THEN RETURN, ""

CASE dataType OF
   'INT': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Fix(inputValue)
   'LONG': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Long(inputValue)
   'FLOAT' : IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Float(inputValue)
   'DOUBLE': IF inputValue EQ "" OR inputValue EQ "-" OR inputValue EQ "+" THEN $
      retValue = 'NULLVALUE' ELSE retValue = Double(inputValue)
   'STRING' : retValue = inputValue
ENDCASE
RETURN, retValue

CatchIt:
   retValue = 'NULLVALUE'
   RETURN, retValue
END ;----------------------------------------------------------------------------



Function COYOTE_Field_Validate, value, dataType, Decimal=decimal, Digits=digits, $
   Positive=positive

; This function eliminates illegal characters from a string that represents
; a number. The return value is a properly formatted string that can be turned into
; an INT, LONG, FLOAT, or DOUBLE value.
;
; + 43B
; - 45B
; . 46B
; 0 - 9 48B -57B
; 'eEdD' [101B, 69B, 100B, 68B]

   ; A null string should be returned at once.

IF N_Elements(value) EQ 0 THEN value = ""
value = value[0]
IF value EQ "" THEN RETURN, String(value)

   ; No leading or trainnig blank characters to evaluate.

value = StrTrim(value, 2)

IF N_Elements(dataType) EQ 0 THEN dataType = 'STRING'

   ; A string value should be returned at once. Nothing to check.

IF StrUpCase(dataType) EQ 'STRING' THEN RETURN, value

   ; Check integers and longs. A "-" or "+" in the first character is allowed. Otherwise,
   ; only number between 0 and 9, or 43B to 57B.

IF StrUpCase(dataType) EQ 'INT' OR StrUpCase(dataType) EQ 'LONG' THEN BEGIN

   returnValue = Ptr_New(/Allocate_Heap)
   asBytes = Byte(value)
   IF positive THEN BEGIN
      IF (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) THEN *returnValue = [asBytes[0]]
   ENDELSE
   length = StrLen(asBytes)
   IF length EQ 1 THEN BEGIN
      IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
            *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      FOR j=1,length-1 DO BEGIN
         IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
            IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
               *returnValue = [*returnValue, asBytes[j]]
         ENDIF
      ENDFOR
  ENDELSE
  IF N_Elements(*returnValue) NE 0 THEN retValue = String(*returnValue) ELSE retValue = ""
  Ptr_Free, returnValue

      ; Check for digit restrictions.

  IF digits GT 0 THEN BEGIN
      retValue = StrTrim(retValue, 2)
      IF StrMid(retValue, 0, 1) EQ "-" THEN digits = digits + 1
      retValue = StrMid(retValue, 0, digits)
  ENDIF

  RETURN, retValue

ENDIF

   ; Check floating and double values. (+,-) in first character or after 'eEdD'.
   ; Only numbers, signs, decimal points, and 'eEdD' allowed.

IF StrUpCase(dataType) EQ 'FLOAT' OR StrUpCase(dataType) EQ 'DOUBLE' THEN BEGIN
   returnValue = Ptr_New(/Allocate_Heap)
   asBytes = Byte(value)
   IF positive THEN BEGIN
      IF (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
         (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
      IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
   ENDIF ELSE BEGIN
      IF (asBytes[0] EQ 45B) OR (asBytes[0] EQ 43B) OR $
         (asBytes[0] GE 48B AND asBytes[0] LE 57B) OR $
         (asBytes[0] EQ 46B) THEN *returnValue = [asBytes[0]]
      IF (asBytes[0] EQ 46B) THEN haveDecimal = 1 ELSE haveDecimal = 0
   ENDELSE
   haveExponent = 0
   length = StrLen(asBytes)
   prevByte = asBytes[0]
   exponents = Byte('eEdD')
   IF length EQ 1 THEN BEGIN
      IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [32B] ELSE $
            *returnValue = [asBytes[0]]
   ENDIF ELSE BEGIN
      FOR j=1,length-1 DO BEGIN
         IF (asBytes[j] GE 48B AND asBytes[j] LE 57B) THEN BEGIN
            IF N_Elements(*returnValue) EQ 0 THEN  *returnValue = [asBytes[j]] ELSE $
               *returnValue = [*returnValue, asBytes[j]]
            prevByte = asBytes[j]
         ENDIF ELSE BEGIN

            ; What kind of thing is it?

            IF (asBytes[j] EQ 46B) THEN BEGIN ; A decimal point.
               IF haveDecimal EQ 0 THEN BEGIN
                  *returnValue = [*returnValue, asBytes[j]]
                  haveDecimal = 1
                  prevByte = asBytes[j]
               ENDIF
            ENDIF

            IF (asBytes[j] EQ 45B) OR (asBytes[j] EQ 43B) THEN BEGIN ; A + or - sign.
               index = Where(exponents EQ prevByte, count)
               IF count EQ 1 AND haveExponent THEN BEGIN
                  *returnValue = [*returnValue, asBytes[j]]
                  haveDecimal = 1
                  prevByte = asBytes[j]
               ENDIF
            ENDIF

            index = Where(exponents EQ asBytes[j], count)
            IF count EQ 1 AND haveExponent EQ 0 THEN BEGIN ; An exponent
               *returnValue = [*returnValue, asBytes[j]]
               haveExponent = 1
               prevByte = asBytes[j]
            ENDIF
         ENDELSE
      ENDFOR
   ENDELSE
   IF N_Elements(*returnValue) NE 0 THEN BEGIN

      retValue = String(*returnValue)
      retValue = StrTrim(retValue, 2)

               ; Check for decimal restrictions

      IF decimal GE 0 THEN BEGIN
         theDecimalPt = StrPos(retValue, '.')
         IF theDecimalPt NE -1 THEN retValue = StrMid(retValue, 0, theDecimalPt + decimal + 1)
      ENDIF

   ENDIF ELSE retValue = ""
   Ptr_Free, returnValue

      ; Is this a representable number?

   testValue = COYOTE_Field_ReturnValue(retValue, dataType)
   IF String(testValue) NE 'NULLVALUE' THEN numCheck = Finite(testValue) ELSE numCheck = 1
   IF numCheck THEN BEGIN
      RETURN, retValue
   ENDIF ELSE BEGIN
      Message, 'The requested number is not representable.', /Informational
      RETURN, ""
   ENDELSE
ENDIF

END ;----------------------------------------------------------------------------



Pro COYOTE_Field__Define

; The COYOTE_Field Event Structure.

   event = { COYOTE_FIELD, $      ; The name of the event structure.
             ID: 0L, $            ; The ID of the compound widget's top-level base.
             TOP: 0L, $           ; The widget ID of the top-level base of the hierarchy.
             HANDLER: 0L, $       ; The event handler ID. Filled out by IDL.
             Value: Ptr_New(), $  ; A pointer to the widget value.
             Type:"" $            ; A string indicating the type of data in the VALUE field.
           }                      ; Values are "INT", "LONG", "FLOAT", "DOUBLE", or "STRING".

END ;----------------------------------------------------------------------------



Pro COYOTE_Field_Kill_Notify, ID

; This routine cleans up the pointer when the compound widget is destroyed.

Widget_Control, ID, Get_UValue=info, /No_Copy
Ptr_Free, info.theValue
END ;----------------------------------------------------------------------------



Pro COYOTE_Field_Set_Value, cw_tlb, value

; This procedure sets a value for the compound widget. The value
; is a value appropriate for the data type or a string.

   ; Get info structure.

info_carrier = Widget_Info(cw_tlb, Find_by_UName='INFO_CARRIER')
Widget_Control, info_carrier, Get_UValue=info, /No_Copy

   ; Validate the value.

theText = Strtrim(value, 2)
theText = COYOTE_Field_Validate(theText, info.dataType, Decimal=info.decimal, $
   Digits=info.digits, Positive=info.positive)

   ; Load the value in the widget.

Widget_Control, info.textID, Set_Value=theText, Set_Text_Select=[StrLen(theText),0]
info.theText = theText

   ; Set the actual value of the compound widget.

*info.theValue = COYOTE_Field_ReturnValue(info.theText, info.dataType)
Widget_Control, info_carrier, Set_UValue=info, /No_Copy
END ;----------------------------------------------------------------------------



Function COYOTE_Field_Get_Value, cw_tlb

; This function returns the numerical or string value of the
; compound widget.

info_carrier = Widget_Info(cw_tlb, Find_by_UName='INFO_CARRIER')
Widget_Control, info_carrier, Get_UValue=info, /No_Copy
value = info.theValue
Widget_Control, info_carrier, Set_UValue=info, /No_Copy
RETURN, value
END ;----------------------------------------------------------------------------




PRO COYOTE_Field_Event_Handler, event

; The main event handler for the compound widget.

   ; Get the info structure. Get the previous text, the current
   ; cursor location in the text widget, and indicate this is not
   ; a Carriage Return event.

Widget_Control, event.ID, Get_UValue=info, /No_Copy
previousText = info.theText
textLocation = Widget_Info(event.id, /Text_Select)
cr_event = 0

   ; What kind of event is this?

possibleTypes = ['INSERT SINGLE CHARACTER', 'INSERT MULTIPLE CHARACTERS', 'DELETE TEXT', 'SELECT TEXT']
thisType = possibleTypes[event.type]

   ; Branch on event type.

CASE thisType OF

   'INSERT SINGLE CHARACTER': BEGIN

            ; Get the current contents of text widget. Validate it.

         Widget_Control, info.textID, Get_Value=newText
         newText = newText[0]
         validText = COYOTE_Field_Validate(newText, info.dataType, Decimal=info.decimal, $
               Digits=info.digits, Positive=info.positive)

            ; If it is valid, leave it alone. If not, go back to previous text.

         IF validText NE newText THEN BEGIN
            Widget_Control, info.textID, Set_Value=previousText, Set_Text_Select=[textLocation[0]-1,0]
         ENDIF ELSE BEGIN
            info.theText = validText
            testValue  = COYOTE_Field_ReturnValue(validText, info.dataType)
            IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, info.theValue
               info.theValue = Ptr_New(/Allocate_Heap)
            ENDIF ELSE *info.theValue = testValue
         ENDELSE

            ; Is this a Carriage Return event?

         IF event.ch EQ 10B then cr_event = 1
      ENDCASE

   'INSERT MULTIPLE CHARACTERS': BEGIN

            ; Same thing as above, but for all the characters you are inserting.

         Widget_Control, info.textID, Get_Value=newText
         newText = newText[0]
         validText = COYOTE_Field_Validate(newText, info.dataType, Decimal=info.decimal, $
            Digits=info.digits, Positive=info.positive)
         IF validText NE newText THEN BEGIN
            Widget_Control, info.textID, Set_Value=previousText, Set_Text_Select=[textLocation[0]-1,0]
         ENDIF ELSE BEGIN
            info.theText = validText
            testValue  = COYOTE_Field_ReturnValue(validText, info.dataType)
            IF String(testValue) EQ "NULLVALUE" THEN BEGIN
               Ptr_Free, info.theValue
               info.theValue = Ptr_New(/Allocate_Heap)
            ENDIF ELSE *info.theValue = testValue
         ENDELSE
      ENDCASE

   'DELETE TEXT': BEGIN

            ; Just get the new text and update the info stucture.

         Widget_Control, info.textID, Get_Value=theText
         theText = theText[0]
         validText = COYOTE_Field_Validate(theText, info.dataType, Decimal=info.decimal, $
            Digits=info.digits, Positive=info.positive)

           ; Load the valid text.

        Widget_Control, info.textID, Set_Value=validText, Set_Text_Select=[textLocation[0],0]
        info.theText = validText
        testValue  = COYOTE_Field_ReturnValue(info.theText, info.dataType)
        IF String(testValue) EQ "NULLVALUE" THEN BEGIN
            Ptr_Free, info.theValue
            info.theValue = Ptr_New(/Allocate_Heap)
        ENDIF ELSE *info.theValue = testValue
      ENDCASE

   'SELECT TEXT': ; Nothing to do.

ENDCASE

   ; Do you report all events, or only Carriage Return events?

IF info.cr_only THEN BEGIN
   IF info.event_func NE "" THEN BEGIN
      thisEvent = {COYOTE_Field, info.cw_tlb, event.top, 0L, info.theValue, info.dataType}
      IF cr_event THEN Widget_Control, info.cw_tlb, Send_Event=thisEvent
   ENDIF

   IF info.event_pro NE "" THEN BEGIN
      thisEvent = {COYOTE_Field, info.cw_tlb, event.top, 0L, info.theValue, info.dataType}
      IF cr_event THEN Widget_Control, info.cw_tlb, Send_Event=thisEvent
   ENDIF
ENDIF ELSE BEGIN
   IF info.event_func NE "" THEN BEGIN
      thisEvent = {COYOTE_Field, info.cw_tlb, event.top, 0L, info.theValue, info.dataType}
      Widget_Control, info.cw_tlb, Send_Event=thisEvent
   ENDIF

   IF info.event_pro NE "" THEN BEGIN
      thisEvent = {COYOTE_Field, info.cw_tlb, event.top, 0L, info.theValue, info.dataType}
      Widget_Control, info.cw_tlb, Send_Event=thisEvent
   ENDIF
ENDELSE

   ; Out of here.

Widget_Control, event.ID, Set_UValue=info, /No_Copy
END ;----------------------------------------------------------------------------



Function COYOTE_Field, $          ; The compound widget COYOTE_Field.
   parent, $                      ; The parent widget. Required for all compound widgets.
   Column=column, $               ; Set this keyword to have Label above Text Widget.
   CR_Only=cr_only, $             ; Set this keyword if you only want Carriage Return events.
   Digits=digits, $               ; Set this keyword to number of allowed digits in INT and LONG values.
   Decimal=decimal, $             ; Set to the number of digits to right of decimal point.
   DoubleValue=doublevalue, $     ; Set this keyword if you want DOUBLE values returned.
   Event_Func=event_func, $       ; Set this keyword to the name of an Event Function.
   Event_Pro=event_pro, $         ; Set this keyword to the name of an Event Procedure.
   FieldFont=fieldfont, $         ; The font name for the text in the Text Widget.
   FloatValue=floatvalue, $       ; Set this keyword for FLOAT values.
   Frame=frame, $                 ; Set this keyword to put a frame around the compound widget.
   IntegerValue=integervalue, $   ; Set this keyword for INTEGER values.
   LabelFont=labelfont, $         ; The fon name for the text in the Label Widget.
   LabelSize=labelsize, $         ; The X screen size of the Label Widget.
   LongValue=longvalue, $         ; Set this keyword for LONG values.
   Positive=positive, $           ; Set this keyword to only allow positive number values.
   Row=row, $                     ; Set this keyword to have the Label beside the Text Widget. (The default.)
   Scr_XSize=scr_xsize, $         ; The X screen size of the compound widget.
   Scr_YSize=scr_ysize, $         ; The Y screen size of the compound widget.
   StringValue=stringvalue, $     ; Set this keyword for STRING values. (The default.)
   Title=title, $                 ; The text to go on the Label Widget.
   UValue=uvalue, $               ; A user value for any purpose.
   Value=value, $                 ; The "value" of the compound widget.
   XSize=xsize                    ; The X size of the Text Widget.

   ; A parent is required.

IF N_Elements(parent) EQ 0 THEN BEGIN
   Message, 'A PARENT argument is required. Returning...', /Informational
   RETURN, -1L
ENDIF

   ; Check keyword values.

IF N_Elements(column) EQ 0 THEN column = 0
IF N_Elements(digits) EQ 0 THEN digits = 0 ELSE digits = Fix(digits)
IF N_Elements(decimal) EQ 0 THEN decimal = -1 ELSE decimal = Fix(decimal)
IF N_Elements(event_func) EQ 0 THEN event_func = ""
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(fieldfont) EQ 0 THEN fieldfont = ""
IF N_Elements(frame) EQ 0 THEN frame = 0
IF N_Elements(labelfont) EQ 0 THEN labelfont = ""
IF N_Elements(labelsize) EQ 0 THEN labelsize = 0
IF N_Elements(scr_xsize) EQ 0 THEN scr_xsize = 0
IF N_Elements(scr_ysize) EQ 0 THEN scr_ysize = 0
IF N_Elements(title) EQ 0 THEN title = "Input Value: "
IF N_Elements(uvalue) EQ 0 THEN uvalue = ""
IF N_Elements(value) EQ 0 THEN value = "" ELSE value = StrTrim(value,2)
IF N_Elements(xsize) EQ 0 THEN xsize = 0
IF N_Elements(row) EQ 0 AND column EQ 0 THEN row = 1 ELSE row = 0
positive = Keyword_Set(positive)

   ; What data type are we looking for?

dataType = 'STRING'
IF Keyword_Set(stringvalue) THEN dataType = 'STRING'
IF Keyword_Set(integervalue) THEN dataType = 'INT'
IF Keyword_Set(longvalue) THEN dataType = 'LONG'
IF Keyword_Set(floatvalue) THEN dataType = 'FLOAT'
IF Keyword_Set(doublevalue) THEN dataType = 'DOUBLE'

   ; Validate the input value.

value = COYOTE_Field_Validate(value, dataType, Decimal=decimal, Digits=digits, Positive=positive)

   ; Create the widgets.

cw_tlb = Widget_Base( parent, $               ; The top-level base of the compound widget.
   Pro_Set_Value='COYOTE_Field_Set_Value', $
   Func_Get_Value='COYOTE_Field_Get_Value', $
   Frame=frame, $
   Row=row, $
   Column=Keyword_Set(column), $
   Base_Align_Center=1, $
   UValue=uvalue, $
   Event_Pro=event_pro, $
   Event_Func=event_func )

labelID = Widget_Label( cw_tlb, Value=title, Font=labelfont, $ ; The Label Widget.
  Scr_XSize=labelsize)

textID = Widget_Text( cw_tlb, $  ; The Text Widget.
   Value=value, $
   XSize=xsize, $
   YSize=1, $
   Scr_XSize=scr_xsize, $
   Scr_YSize=scr_ysize, $
   Font=fieldfont, $
   All_Events=1, $
   Event_Pro='COYOTE_Field_Event_Handler', $
   UName='INFO_CARRIER', $
   Kill_Notify='COYOTE_Field_Kill_Notify', $
   Editable=1 )

   ; Set the actual return value of the compound widget.

theValue = COYOTE_Field_ReturnValue(value, dataType)

   ; The info structure.

info = { theText:value, $                  ; The text in the Text Widget.
         theValue:Ptr_New(theValue), $     ; The real value of the Text Widget.
         cw_tlb:cw_tlb, $                  ; The top-level base of the compound widget.
         event_func:event_func, $          ; The name of the event handler function.
         event_pro:event_pro, $            ; The name of the event handler procedure.
         cr_only:Keyword_Set(cr_only), $   ; A flag to return events only on CR events.
         dataType:dataType, $              ; The type of data wanted in the compound widget.
         decimal:decimal, $                ; The number of digits in decimal numbers.
         digits:digits, $                  ; The number of digits in integer numbers.
         positive:positive, $              ; Flag to indicate positive number values.
         textID:textID }                   ; The widget identifier of the Text Widget.

    ; Store info structure in Text Widget.

Widget_Control, textID, Set_UValue=info, /No_Copy
RETURN, cw_tlb
END ;----------------------------------------------------------------------------


PRO CW_Light_Field_Events, event

; Handles carriage return events from the Intensity Value widget.

   ; Get the info carrier.

parent = Widget_Info(event.handler, /Parent)
infoCarrier = Widget_Info(parent, Find_by_UName='CW_LIGHT_CARRIER')
Widget_Control, infoCarrier, Get_UValue=info, /No_Copy

   ; Get the new intensity value. Make sure it is in range.

newIntensity = 0.00 > *event.value < 1.00
Widget_Control, info.intensityID, Set_Value=newIntensity
info.theIntensity = newIntensity

   ; Change the intensity of the light.

info.theLight->SetProperty, Intensity=newIntensity

   ; Prepare to send an event that notifies the Simple_Surface program.

event_pro = info.event_pro
tlb = info.tlb
top = event.top
parent = info.parent
name = info.name
intensity = info.theIntensity
color = info.theColor
hide = info.theHide
Widget_Control, infoCarrier, Set_UValue=info, /No_Copy

IF event_pro NE "" THEN BEGIN
   eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
      NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
   Widget_Control, parent, Send_Event=eventStruct
ENDIF
END ;------------------------------------------------------------------------------



PRO CW_Light_Control_Events, event

   ; Get the info structure.

infoCarrier = Widget_Info(event.handler, Find_By_UName='CW_LIGHT_CARRIER')
Widget_Control, infoCarrier, Get_UValue=info, /No_Copy

   ; What kind of event is this? Branch appropriately.

Widget_Control, event.id, Get_UValue=thisEvent
CASE thisEvent OF

   'COLOR': BEGIN
      TVLCT, info.color, info.index
      DEVICE, Decomposed=0, Get_Decomposed=theDecomposedState
      thisColor = PickColor(Group_Leader=event.top, info.index)
      thisColor = Reform(thisColor, 3, 1)
      info.theLight->SetProperty, Color=thisColor
      info.theColor = thisColor
      DEVICE, Decomposed=theDecomposedState
      info.color = thisColor
      END

   'RESET': BEGIN
      info.theColor = info.origColor
      info.theIntensity = info.origIntensity
      info.theHide = info.origHide
      info.color = info.origColor
      Widget_Control, info.intensityID, Set_Value=info.origIntensity
      IF info.origHide THEN BEGIN
         Widget_Control, info.onButtonID, Set_Button=0
         Widget_Control, info.offButtonID, Set_Button=1
      ENDIF ELSE BEGIN
         Widget_Control, info.onButtonID, Set_Button=1
         Widget_Control, info.offButtonID, Set_Button=0
      ENDELSE

      info.theLight->SetProperty, Intensity=info.origIntensity, $
         Color=info.origColor, Hide=info.origHide

      END

   'ON': BEGIN
      info.theHide = 0
      info.theLight->SetProperty, Hide=0
      END

   'OFF': BEGIN
      info.theHide = 1
      info.theLight->SetProperty, Hide=1
      END

ENDCASE

   ; Send an event if requested.

event_pro = info.event_pro
tlb = info.tlb
top = event.top
parent = info.parent
name = info.name
intensity = info.theIntensity
color = info.theColor
hide = info.theHide
Widget_Control, infoCarrier, Set_UValue=info, /No_Copy

IF event_pro NE "" THEN BEGIN
   eventStruct = {CW_LIGHT_CONTROL, ID:tlb, TOP:parent, HANDLER:0L, $
      NAME:name, INTENSITY:intensity, COLOR:color, HIDE:hide}
   Widget_Control, parent, Send_Event=eventStruct
ENDIF

END ;------------------------------------------------------------------------------



FUNCTION CW_Light_Control, parent, theLight, Name=name, UValue=uvalue, Event_Pro=event_pro, $
   LabelSize=labelsize, Index=index, Color=color

; This is a compound widget that allows one to manipulate various
; properties of light objects.

On_Error, 2

   ; Check parameters. Define defaults if necessary.

IF N_Elements(parent) EQ 0 THEN Message, 'Parent widget parameter is required 1st parameter.'
IF (N_Elements(theLight) EQ 0) OR (Size(theLight, /TName) NE 'OBJREF') THEN $
   Message, 'Light Object Reference is required 2nd parameter.'
IF N_Elements(uvalue) EQ 0 THEN uvalue = "LIGHT_CONTROL"
IF N_Elements(event_pro) EQ 0 THEN event_pro = ""
IF N_Elements(index) EQ 0 THEN index =  !D.Table_Size-2
IF N_Elements(color) EQ 0 THEN BEGIN
   TVLCT, r, g, b, /Get
   color = Reform([r[index], g[index], b[index]], 1, 3)
ENDIF ELSE color = Reform(color, 1, 3)
TVLCT, color, index

   ; Set the light properties.

theLight->GetProperty, Intensity=theIntensity, Hide=theHide, Color=theColor

IF N_Elements(name) EQ 0 THEN name = 'Light'

   ; Create the widgets.

tlb = Widget_Base(parent, Row=1, Base_Align_Center=1, $
   Event_Pro='CW_Light_Control_Events')
IF N_Elements(labelsize) NE 0 THEN $
   labelID = Widget_Label(tlb, Value=name + ': ', UNAME='CW_LIGHT_CARRIER', XSize=labelsize) ELSE $
   labelID = Widget_Label(tlb, Value=name + ': ', UNAME='CW_LIGHT_CARRIER')
exBaseID = Widget_Base(tlb, Row=1, /Exclusive, /Frame)
onButtonID = Widget_Button(exBaseID, Value='On', UValue='ON')
offButtonID = Widget_Button(exBaseID, Value='Off', UValue='OFF')
IF theHide THEN Widget_Control, offbuttonID, /Set_Button ELSE $
   Widget_Control, onbuttonID, /Set_Button
intensityID = Coyote_Field(tlb, Title='Intensity: ', /FloatValue, Value=theIntensity, $
   Decimal=2, /Positive, XSize=6, UValue='INTENSITY', Event_Pro='CW_Light_Field_Events', CR_Only=1)
colorID = Widget_Button(tlb, Value='Set Color', UValue='COLOR')
resetID= Widget_Button(tlb, Value='Reset', UValue='RESET')
Widget_Control, tlb, /Realize

   ; Create info structure with information to run the program. Store it.

info = {theLight:theLight, name:name, theIntensity:theIntensity, theHide:theHide, color:color, $
        theColor:theColor, Event_Pro:event_pro, origIntensity:theIntensity, index:index, $
        origColor:theColor, origHide:theHide, tlb:tlb, parent:parent, intensityID:intensityID, $
        onButtonID:onButtonID, offButtonID:offButtonID}
Widget_Control, labelID, Set_UValue=info, /No_Copy

RETURN, tlb
END ;------------------------------------------------------------------------------


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

PRO Light_Surface_Color, event
Widget_Control, event.top, Get_UValue=info
Widget_Control, event.id, Get_UValue=theColor
info.thisSurface->SetProperty, Color=theColor
info.thisWindow->Draw, info.thisView
END


PRO Light_Surface_Light_Done, event
Widget_Control, event.top, /Destroy
END


PRO Light_Surface_Light_Controls_Event, event
Widget_Control, event.top, Get_UValue=info
info.theWindow->Draw, info.theView
END
;-------------------------------------------------------------------------



PRO Light_Surface_Light_Controls, event

Widget_Control, event.top, Get_UValue=info, /No_Copy
Widget_Control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
xpos = sizes[0] + offsets[0] + 10
ypos = offsets[1] + 100
tlb = Widget_Base(Title='Light_Surface Light Controls', Column=1, Group_Leader=event.top, $
   UValue={theView:info.thisView, theWindow:info.thisWindow}, XOffset=xpos, YOffset=ypos)
dummy = CW_Light_Control(tlb, Name='Non-Rotating Light', info.nonRotatingLight, LabelSize=130, $
   Event_Pro='Light_Surface_Light_Controls_Event', Index=!D.Table_Size-18, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Rotating Light', info.rotatingLight, LabelSize=130, $
   Event_Pro='Light_Surface_Light_Controls_Event', Index=!D.Table_Size-19, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Fill Light', info.fillLight, LabelSize=130, $
   Event_Pro='Light_Surface_Light_Controls_Event', Index=!D.Table_Size-20, Color=[255,255,255])
dummy = CW_Light_Control(tlb, Name='Ambient Light', info.ambientLight, LabelSize=130, $
   Event_Pro='Light_Surface_Light_Controls_Event', Index=!D.Table_Size-21, Color=[255,255,255])
quit = Widget_Button(tlb, Value='Done', Event_Pro='Light_Surface_Light_Done')

Widget_Control, tlb, /Realize

XManager, 'Light_Controls', tlb, /No_Block, Event_Handler='Light_Surface_Light_Controls_Event'
Widget_Control, event.top, Set_UValue=info, /No_Copy

END
;-------------------------------------------------------------------------



PRO Light_Surface_Cleanup, tlb

    ; Come here when program dies. Free all created objects.

Widget_Control, tlb, Get_UValue=info
IF N_Elements(info) NE 0 THEN Obj_Destroy, info.thisContainer
END
;-------------------------------------------------------------------



PRO Light_Surface_Draw_Events, event

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



PRO Light_Surface_Style, event

     ; Event handler to select surface style.

Widget_Control, event.top, Get_UValue=info, /No_Copy

    ; What style is wanted?

Widget_Control, event.id, Get_UValue=newStyle
CASE newStyle OF

   'DOTS': info.thisSurface->SetProperty, Style=0
   'MESH': info.thisSurface->SetProperty, Style=1
   'SOLID': info.thisSurface->SetProperty, Style=2
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



PRO Light_Surface_Output, event

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


PRO Light_Surface_Exit, event

   ; Exit the program. This will cause the CLEANUP
   ; routine to be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO Light_Surface_Printing, event

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
      ; as I see in the display window. I use the ASPECT function
      ; from the Coyote library to modify the view appropriately.
      ; Note that the "position" is returned in Normalized units (Units=3).

   info.thisWindow->GetProperty, Dimensions=wdims
   plotAspect = Float(wdims[1]) / wdims[0]
   info.thisPrinter->GetProperty, Dimensions=pdims
   windowAspect = Float(pdims[1]) / pdims[0]
   position = Aspect(plotAspect, WindowAspect=windowAspect, Margin=0)
   info.thisView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
      Location=[position[0], position[1]], Units=3

      ; Print the document.

   Widget_Control, Hourglass=1
   info.thisPrinter->Draw, info.thisView, Vector=info.vector
   info.thisPrinter->NewDocument
    Widget_Control, Hourglass=1

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



PRO Light_Surface_Resize, event

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



PRO Light_Surface, data, x, y, _Extra=extra, XTitle=xtitle, $
   YTitle=ytitle, ZTitle=ztitle, Group_Leader=groupLeader, $
   Hidden_Lines=hidden_lines, Vector=vector, Exact=exact, $
   Landscape=landscape

    ; Check for keywords.

IF N_Elements(xtitle) EQ 0 THEN xtitle='X Axis'
IF N_Elements(ytitle) EQ 0 THEN ytitle='Y Axis'
IF N_Elements(ztitle) EQ 0 THEN ztitle='Z Axis'
hidden_lines = Keyword_Set(hidden_lines)
landscape = Keyword_Set(landscape)
vector = Keyword_Set(vector)
solid = 1

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

    ; Create a wire mesh surface object. Make it yellow.

thisSurface = OBJ_NEW('IDLgrSurface', data, x, y, $
   Color=[255,255,255], _Extra=extra, Hidden_Lines=hidden_lines, $
   Shading=1, Style=solid+1)

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
    ; or the surface will appear washed out.

ambientLight = Obj_New('IDLgrLight', Type=0, Intensity=0.25)
thisModel->Add, ambientLight

    ; Create a non-rotating overhead side light.

nonrotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.85, $
    Location=[-xrange[1], (yrange[1]-yrange[0])/2.0, 4*zrange[1]], $
    Direction=[xrange[1], (yrange[1]-yrange[0])/2.0, zrange[0]])
nonrotatingModel = Obj_New('IDLgrModel')
nonrotatingModel->Add, nonrotatingLight
thisView->Add, nonrotatingModel

    ; Rotate the non-rotating model to the standard surface view.

nonrotatingModel->Rotate,[1,0,0], -90  ; To get the Z-axis vertical.
nonrotatingModel->Rotate,[0,1,0],  30  ; Rotate it slightly to the right.
nonrotatingModel->Rotate,[1,0,0],  30  ; Rotate it down slightly.

    ; Shaded surfaces will not look shaded unless there is a
    ; positional light source to give the surface edges definition.

rotatingLight = Obj_New('IDLgrLight', Type=1, Intensity=0.65, $
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

    ; Scale the light sources.

rotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
fillLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs
nonrotatingLight->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys, ZCoord_Conv=zs

    ; Create the widgets to view the surface. Set expose events
    ; on the draw widget so that it refreshes itself whenever necessary.
    ; Button events are on to enable trackball movement.

tlb = Widget_Base(Title='Resizeable Window Surface Example', Column=1, $
   TLB_Size_Events=1, MBar=menubase)
drawID = Widget_Draw(tlb, XSize=400, YSize=400, Graphics_Level=2, Retain=0, $
   Expose_Events=1, Event_Pro='Light_Surface_Draw_Events', Button_Events=1)

    ; Create FILE menu buttons for printing and exiting.

fileID = Widget_Button(menubase, Value='File')

lightID = Widget_Button(fileID, Value='Light Controls...', $
   Event_Pro='Light_Surface_Light_Controls')

surfColorID = Widget_Button(fileID, Value='Surface Color', /Menu, $
   Event_Pro='Light_Surface_Color')
dummy = Widget_Button(surfColorID, Value='White', UValue=[255,255,255])
dummy = Widget_Button(surfColorID, Value='Black', UValue=[0,0,0])
dummy = Widget_Button(surfColorID, Value='Charcoal', UValue=[80,80,80])
dummy = Widget_Button(surfColorID, Value='Yellow', UValue=[255,255,0])
dummy = Widget_Button(surfColorID, Value='Green', UValue=[0,255,0])
dummy = Widget_Button(surfColorID, Value='Blue', UValue=[0,0,255])
dummy = Widget_Button(surfColorID, Value='Red', UValue=[255,0,0])

outputID = Widget_Button(fileID, Value='Save As...', /Menu, /Separator)
dummy = Widget_Button(outputID, Value='GIF File', $
   UValue='GIF', Event_Pro='Light_Surface_Output')
dummy = Widget_Button(outputID, Value='JPEG File', $
   UValue='JPEG', Event_Pro='Light_Surface_Output')

dummy = Widget_Button(fileID, Value='Print', Event_Pro='Light_Surface_Printing')

dummy = Widget_Button(fileID, /Separator, Value='Exit', $
   Event_Pro='Light_Surface_Exit')

   ; Create STYLE menu buttons for surface style.

styleID = Widget_Button(menubase, Value='Style', Event_Pro='Light_Surface_Style', /Menu)
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

   ; Create an INFO structure to hold needed program information.

info = { thisContainer:thisContainer, $ ; The object container.
         thisWindow:thisWindow, $       ; The window object.
         thisPrinter:thisPrinter, $     ; The printer object.
         thisSurface:thisSurface, $     ; The surface object.
         thisTrackball:thisTrackball, $ ; The trackball object.
         thisModel:thisModel, $         ; The model object.
         thisView:thisView, $           ; The view object.
         nonRotatingLight:nonRotatingLight, $
         rotatingLight:rotatingLight, $
         fillLight:fillLight, $
         ambientLight:ambientLight, $
         xaxis:xaxis, $                 ; The X axis object.
         yaxis:yaxis, $                 ; The Y axis object.
         zaxis:zaxis, $                 ; The Z axis object.
         landscape:landscape, $         ; A flag for landscape output.
         vector:vector }                ; A flag for vector output.

   ; Store the info structure in the UValue of the TLB.

Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Call XManager. Set a cleanup routine so the objects
   ; can be freed upon exit from this program.

XManager, 'Light_Surface', tlb, Cleanup='Light_Surface_Cleanup', /No_Block, $
   Event_Handler='Light_Surface_Resize', Group_Leader=groupLeader
END
;-------------------------------------------------------------------