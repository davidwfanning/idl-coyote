;+
; NAME:
;       GETCOLOR
;
; PURPOSE:
;       The original purpose of this function was to enable the
;       user to specify one of the 16 colors supported by the
;       McIDAS color map by name. Over time, however, the function
;       has become a general purpose function for handling and
;       supporting drawing colors in a device-independent way.
;       In particular, I have been looking for ways to write color
;       handling code that will work transparently on both 8-bit and
;       24-bit machines. On 24-bit machines, the code should work the
;       same where color decomposition is turned on or off. The program
;       now supports 88 colors.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING:
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Graphics, Color Specification.
;
; CALLING SEQUENCE:
;       result = GETCOLOR(color, index)
;
; OPTIONAL INPUT PARAMETERS:
;       COLOR: A string with the "name" of the color. Valid names are:
;           black
;           magenta
;           cyan
;           yellow
;           green
;           red
;           blue
;           navy
;           pink
;           aqua
;           orchid
;           sky
;           beige
;           charcoal
;           gray
;           white
;
;           The color YELLOW is returned if the color name can't be resolved.
;           Case is unimportant.
;
;           If the function is called with just this single input parameter,
;           the return value is either a 1-by-3 array containing the RGB values of
;           that particular color, or a 24-bit integer that can be "decomposed" into
;           that particular color, depending upon the state of the TRUE keyword and
;           upon whether color decomposition is turned on or off. The state of color
;           decomposition can ONLY be determined if the program is being run in
;           IDL 5.2 or higher.
;
;       INDEX: The color table index where the specified color should be loaded.
;           If this parameter is passed, then the return value of the function is the
;           index number and not the color triple. (If color decomposition is turned
;           on AND the user specifies an index parameter, the color is loaded in the
;           color table at the proper index, but a 24-bit value is returned to the
;           user in IDL 5.2 and higher. This assumes the INDEXED keyword is NOT set.)
;
;       If no positional parameter is present, then the return value is either a 16-by-3
;       byte array containing the RGB values of all 16 colors or it is a 16-element
;       long integer array containing color values that can be decomposed into colors.
;       The 16-by-3 array is appropriate for loading color tables with the TVLCT command:
;
;           Device, Decomposed=0
;           colors = GetColor()
;           TVLCT, colors, 100
;
;
; INPUT KEYWORD PARAMETERS:
;
;       NAMES: If this keyword is set, the return value of the function is
;              a 88-element string array containing the names of the colors.
;              These names would be appropriate, for example, in building
;              a list widget with the names of the colors. If the NAMES
;              keyword is set, the COLOR and INDEX parameters are ignored.
;
;                 listID = Widget_List(baseID, Value=GetColor(/Names), YSize=16)
;
;       INDEXED:  If this keyword is set, the return value is always an index
;              into the color table. In the absence of a color table INDEX
;              parameter, the color is loaded at !P.COLOR < (!D.Table_Size-1).
;
;       LOAD:  If this keyword is set, all 88 colors are automatically loaded
;              starting at the color index specified by the START keyword.
;              Note that setting this keyword means that the return value of the
;              function will be a structure, with each field of the structure
;              corresponding to a color name. The value of each field will be
;              an index number (set by the START keyword) corresponding to the
;              associated color, or a 24-bit long integer value that creates the
;              color on a true-color device. What you have as the field values is
;              determined by the TRUE keyword or whether color decomposition is on
;              or off in the absense of the TRUE keyword. It will either be a 1-by-3
;              byte array or a long integer value.
;
;       START: The starting color index number if the LOAD keyword is set. This keyword
;              value is ignored unless the LOAD keyword is also set. The keyword is also
;              ignored if the TRUE keyword is set or if color decomposition in on in
;              IDL 5.2 and higher. The default value for the START keyword is
;              !D.TABLE_SIZE - 89.
;
;       TRUE:  If this keyword is set, the specified color triple is returned
;              as a 24-bit integer equivalent. The lowest 8 bits correspond to
;              the red value; the middle 8 bits to the green value; and the
;              highest 8 bits correspond to the blue value. In IDL 5.2 and higher,
;              if color decomposition is turned on, it is as though this keyword
;              were set.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       The TRUE keyword causes the START keyword to be ignored.
;       The NAMES keyword causes the COLOR, INDEX, START, and TRUE parameters to be ignored.
;       The COLOR parameter is ignored if the LOAD keyword is used.
;       On systems where it is possible to tell the state of color decomposition
;       (i.e., IDL 5.2 and higher), a 24-bit value (or values) is automatically
;       returned if color decomposition is ON.
;
; EXAMPLE:
;       To load a yellow color in color index 100 and plot in yellow, type:
;
;          yellow = GETCOLOR('yellow', 100)
;          PLOT, data, COLOR=yellow
;
;       or,
;
;          PLOT, data, COLOR=GETCOLOR('yellow', 100)
;
;       To do the same thing on a 24-bit color system with decomposed color on, type:
;
;          PLOT, data, COLOR=GETCOLOR('yellow', /TRUE)
;
;       or in IDL 5.2 and higher,
;
;          DEVICE, Decomposed=1
;          PLOT, data, COLOR=GETCOLOR('yellow')
;
;       To load all 88 colors into the current color table, starting at
;       color index 100, type:
;
;          TVLCT, GETCOLOR(), 100
;
;       To add the color names to a list widget:
;
;           listID = Widget_List(baseID, Value=GetColor(/Names), YSize=16)
;
;       To load all 88 colors and have the color indices returned in a structure:
;
;           DEVICE, Decomposed=0
;           colors = GetColor(/Load, Start=1)
;           HELP, colors, /Structure
;           PLOT, data, COLOR=colors.yellow
;
;       To get the direct color values as 24-bit integers in color structure fields:
;
;           DEVICE, Decomposed=1
;           colors = GetColor(/Load)
;           PLOT, data, COLOR=colors.yellow
;
;       Note that the START keyword value is ignored if on a 24-bit device,
;       so it is possible to write completely device-independent code by
;       writing code like this:
;
;           colors = GetColor(/Load)
;           PLOT, data, Color=colors.yellow
;
; MODIFICATION HISTORY:
;       Written by: David Fanning, 10 February 96.
;       Fixed a bug in which N_ELEMENTS was spelled wrong. 7 Dec 96. DWF
;       Added the McIDAS colors to the program. 24 Feb 99. DWF
;       Added the INDEX parameter to the program 8 Mar 99. DWF
;       Added the NAMES keyword at insistence of Martin Schultz. 10 Mar 99. DWF
;       Reorderd the colors so black is first and white is last. 7 June 99. DWF
;       Added automatic recognition of DECOMPOSED=1 state. 7 June 99. DWF
;       Added LOAD AND START keywords. 7 June 99. DWF.
;       Replaced GOLD with CHARCOAL color. 28 Oct 99. DWF.
;       Added INDEXED keyword to force indexed color mode. 28 Oct 99. DWF.
;       Fixed problem of "aqua" and "pink" being mixed up. 18 Mar 00. DWF.
;       Changed ON_ERROR from 1 to 2, and improved error handling. 2 Aug 00. DWF.
;       Increased the known colors from 16 to 88. 19 October 2000. DWF.
;       Fixed typos in which "thisColor" was written as "theColor". 10 AUG 2001. DWF.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


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

   names =  ['White']
   rvalue = [ 255]
   gvalue = [ 255]
   bvalue = [ 255]
   names  = [ names,        'Snow',     'Ivory','Light Yellow',   'Cornsilk',      'Beige',   'Seashell' ]
   rvalue = [ rvalue,          255,          255,          255,          255,          245,          255 ]
   gvalue = [ gvalue,          250,          255,          255,          248,          245,          245 ]
   bvalue = [ bvalue,          250,          240,          224,          220,          220,          238 ]
   names  = [ names,       'Linen','Antique White',    'Papaya',     'Almond',     'Bisque',  'Moccasin' ]
   rvalue = [ rvalue,          250,          250,          255,          255,          255,          255 ]
   gvalue = [ gvalue,          240,          235,          239,          235,          228,          228 ]
   bvalue = [ bvalue,          230,          215,          213,          205,          196,          181 ]
   names  = [ names,       'Wheat',  'Burlywood',        'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
   rvalue = [ rvalue,          245,          222,          210,          230,          230,          210 ]
   gvalue = [ gvalue,          222,          184,          180,          230,          230,          210 ]
   bvalue = [ bvalue,          179,          135,          140,          230,          250,          210 ]
   names  = [ names,        'Gray', 'Slate Gray',  'Dark Gray',   'Charcoal',      'Black', 'Light Cyan' ]
   rvalue = [ rvalue,          190,          112,          110,           70,            0,          224 ]
   gvalue = [ gvalue,          190,          128,          110,           70,            0,          255 ]
   bvalue = [ bvalue,          190,          144,          110,           70,            0,          255 ]
   names  = [ names, 'Powder Blue',   'Sky Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',       'Blue' ]
   rvalue = [ rvalue,          176,          135,           70,           30,           65,            0 ]
   gvalue = [ gvalue,          224,          206,          130,          144,          105,            0 ]
   bvalue = [ bvalue,          230,          235,          180,          255,          225,          255 ]
   names  = [ names,        'Navy',   'Honeydew', 'Pale Green','Aquamarine','Spring Green',       'Cyan' ]
   rvalue = [ rvalue,            0,          240,          152,          127,            0,            0 ]
   gvalue = [ gvalue,            0,          255,          251,          255,          250,          255 ]
   bvalue = [ bvalue,          128,          240,          152,          212,          154,          255 ]
   names  = [ names,   'Turquoise', 'Sea Green','Forest Green','Green Yellow','Chartreuse', 'Lawn Green' ]
   rvalue = [ rvalue,           64,           46,           34,          173,          127,          124 ]
   gvalue = [ gvalue,          224,          139,          139,          255,          255,          252 ]
   bvalue = [ bvalue,          208,           87,           34,           47,            0,            0 ]
   names  = [ names,       'Green', 'Lime Green', 'Olive Drab',     'Olive','Dark Green','Pale Goldenrod']
   rvalue = [ rvalue,            0,           50,          107,           85,            0,          238 ]
   gvalue = [ gvalue,          255,          205,          142,          107,          100,          232 ]
   bvalue = [ bvalue,            0,           50,           35,           47,            0,          170 ]
   names  = [ names,       'Khaki', 'Dark Khaki',     'Yellow',       'Gold','Goldenrod','Dark Goldenrod']
   rvalue = [ rvalue,          240,          189,          255,          255,          218,          184 ]
   gvalue = [ gvalue,          230,          183,          255,          215,          165,          134 ]
   bvalue = [ bvalue,          140,          107,            0,            0,           32,           11 ]
   names  = [ names,'Saddle Brown',       'Rose',       'Pink', 'Rosy Brown','Sandy Brown',       'Peru' ]
   rvalue = [ rvalue,          139,          255,          255,          188,          244,          205 ]
   gvalue = [ gvalue,           69,          228,          192,          143,          164,          133 ]
   bvalue = [ bvalue,           19,          225,          203,          143,           96,           63 ]
   names  = [ names,  'Indian Red',  'Chocolate',     'Sienna','Dark Salmon',    'Salmon','Light Salmon' ]
   rvalue = [ rvalue,          205,          210,          160,          233,          250,          255 ]
   gvalue = [ gvalue,           92,          105,           82,          150,          128,          160 ]
   bvalue = [ bvalue,           92,           30,           45,          122,          114,          122 ]
   names  = [ names,      'Orange',      'Coral', 'Light Coral',  'Firebrick',      'Brown',  'Hot Pink' ]
   rvalue = [ rvalue,          255,          255,          240,          178,          165,          255 ]
   gvalue = [ gvalue,          165,          127,          128,           34,           42,          105 ]
   bvalue = [ bvalue,            0,           80,          128,           34,           42,          180 ]
   names  = [ names,   'Deep Pink',    'Magenta',     'Tomato', 'Orange Red',        'Red', 'Violet Red' ]
   rvalue = [ rvalue,          255,          255,          255,          255,          255,          208 ]
   gvalue = [ gvalue,           20,            0,           99,           69,            0,           32 ]
   bvalue = [ bvalue,          147,          255,           71,            0,            0,          144 ]
   names  = [ names,      'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
   rvalue = [ rvalue,          176,          216,          221,          238,          218,          186 ]
   gvalue = [ gvalue,           48,          191,          160,          130,          112,           85 ]
   bvalue = [ bvalue,           96,          216,          221,          238,          214,          211 ]
   names  = [ names, 'Dark Orchid','Blue Violet',     'Purple' ]
   rvalue = [ rvalue,          153,          138,          160 ]
   gvalue = [ gvalue,           50,           43,           32 ]
   bvalue = [ bvalue,          204,          226,          240 ]

   ; Did the user ask for a specific color? If not, return
   ; all the colors. If the user asked for a specific color,
   ; find out if a 24-bit value is required. Return to caller
   ; if an error occurs.

ON_Error, 2
ncolors = N_Elements(names)

np = N_Params()
IF N_Elements(start) EQ 0 THEN start = !D.TABLE_SIZE - ncolors - 1 ELSE start = start < (!D.TABLE_SIZE - ncolors - 1)

   ; User ask for the color names?

IF Keyword_Set(colornames) THEN RETURN, Reform(names, 1, N_Elements(names)) $
    ELSE names = StrUpCase(StrCompress(StrTrim(names,2), /Remove_All))

   ; If no positional parameter, return all colors.

IF np EQ 0 THEN BEGIN

   ; Did the user want a 24-bit value? If so, call COLOR24.

   IF Keyword_Set(trueColor) THEN BEGIN
      returnColor = LonArr(ncolors)
      FOR j=0,ncolors-1 DO returnColor[j] = Color24([rvalue[j], gvalue[j], bvalue[j]])

         ; If LOAD keyword set, return a color structure.

      IF Keyword_Set(load) THEN BEGIN
         returnValue = Create_Struct('white', returnColor[0])
         FOR j=1,ncolors-1 DO returnValue = Create_Struct(returnValue, names[j], returnColor[j])
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
         returnColor = LonArr(ncolors)
         FOR j=0,ncolors-1 DO returnColor[j] = Color24([rvalue[j], gvalue[j], bvalue[j]])
         IF Keyword_Set(load) THEN BEGIN
            returnValue = Create_Struct('white', returnColor[0])
            FOR j=1,ncolors-1 DO returnValue = Create_Struct(returnValue, names[j], returnColor[j])
            RETURN, returnValue
         ENDIF
         RETURN, returnColor
      ENDIF

      IF Keyword_Set(load) THEN BEGIN
         TVLCT, Reform([rvalue, gvalue, bvalue], ncolors, 3), start
         returnValue = Create_Struct('white', start)
         FOR j=1,ncolors-1 DO returnValue = Create_Struct(returnValue, names[j], start+j)
         RETURN, returnValue
      ENDIF

      returnColor = REFORM([rvalue, gvalue, bvalue], ncolors, 3)
      RETURN, returnColor

   ENDIF

   IF Keyword_Set(load) THEN BEGIN
      TVLCT, Reform([rvalue, gvalue, bvalue], ncolors, 3), start
      returnValue = Create_Struct('white', start)
      FOR j=1,ncolors-1 DO returnValue = Create_Struct(returnValue, names[j], start+j)
      RETURN, returnValue
   ENDIF

   returnColor = REFORM([rvalue, gvalue, bvalue], ncolors, 3)
   RETURN, returnColor

ENDIF

   ; Make sure the color parameter is an uppercase string.

varInfo = SIZE(thisColor)
IF varInfo(varInfo(0) + 1) NE 7 THEN $
   MESSAGE, 'The color name must be a string.'
thisColor = StrUpCase(StrCompress(StrTrim(thisColor,2), /Remove_All))

   ; Check synonyms of colors.

IF StrUpCase(thisColor) EQ 'GREY' THEN thisColor = 'GRAY'
IF StrUpCase(thisColor) EQ 'AQUA' THEN thisColor = 'AQUAMARINE'
IF StrUpCase(thisColor) EQ 'SKYBLUE' THEN thisColor = 'SKY BLUE'
IF StrUpCase(thisColor) EQ 'LIGHTGREY' THEN thisColor = 'LIGHTGRAY'
IF StrUpCase(thisColor) EQ 'MEDIUMGREY' THEN thisColor = 'MEDIUMGRAY'
IF StrUpCase(thisColor) EQ 'SLATEGREY' THEN thisColor = 'SLATEGRAY'
IF StrUpCase(thisColor) EQ 'DARKGREY' THEN thisColor = 'DARKGRAY'
IF StrUpCase(thisColor) EQ 'SKY' THEN thisColor = 'SKY BLUE'
IF StrUpCase(thisColor) EQ 'NAVY BLUE' THEN thisColor = 'NAVY'
IF StrUpCase(thisColor) EQ 'NAVYBLUE' THEN thisColor = 'NAVY'


   ; Get the color triple for this color.

colorIndex = WHERE(names EQ thisColor)

   ; If you can't find it. Issue an informational message,
   ; set the index to a YELLOW color, and continue.

IF colorIndex(0) LT 0 THEN BEGIN
   MESSAGE, "Can't find color " + thisColor + ". Returning " + StrUpCase(names[0]) + ".", /INFORMATIONAL
   thisColor = names[0]
   colorIndex = 0
ENDIF

   ; Get the color triple.

r = rvalue(colorIndex)
g = gvalue(colorIndex)
b = bvalue(colorIndex)
returnColor = REFORM([r, g, b], 1, 3)

   ; Did the user want a 24-bit value? If so, call COLOR24.

IF KEYWORD_SET(trueColor) THEN BEGIN
   returnColor = COLOR24(returnColor)
   RETURN, returnColor[0]
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
      RETURN, returnColor[0]
   ENDIF
ENDIF

   ; Did the user specify a color index? If so, load it.

IF N_Elements(index) NE 0 THEN BEGIN
   index = 0 > index < (!D.Table_Size-1)
   TVLCT, returnColor, index
   returnColor = index
   RETURN, returnColor[0]
ENDIF

   ; Did the user specify INDEXED color? If so, load it.

IF Keyword_Set(indexedColor) THEN BEGIN
   TVLCT, returnColor, !P.Color
   returnColor = !P.Color < (!D.Table_Size -1)
   RETURN, returnColor[0]
ENDIF

RETURN, returnColor
END
