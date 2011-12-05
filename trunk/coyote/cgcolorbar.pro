; docformat = 'rst'
;
; NAME:
;   cgColorbar
;
; PURPOSE:
;   The purpose of this routine is to add a color bar to the current graphics window.
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
;+
; The purpose of this routine is to add a color bar to the current graphics window.
;
; :Categories:
;    Graphics
;    
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    annotatecolor: in, optional, type=string, default="opposite"
;       The name of the "annotation color" to use. The names are those for
;       cgCOLOR. If this keyword is used, the annotation color is loaded after
;       the color bar is displayed. This keyword is provided to maintain backward 
;       compatibility, but also to solve the potential problem of an extra line showing up
;       in the color bar when the COLOR keyword is used in indexed color mode. In other words,
;       use ANNOTATECOLOR in place of COLOR for complete color model independent results.
;    bottom: in, optional, type=integer, default=0
;       The lowest color index of the colors to be loaded in the color bar.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    clamp: in, optional, type=float
;        A two-element array in data units. The color bar is clamped to these
;        two values. This is mostly of interest if you are "window-leveling"
;        an image. The clamp is set to the "window" of the color bar.
;        Normally, when you are doing this, you would like the colors outside
;        the "window" to be set to a neutral color. Use the NEUTRALINDEX keyword
;        to set the netural color index in the color bar. (See the Examples section
;        for more information.)
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color. The default value is to use the ANNOTATECOLOR.
;    divisions: in, optional, type=integer
;         The number of divisions to divide the bar into. There will
;         be (divisions + 1) annotations. The default is 0 if using the
;         default color bar formatting, which allows the plot command to 
;         determine how many divisions to make. Otherwise, if you are specifying
;         some other format for the tick labels, the default number of divisions
;         is six.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last plot command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot.
;    font: in, optional, type=integer, default=!P.Font
;       Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. Default is "", which allows
;       the Plot command to determine the appropriate format.
;    invertcolors: in, optional, type=boolean, default=0
;       Setting this keyword inverts the colors in the color bar.
;    maxrange: in, optional
;       The maximum data value for the color bar annotation. Default is NCOLORS.
;    minrange: in, optional, type=float, default=0.0
;       The minimum data value for the bar annotation. 
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    ncolors: in, optional, type=integer, default=256
;       This is the number of colors in the color bar.
;    neutralindex: in, optional, type=integer   
;       This is the color index to use for color bar values outside the
;       clamping range when clamping the color bar with the CLAMP keyword.
;       If this keyword is absent, the highest color table value is used
;       for low range values and the lowest color table value is used
;       for high range values, in order to provide contrast with the
;       clamped region. (See the Examples section for more information.)
;    nodisplay: in, optional
;       This keyword is obsolete and is no longer used.
;    palette: in, optional, type=byte
;       A color palette containing the RGB color vectors to use for the color
;       bar. The program will sample NCOLORS from the color palette. 
;    position: in, optional, type=float          
;       A four-element array of normalized coordinates in the same
;       form as the POSITION keyword on a plot. Default is[0.88, 0.10, 0.95, 0.90] 
;       for a vertical bar and [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;       See the FIT keyword, also.
;    range: in, optional, type=float
;       A two-element vector of the form [min, max]. Provides an alternative 
;       and faster way way of setting the MINRANGE and MAXRANGE keywords.
;    reverse: in, optional, type=boolean, default=0
;       An alternative keyword name (one I can actually remember!) for the INVERTCOLORS keyword.
;       It reverses the colors in the color bar.
;    right: in, optional, type=boolean, default=0   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    ticklen: in, optional, type=float, default=0.25
;       Set this keyword to the major tick length desired. Default is 0.25. Setting this 
;       keyword to a value greater than or equal to 0.5 will result in major tick marks 
;       extending the width of the color bar. Note that setting this keyword to 0.3 or
;       greater will result in minor tick mark lengths being set to 0.01, which is almost 
;       too small to be seen. All direct graphics tick marks act in this (strange!) way.
;    ticknames: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks.
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    top: in, optional, type=boolean, default=0
;       This puts the labels on top of the bar rather than under it. The keyword only 
;       applies if a horizontal color bar is rendered.
;    vertical: in, optional, type=boolean, default=0
;       Setting this keyword give a vertical color bar. The default is a horizontal color bar.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;    _ref_extra: in, optional
;         Any keyword appropriate for the PLOT and AXIS commands is also accepted by keyword
;         inheritance.
;    
; :Examples:
;    To display a horizontal color bar above a contour plot, type::
;
;       cgLOADCT, 5, NCOLORS=100
;       cgCONTOUR, DIST(31,41), POSITION=[0.15, 0.15, 0.95, 0.75], $
;          C_COLORS=INDGEN(25)*4, NLEVELS=25
;       cgCOLORBAR, NCOLORS=100, POSITION=[0.15, 0.85, 0.95, 0.90]
;       
;    Example using the CLAMP and NEUTRALINDEX keywords::
;       
;       cgLOADCT, 33, NCOLORS=254
;       TVLCT, cgCOLOR('gray', /TRIPLE), 255
;       cgCOLORBAR, NCOLORS=254, NEUTRALINDEX=255, RANGE=[0,1500], $
;           DIVISIONS=8, CLAMP=[400, 800]
;
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: david@idlcoyote.com
;           Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;       Written by: David W. Fanning, 4 February 2011, as a direct descendant of cgColorbar.
;       Program developement stopped on cgColorbar as of this date, and this program has
;       become a part of the Coyote Graphics System.
;       Added FIT keyword. 28 Feb 2011. DWF
;       Made default character size cgDefCharsize*0.85. 28 Feb 2011. DWF.
;       Modified error handler to restore the entry decomposition state if there is an error. 17 March 2011. DWF
;       Added CHARPERCENT keyword 18 March 2011. DWF.
;       Added XTITLE and YTITLE keywords, which do nothing except prevent these keywords
;          from being used inadvertently. 27 May 2011. DWF.
;       Fixed a problem with assigning the color with the ANNOTATECOLOR keyword in the
;          Z-graphics buffer. 30 Aug 2011. DWF.
;       Changed the default DIVISIONS to 0 and the default FORMAT to "". 2 Sept 2011. DWF.
;       Added code that will force MINRANGE and MAXRANGE values to be scalars. 5 Sept 2011. DWF.
;       Problem with division by zero when FORMAT is not default value. Now, if format
;          is the default value, then default is DIVISIONS=0, else DIVISIONS=6.
;       Documented the TICKLEN keyword and set the default tick length to 0.25. 3 Oct 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2008, Fanning Software Consulting, Inc.
;-
PRO cgColorbar, $
    ADDCMD=addcmd, $
    ANNOTATECOLOR=annotatecolor, $
    BOTTOM=bottom, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    DIVISIONS=divisions, $
    FIT=fit, $
    FONT=font, $
    FORMAT=format, $
    INVERTCOLORS=invertcolors, $
    MAXRANGE=maxrange, $
    MINOR=minor, $
    MINRANGE=minrange, $
    NCOLORS=ncolors, $
    NEUTRALINDEX=neutralIndex, $
    NODISPLAY=nodisplay, $
    PALETTE=palette, $
    POSITION=position, $
    RANGE=range, $
    REVERSE=reverse, $
    RIGHT=right, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TITLE=title, $
    TOP=top, $
    VERTICAL=vertical, $
    XLOG=xlog, $
    XTITLE=xtitle, $ ; Ignored.
    YLOG=ylog, $
    YTITLE=ytitle, $ ; Ignored
    WINDOW=window, $
    _REF_EXTRA=extra

    Compile_Opt idl2

    ; Catch the error.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(currentState) NE 0 THEN SetDecomposedState, currentState
        RETURN
    ENDIF

    ; Should this be added to a resizeable graphics window?
    IF (Keyword_Set(window) OR Keyword_Set(addcmd)) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    
        IF Keyword_Set(addcmd) THEN window = 0
        void = cgQuery(COUNT=wincnt)
        IF wincnt EQ 0 THEN cgWindow
        cgWindow, 'cgColorbar', $
            ANNOTATECOLOR=annotatecolor, $
            BOTTOM=bottom, $
            CHARPERCENT=charpercent, $
            CHARSIZE=charsize, $
            CLAMP=clamp, $
            COLOR=color, $
            DIVISIONS=divisions, $
            FIT=fit, $
            FONT=font, $
            FORMAT=format, $
            INVERTCOLORS=invertcolors, $
            MAXRANGE=maxrange, $
            MINOR=minor, $
            MINRANGE=minrange, $
            NCOLORS=ncolors, $
            NEUTRALINDEX=neutralIndex, $
            NODISPLAY=nodisplay, $
            PALETTE=palette, $
            POSITION=position, $
            RANGE=range, $
            REVERSE=reverse, $
            RIGHT=right, $
            TICKLEN=ticklen, $
            TICKNAMES=ticknames, $
            TITLE=title, $
            TOP=top, $
            VERTICAL=vertical, $
            XLOG=xlog, $
            XTITLE=xtitle, $
            YLOG=ylog, $
            YTITLE=ytitle, $
            REPLACECMD=Keyword_Set(window), $
            ADDCMD=Keyword_Set(addcmd), $
             _EXTRA=extra

            
         RETURN
    ENDIF
    
    ; Get the current color table vectors. 
    TVLCT, r, g, b, /GET
    
    ; Default values.
    IF (N_Elements(charPercent) EQ 0) $
        THEN charPercent = 0.85 $
        ELSE charPercent = 0.0 > charPercent < 1.0
        
    ; If you have a palette, load the colors now. Otherwise whatever colors
    ; are in the current color table will be used.
    IF N_Elements(palette) NE 0 THEN BEGIN
        IF Size(palette, /N_DIMENSIONS) NE 2 THEN Message, 'Color palette is not a 3xN array.'
        dims = Size(palette, /DIMENSIONS)
        threeIndex = Where(dims EQ 3)
        IF ((threeIndex)[0] LT 0) THEN Message, 'Color palette is not a 3xN array.'
        IF threeIndex[0] EQ 0 THEN palette = Transpose(palette)
        TVLCT, palette
        TVLCT, rr, gg, bb, /Get
    ENDIF
    
    ; Set up PostScript device for working with colors.
    IF !D.Name EQ 'PS' THEN Device, COLOR=1, BITS_PER_PIXEL=8
    
    ; Save the current plot state.
    bang_p = !P
    bang_x = !X
    bang_Y = !Y
    bang_Z = !Z
    bang_Map = !Map

    ; Are scalable pixels available on the device?
    IF (!D.Flags AND 1) NE 0 THEN scalablePixels = 1 ELSE scalablePixels = 0

    ; Which release of IDL is this?
    thisRelease = Float(!Version.Release)

    ; Check and define keywords.
    IF N_ELEMENTS(ncolors) EQ 0 THEN ncolors = 256
    IF N_ELEMENTS(bottom) EQ 0 THEN bottom = 0B
    IF N_Elements(palette) NE 0 THEN BEGIN
       rrr = Congrid(rr, ncolors)
       ggg = Congrid(gg, ncolors)
       bbb = Congrid(bb, ncolors)
       TVLCT, rrr, ggg, bbb, bottom
    ENDIF
    IF N_ELEMENTS(charsize) EQ 0 THEN charsize = cgDefCharsize() * charPercent
    IF N_ELEMENTS(format) EQ 0 THEN format = ""
    IF N_Elements(nodisplay) EQ 0 THEN nodisplay = 1
    minrange = (N_ELEMENTS(minrange) EQ 0) ? 0. : Float(minrange[0])
    maxrange = (N_ELEMENTS(maxrange) EQ 0) ? Float(ncolors) : Float(maxrange[0])
    IF N_ELEMENTS(ticklen) EQ 0 THEN ticklen = 0.25
    IF N_ELEMENTS(minor) EQ 0 THEN minor = 2
    IF N_ELEMENTS(range) NE 0 THEN BEGIN
       minrange = Float(range[0])
       maxrange = Float(range[1])
    ENDIF
    IF N_ELEMENTS(divisions) EQ 0 THEN BEGIN
      IF format EQ "" THEN divisions = 0 ELSE divisions = 6
    ENDIF
    IF N_ELEMENTS(font) EQ 0 THEN font = !P.Font
    IF N_ELEMENTS(title) EQ 0 THEN title = ''
    xlog = Keyword_Set(xlog)
    ylog = Keyword_Set(ylog)

    ; You can't have a format set *and* use ticknames.
    IF N_ELEMENTS(ticknames) NE 0 THEN format = ""

    ; If the format is NOT null, then format the ticknames yourself.
    ; Can't assume minrange is less than maxrange.
    IF (xlog XOR ylog) EQ 0 THEN BEGIN
        IF format NE "" THEN BEGIN
           IF minrange LT maxrange THEN BEGIN
               step = (maxrange - minrange) / divisions
               levels = minrange > (Indgen(divisions+1) * step + minrange) < maxrange
               IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
               ticknames = String(levels, Format=format)
               format = "" ; No formats allowed in PLOT call now that we have ticknames.
           ENDIF ELSE BEGIN
               step = (minrange - maxrange) / divisions
               levels = maxrange > (Indgen(divisions+1) * step + maxrange) < minrange
               levels = Reverse(levels)
               IF StrPos(StrLowCase(format), 'i') NE -1 THEN levels = Round(levels)
               ticknames = String(levels, Format=format)
               format = "" ; No formats allowed in PLOT call now that we have ticknames.
           ENDELSE
        ENDIF
    ENDIF

    IF KEYWORD_SET(vertical) THEN BEGIN
       bar = REPLICATE(1B,20) # BINDGEN(ncolors)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 2)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          position = [0.88, 0.1, 0.95, 0.9]
       ENDIF ELSE BEGIN
          IF position[2]-position[0] GT position[3]-position[1] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
      IF Keyword_Set(fit) THEN BEGIN
            position[[1,3]] = !Y.Window
            distance = position[2] - position[0]
            IF Keyword_Set(right) THEN BEGIN
                position[0] = !X.Window[1] + ((4*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDIF ELSE BEGIN
                position[0] = !X.Window[1] + ((10*!D.X_CH_SIZE*charsize) / !D.X_Size)
            ENDELSE
            position[2] = position[0] + distance
       ENDIF
    ENDIF ELSE BEGIN
       bar = BINDGEN(ncolors) # REPLICATE(1B, 20)
       IF Keyword_Set(invertcolors) THEN bar = Reverse(bar, 1)
       IF N_ELEMENTS(position) EQ 0 THEN BEGIN
          position = [0.1, 0.88, 0.9, 0.95]
       ENDIF ELSE BEGIN
          IF position[3]-position[1] GT position[2]-position[0] THEN BEGIN
             position = [position[1], position[0], position[3], position[2]]
          ENDIF
          IF position[0] GE position[2] THEN Message, "Position coordinates can't be reconciled."
          IF position[1] GE position[3] THEN Message, "Position coordinates can't be reconciled."
       ENDELSE
       IF Keyword_Set(fit) THEN BEGIN
            position[[0,2]] = !X.Window
            distance = position[3] - position[1]
            position[1] = !Y.Window[1] + ((4*!D.Y_CH_SIZE*charsize) / !D.Y_Size)
            position[3] = position[1] + distance
       ENDIF
     ENDELSE

     ; Scale the color bar.
     IF N_Elements(clamp) NE 0 THEN BEGIN
        IF N_Elements(clamp) NE 2 THEN Message, 'The CLAMP keyword must be a two-element array.'
        byterange = BytScl(clamp, minrange, maxrange)
        tempbar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom)) + bottom   
        bar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom), MIN=byterange[0], MAX=byterange[1]) + bottom 
        IF N_Elements(neutralIndex) EQ 0 THEN BEGIN
            neutralBottom = (ncolors-1) < (255-bottom)
            neutralTop = bottom
        ENDIF ELSE BEGIN
            neutralBottom = neutralIndex
            neutralTop = neutralIndex
        ENDELSE
        i = Where(tempbar LT byterange[0], count)
        IF count GT 0 THEN bar[i] = neutralBottom
        i = Where(tempbar GT byterange[1], count)
        IF count GT 0 THEN bar[i] = neutralTop
        
         
     ENDIF ELSE BEGIN
        bar = BYTSCL(bar, TOP=(ncolors-1) < (255-bottom)) + bottom
     ENDELSE

     IF Keyword_Set(reverse) THEN BEGIN
       IF Keyword_Set(vertical) THEN bar = Reverse(bar,2) ELSE bar = Reverse(bar,1)
     ENDIF

    ; Get starting locations in NORMAL coordinates.
    xstart = position[0]
    ystart = position[1]

    ; Get the size of the bar in NORMAL coordinates.
    xsize = (position[2] - position[0])
    ysize = (position[3] - position[1])

       
    ; Decomposed color off if device supports it.
    SetDecomposedState, 0, CURRENTSTATE=currentState
       
    ; Display the color bar in the window. Sizing is
    ; different for PostScript and regular display.
    IF scalablePixels THEN BEGIN

       ; Display the color bar.
       TV, bar, xstart, ystart, XSIZE=xsize, YSIZE=ysize, /Normal

    ENDIF ELSE BEGIN

       bar = CONGRID(bar, CEIL(xsize*!D.X_VSize), CEIL(ysize*!D.Y_VSize))

       ; Display the color bar.
       TV, bar, xstart, ystart, /Normal

    ENDELSE
   
    ; Restore the decomposed state if needed.
    IF currentState THEN SetDecomposedState, 1

    ; Get the current colortable.
    TVLCT, rr, gg, bb, /GET
    
    ; Annotate the color bar.
    IF (!D.Name EQ 'PS') AND N_Elements(annotateColor) EQ 0 THEN BEGIN
        annotateColor = 'black'
    ENDIF ELSE BEGIN
        IF N_Elements(annotateColor) EQ 0 THEN BEGIN
            IF (!D.Window GE 0) AND ~scalablePixels THEN BEGIN
                pixel = cgSnapshot(!D.X_Size-1,  !D.Y_Size-1, 1, 1)
                IF N_ELEMENTS(color) EQ 0 THEN BEGIN
                    IF Total(pixel) EQ 765 THEN annotateColor = 'black'
                    IF Total(pixel) EQ 0 THEN annotateColor = 'white'
                    IF N_Elements(annotateColor) EQ 0 THEN annotateColor = 'opposite'
                ENDIF ELSE BEGIN
                     IF Size(color, /TNAME) EQ 'STRING' THEN annotateColor = color
                ENDELSE
            ENDIF ELSE IF N_ELEMENTS(annotateColor) EQ 0 THEN annotateColor = 'opposite'
        ENDIF 
    ENDELSE
    IF N_Elements(annotateColor) EQ 0 THEN annotateColor = 'opposite'
    
    ; If color is undefined, use the annotate color.
    IF N_Elements(color) EQ 0 THEN BEGIN
        IF Size(annotateColor, /TNAME) EQ 'STRING' THEN BEGIN
            color = cgColor(annotateColor)
        ENDIF ELSE BEGIN
            color = annotateColor
        ENDELSE
    ENDIF 
    
    IF KEYWORD_SET(vertical) THEN BEGIN

       IF KEYWORD_SET(right) THEN BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1, $
             YTICKS=divisions, XSTYLE=1, YSTYLE=9, XTITLE="", YTITLE="", $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             XTICKFORMAT='(A1)', YTICKFORMAT='(A1)', YMINOR=minor, _STRICT_EXTRA=extra, $
             YTICKNAME=ticknames, FONT=font, YLOG=ylog

          AXIS, YAXIS=1, YRANGE=[minrange, maxrange], YTICKFORMAT=format, YTICKS=divisions, $
             YTICKLEN=ticklen, YSTYLE=1, COLOR=color, CHARSIZE=charsize, XTITLE="", $
             FONT=font, YTITLE=title, _STRICT_EXTRA=extra, YMINOR=minor, YTICKNAME=ticknames, YLOG=ylog

       ENDIF ELSE BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=1,  $
             YTICKS=divisions, YSTYLE=1, XSTYLE=1, YTITLE=title, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             XTICKFORMAT='(A1)', YTICKFORMAT=format, YMinor=minor, _STRICT_EXTRA=extra, $
             YTICKNAME=ticknames, YLOG=ylog, YTICKLEN=ticklen, FONT=font, XTITLE=""

       ENDELSE

    ENDIF ELSE BEGIN

       IF KEYWORD_SET(top) THEN BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=9, YSTYLE=1, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT='(A1)', XTICKLEN=ticklen, $
             XRANGE=[minrange, maxrange], FONT=font, XMINOR=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE=""

          AXIS, XTICKS=divisions, XSTYLE=1, COLOR=color, CHARSIZE=charsize, $
             XTICKFORMAT=format, XTICKLEN=ticklen, XRANGE=[minrange, maxrange], XAXIS=1, $
             FONT=font, XTITLE=title, _STRICT_EXTRA=extra, XCHARSIZE=charsize, XMINOR=minor, $
             XTICKNAME=ticknames, XLOG=xlog, YTITLE=""

       ENDIF ELSE BEGIN

          PLOT, [minrange,maxrange], [minrange,maxrange], /NODATA, XTICKS=divisions, $
             YTICKS=1, XSTYLE=1, YSTYLE=1, TITLE=title, $
             POSITION=position, COLOR=color, CHARSIZE=charsize, /NOERASE, $
             YTICKFORMAT='(A1)', XTICKFORMAT=format, XTICKLEN=ticklen, $
             XRANGE=[minrange, maxrange], FONT=font, XMinor=minor, _STRICT_EXTRA=extra, $
             XTICKNAME=ticknames, XLOG=xlog, XTITLE="", YTITLE=""

        ENDELSE

    ENDELSE

    ; Restore the previous plot and map system variables.
    !P = bang_p
    !X = bang_x
    !Y = bang_y
    !Z = bang_z
    !Map = bang_map
    
    ; Set the current colors back.
    IF !D.Name NE 'Z' THEN TVLCT, r, g, b
END
