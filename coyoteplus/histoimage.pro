PRO HistoImage, $                   ; The program name.
   image, $                         ; The image data.
   AxisColorName=axisColorName, $   ; The axis color.
   BackColorName=backcolorName, $   ; The background color.
   Binsize=binsize, $               ; The histogram bin size.
   ColorTable=colortable, $         ; The colortable index to load.
   DataColorName=datacolorName, $   ; The data color.
   Debug=debug, $                   ; A debug flag variable.
   _Extra=extra, $                  ; For passing extra keywords.
   ImageColors=imagecolors, $       ; The number of image colors used. (Out)
   Max_Value=max_value, $           ; The maximum value of the histogram plot.
   NoLoadCT=noloadct, $             ; A flag to not load the image color table.
   XScale=xscale, $                 ; The scale for the X axis of the image.
   YScale=yscale                    ; The scale for the Y axis of the image.

   ; Catch any error in the HistoImage program.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg + ' Returning...', $
      Traceback=Keyword_Set(debug))
   RETURN
ENDIF

   ; Check for positional parameter. Define if necessary.
   ; Make sure it is correct size.

IF N_Elements(image) EQ 0 THEN image = LoadData(7)
ndim = Size(image, /N_Dimensions)
IF ndim NE 2 THEN Message, '2D Image Variable Required.', /NoName

   ; Check for histogram keywords.

IF N_Elements(binsize) EQ 0 THEN BEGIN
   range = Max(image) - Min(image)
   binsize = 2.0 > (range / 128.0)
ENDIF

IF N_Elements(max_value) EQ 0 THEN max_value = 5000.0

   ; Check for image scale parameters.

s = Size(image, /Dimensions)
IF N_Elements(xscale) EQ 0 THEN xscale = [0, s[0]]
IF N_Elements(xscale) NE 2 THEN Message, 'XSCALE must be 2-element array', /NoName
IF N_Elements(yscale) EQ 0 THEN yscale = [0, s[1]]
IF N_Elements(yscale) NE 2 THEN Message, 'YSCALE must be 2-element array', /NoName

   ; Check for color keywords.

IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Red"
IF N_Elements(axisColorName) EQ 0 THEN axisColorName = "Navy"
IF N_Elements(backColorName) EQ 0 THEN backColorName = "White"
IF N_Elements(colortable) EQ 0 THEN colortable = 4
colortable = 0 > colortable < 40
imagecolors = !D.Table_Size-4

   ; Load plot colors.

axisColor = GetColor(axisColorName, !D.Table_Size-2)
dataColor = GetColor(datacolorName, !D.Table_Size-3)
backColor = GetColor(backColorName, !D.Table_Size-4)

   ; I don't always want to load a color table. Sometimes I
   ; want to control colors outside the program. Check
   ; the NOLoadCT keyword before loading.

IF NOT Keyword_Set(noloadct) THEN BEGIN
   LoadCT, colortable, NColors=imagecolors, /Silent
ENDIF ELSE BEGIN

   ; This code placed here to work around an obscure
   ; PRINTER bug in IDL 5.3 that causes all pixels with
   ; value 0 to be displayed in the last single color
   ; loaded (the backColor, in this case).

   IF !D.NAME EQ 'PRINTER' THEN BEGIN

       ; Just get the color table vectors and re-load the colors.

      TVLCT, r, g, b, /Get
      TVLCT, r, g, b
   ENDIF
ENDELSE


   ; Determine positions of graphics in window.

histoPos =    [0.15, 0.675, 0.95, 0.950]
colorbarPos = [0.15, 0.500, 0.95, 0.550]
imagePos =    [0.15, 0.100, 0.95, 0.400]

   ; Calculate appropriate character size for plots.

thisCharsize = Str_Size('A Sample String', 0.20)

   ; Calculate the histogram.

histdata = Histogram(image, Binsize=binsize, Max=Max(image), Min=Min(image))

   ; Have to fudge the bins and histdata variables to get the
   ; histogram plot to make sense.

npts = N_Elements(histdata)
halfbinsize = binsize / 2.0
bins = Findgen(N_Elements(histdata)) * binsize + Min(image)
binsToPlot = [bins[0], bins + halfbinsize, bins[npts-1] + binsize]
histdataToPlot = [histdata[0], histdata, histdata[npts-1]]
xrange = [Min(binsToPlot), Max(binsToPlot)]

  ; Plot the histogram of the display image. Axes first.

Plot, binsToPlot, histdataToPlot, $  ; The fudged histogram and bin data.
   Background=backcolor, $           ; The background color of the display.
   Charsize=thisCharsize, $          ; The character size, as determined by Str_Size.
   Color=axiscolor, $                ; The color of the axes.
   Max_Value=max_value, $            ; The maximum value of the plot.
   NoData=1, $                       ; Draw the axes only. No data.
   Position=histoPos, $              ; The position of the plot in the window.
   Title='Image Histogram', $        ; The title of the plot.
   XRange=xrange, $                  ; The X data range.
   XStyle=1, $                       ; Exact axis scaling. No autoscaled axes.
   XTickformat='(I6)', $             ; The format of the X axis annotations.
   XTitle='Image Value', $           ; The title of the X axis.
   YMinor=1, $                       ; One minor tick mark on X axis.
   YRange=[0,max_value], $           ; The Y data range.
   YStyle=1, $                       ; Exact axis scaling. No autoscaled axes.
   YTickformat='(I6)', $             ; The format of the Y axis annotations.
   YTitle='Pixel Density', $         ; The title of the Y axis.
   _Extra=extra                      ; Pass any extra PLOT keywords.

   ; Overplot the histogram data in the data color.

OPlot, binsToPlot, histdataToPlot, PSym=10, Color=dataColor

   ; Make histogram boxes by drawing lines in data color.

FOR j=1L,N_Elements(bins)-2 DO BEGIN
   PlotS, [bins[j], bins[j]], [!Y.CRange[0], histdata[j] < max_value], $
          Color=dataColor
ENDFOR

   ; Display the colorbar.

cbarRange = [Min(binsToPlot), Max(binsToPlot)]
FSC_Colorbar, $
   Charsize=thisCharsize, $    ; The character size as determined by Str_Size.
   Color=axisColor, $          ; The annotation is is the axis color.
   Divisions=0, $              ; Use default PLOT divisions by setting to 0.
   NColors=imagecolors, $      ; The number of image colors.
   Position=colorbarPos, $     ; The position of the colorbar in the window.
   Range=cbarRange, $          ; The range of the color bar.
   XTicklen=-0.2, $            ; Outward facing tick marks.
   _Extra=extra                ; Any extra FSC_COLORBAR keywords.

   ; Display the image.

TVImage, BytScl(image, Top=imagecolors-1), Position=imagePos, _Extra=extra

Plot, xscale, yscale, $
   Charsize=thisCharsize, $  ; The character size as determined by Str_Size.
   Color=axisColor, $        ; The outline should be in the axes color.
   NoData=1, $               ; No data. Draw axes only.
   NoErase=1, $              ; Don't erase what is already in the window.
   Position=imagePos, $      ; The position of the axes around the image.
   XStyle=1, $               ; No axis autoscaling.
   XTicklen=-0.025, $        ; Outward facing tick marks.
   YStyle=1, $               ; No axis autoscaling
   YTicklen=-0.025, $        ; Outward facing tick marks.
    _Extra=extra             ; Any extra PLOT keywords.

END