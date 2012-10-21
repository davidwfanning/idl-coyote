PRO HistoImage::GetProperty, $
   Binsize=binsize, $            ; The bin size of the histogram.
   DataColor=datacolor, $        ; The data color of the histogram.
   Max_Value=max_value, $        ; The maximum value of the histogram plot.
   _Ref_Extra=extra              ; Extra keywords sent to the BoxImage superclass.

; This method sets the properties of the object. Extra keywords
; are passed along to and retrieved from the BoxImage superclass object.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg + ' Returning...', $
      Traceback=1, /Error)
   RETURN
ENDIF

   ; Set properties if keyword is present.

IF Arg_Present(binsize) NE 0 THEN binsize = self.binsize
IF Arg_Present(datacolor) NE 0 THEN datacolor = self.datacolor
IF Arg_Present(max_value) NE 0 THEN max_value = self.max_value

   ; Pass extra keywords along to the BoxImage superclass.

self->BoxImage::GetProperty, _Extra=extra

END ;--------------------------------------------------------------------



PRO HistoImage::SetProperty, $
   Binsize=binsize, $            ; The bin size of the histogram.
   DataColor=datacolor, $        ; The data color of the histogram.
   Max_Value=max_value, $        ; The maximum value of the histogram plot.
   _Extra=extra                  ; Extra keywords sent to the BoxImage superclass.

; This method sets the properties of the object. Extra keywords
; are passed along to and set properties in the BoxImage superclass object.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg + ' Returning...', $
      Traceback=1, /Error)
   RETURN
ENDIF

   ; Set properties if keyword is present.

IF N_Elements(binsize) NE 0 THEN self.binsize = binsize
IF N_Elements(datacolor) NE 0 THEN self.datacolor = datacolor
IF N_Elements(max_value) NE 0 THEN self.max_value = max_value

   ; Pass extra keywords along to the BoxImage superclass.

self->BoxImage::SetProperty, _Extra=extra

END ;--------------------------------------------------------------------



PRO HistoImage::DataColor, Draw=draw, _Extra=extra

; This method changes the data color.

thisColorName = PickColorName(self.datacolor, Cancel=cancelled, $
   _Extra=extra, Title='Data Color')
IF cancelled THEN RETURN

self.datacolor = thisColorName

   ; Redraw the image if needed.

IF Keyword_Set(draw) THEN self->Draw

END ;--------------------------------------------------------------------



PRO HistoImage::Draw, $
   Font=font, $                  ; Type of font wanted for output.
   BMP=bmp, $                    ; Write to BMP file if set.
   GIF=gif, $                    ; Write to GIF file if set.
   JPEG=jpeg, $                  ; Write to JPEG file if set.
   PICT=pict, $                  ; Write to PICT file if set.
   PNG=png, $                    ; Write to PNG file if set.
   TIFF=Tiff, $                  ; Write to TIFF file if set.
   PostScript=postscript, $      ; Write to PostScript file if set.
   PS=ps, $                      ; Write to PostScript file if set.
   Printer=printer, $            ; Write directly to Printer if set.
   _Extra=extra                  ; Extra keywords (e.g., for PSConfig).

; This method draws the graphics display. Polymorpphism is illustrated
; the the draw method working in a variety of devices.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg + ' Returning...', $
      Traceback=1, /Error)
   RETURN
ENDIF

   ; Check keywords.

IF N_Elements(font) EQ 0 THEN font = !P.Font

   ; Special output?

output = ""
IF Keyword_Set(bmp) THEN output = 'BMP'
IF Keyword_Set(gif) THEN output = 'GIF'
IF Keyword_Set(jpeg) THEN output = 'JPEG'
IF Keyword_Set(pict) THEN output = 'PICT'
IF Keyword_Set(png) THEN output = 'PNG'
IF Keyword_Set(tiff) THEN output = 'TIFF'
IF Keyword_Set(postscript) THEN output = 'PS'
IF Keyword_Set(ps) THEN output = 'PS'
IF Keyword_Set(printer) THEN output = 'PRINTER'
IF output NE "" THEN thisDevice = !D.Name

   ; Setup based on type of output.

CASE output OF

   "": BEGIN
         annotateColor = GetColor(self.annotatecolor, !D.Table_Size-2)
         backColor = GetColor(self.backColor, !D.Table_Size-3)
         dataColor = GetColor(self.dataColor, !D.Table_Size-4)
         TVLCT, *self.r, *self.g, *self.b
         END

   "PS": BEGIN
         keywords = PSConfig(Color=1, Filename='histoimage.ps', $
            _Extra=extra, Cancel=cancelled)
         IF cancelled THEN RETURN ELSE keywords.color = 1
         Set_Plot, 'PS'
         Device, _Extra=keywords
         annotateColor = GetColor('Navy', !D.Table_Size-2)
         backColor = GetColor('White', !D.Table_Size-3)
         dataColor = GetColor('Black', !D.Table_Size-4)
         TVLCT, *self.r, *self.g, *self.b
         END

   "PRINTER": BEGIN
         ok = Dialog_PrinterSetup()
         IF NOT ok THEN RETURN
         keywords = PSWindow(/Printer, /Landscape, Fudge=0.25, _Extra=extra)
         annotateColor = GetColor('Black', !D.Table_Size-2)
         backColor = GetColor('Charcoal', !D.Table_Size-3)
         dataColor = GetColor('Black', !D.Table_Size-4)
         TVLCT, *self.r, *self.g, *self.b
         Set_Plot, 'PRINTER', /Copy
         Device, Landscape=1
         Device, _Extra=extra
         thisThickness = !P.Thick
         font = 1
         !P.Thick = 2
         END

   ELSE: BEGIN
         ncolors = !D.Table_Size
         Set_Plot, 'Z'
         Device, Set_Resolution=[500, 500], Set_Colors=ncolors, _Extra=extra
         Erase
         annotateColor = GetColor(self.annotatecolor, ncolors-2)
         backColor = GetColor(self.backColor, ncolors-3)
         dataColor = GetColor(self.dataColor, ncolors-4)
         TVLCT, *self.r, *self.g, *self.b
         END

ENDCASE

   ; Calculate the position of the image, color bar,
   ; and histogram plots in the window.

IF self.vertical THEN BEGIN
   p = self.position
   length = p[2] - p[0]
   imgpos = [p[0], p[1], (p[0] + (0.75*length)), p[3]-(length*0.350)]
   cbpos =  [p[2]-0.05, p[1], p[2], p[3]-(length*0.35)]
   hpos =   [p[0], imgpos[3]+0.1, p[2], p[3]]
ENDIF ELSE BEGIN
   p = self.position
   height = p[3] - p[1]
   imgpos = [p[0], p[1], p[2], p[1]+ 0.4*height]
   cbpos =  [p[0], imgpos[3]+height*0.1, p[2], imgpos[3]+height*0.15]
   hpos =   [p[0], cbpos[3]+height*0.125, p[2], p[3]]
ENDELSE

   ; Calculate appropriate character size for plots.

IF output EQ 'PRINTER' THEN thisCharsize = 1.25 ELSE $
   thisCharsize = Str_Size('A Sample String', 0.20)

   ; Calculate the histogram.

histdata = Histogram(*self.process, Binsize=self.binsize, $
   Max=Max(*self.process), Min=Min(*self.process))

   ; Have to fudge the bins and histdata variables to get the
   ; histogram plot to make sense.

npts = N_Elements(histdata)
halfbinsize = self.binsize / 2.0
bins = Findgen(N_Elements(histdata)) * self.binsize + Min(*self.process)
binsToPlot = [bins[0], bins + halfbinsize, bins[npts-1] + self.binsize]
histdataToPlot = [histdata[0], histdata, histdata[npts-1]]
xrange = [Min(binsToPlot), Max(binsToPlot)]

  ; Plot the histogram of the display image. Axes first.

Plot, binsToPlot, histdataToPlot, $  ; The fudged histogram and bin data.
   Background=backcolor, $           ; The background color of the display.
   Charsize=thisCharsize, $          ; The character size, as determined by Str_Size.
   Color=annotateColor, $            ; The color of the axes.
   Font=font, $                      ; The font type.
   Max_Value=self.max_value, $       ; The maximum value of the plot.
   NoData=1, $                       ; Draw the axes only. No data.
   Position=hpos, $                  ; The position of the plot in the window.
   Title='Image Histogram', $        ; The title of the plot.
   XRange=xrange, $                  ; The X data range.
   XStyle=1, $                       ; Exact axis scaling. No autoscaled axes.
   XTickformat='(I6)', $             ; The format of the X axis annotations.
   XTitle='Image Value', $           ; The title of the X axis.
   YMinor=1, $                       ; One minor tick mark on X axis.
   YRange=[0,self.max_value], $      ; The Y data range.
   YStyle=1, $                       ; Exact axis scaling. No autoscaled axes.
   YTickformat='(I6)', $             ; The format of the Y axis annotations.
   YTitle='Pixel Density', $         ; The title of the Y axis.
   _Extra=*self.extra                ; Pass any extra PLOT keywords.

   ; Overplot the histogram data in the data color.

OPlot, binsToPlot, histdataToPlot, PSym=10, Color=dataColor

   ; Make histogram boxes by drawing lines in data color.

FOR j=1L,N_Elements(bins)-2 DO BEGIN
   PlotS, [bins[j], bins[j]], [!Y.CRange[0], histdata[j] < self.max_value], $
          Color=dataColor, _Extra=*self.extra
ENDFOR

   ; Draw the image and color bar.

TVImage, BytScl(*self.process, Top=self.ncolors-1), $
   Position=imgpos, _Extra=*self.extra
Plot, self.xscale, self.yscale, XStyle=1, YStyle=1, $
   XTitle=self.xtitle, YTitle=self.ytitle, Color=annotateColor, $
   Position=imgpos, /NoErase, /NoData, Ticklen=-0.025, _Extra=*self.extra, $
   CharSize=thisCharSize, Font=font
FSC_Colorbar, Range=[Min(*self.process), Max(*self.process)], Divisions=8, $
   _Extra=*self.extra, Color=annotateColor, Position=cbpos, Ticklen=-0.2, $
   Vertical=self.vertical, NColors=self.ncolors, CharSize=thisCharSize, Font=font

   ; Do you need file output? Get a screen dump and write the file.

CASE output OF
   "BMP":  image = TVRead(/BMP,  Filename='histoimage')
   "GIF":  image = TVRead(/GIF,  Filename='histoimage')
   "JPEG": image = TVRead(/JPEG, Filename='histoimage')
   "PNG":  image = TVRead(/PNG,  Filename='histoimage')
   "PICT": image = TVRead(/PICT, Filename='histoimage')
   "TIFF": image = TVRead(/TIFF, Filename='histoimage')
   "PS": Device, Close_File=1
   "PRINTER": BEGIN
      Device, Close_Document=1
      !P.Thick = thisThickness
      END
   ELSE:
ENDCASE
IF output NE "" THEN Set_Plot, thisDevice

END ;--------------------------------------------------------------------



FUNCTION HistoImage::Init, $
   image, $
   Binsize=binsize, $             ; The bin size of the histogram.
   DataColor=datacolor, $         ; The data color.
   Max_Value=max_value, $         ; The maximum value of the histogram plot.
   _Extra=extra                   ; Holds extra keywords.

; The initialization routine for the object. Create the
; particular instance of the object class.

   ; Error handling.

Catch, theError
IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   ok = Error_Message(!Error_State.Msg + ' Returning...', $
      Traceback=1, /Error)
   RETURN, 0
ENDIF

   ; Check for positional parameter. Define if necessary.

IF N_Elements(image) EQ 0 THEN image = LoadData(7)
ndims = Size(image, /N_Dimensions)
IF ndims NE 2 THEN Message, 'Image must be 2D array.', /NoName

   ; Check for keyword parameters.

IF N_Elements(datacolor) EQ 0 THEN datacolor = "RED"
IF N_Elements(max_value) EQ 0 THEN max_value = 5000.0
IF N_Elements(binsize) EQ 0 THEN BEGIN
   range = Max(image) - Min(image)
   binsize = 2.0 > (range / 128.0)
ENDIF

   ; Initialize the BoxImage superclass object.

IF NOT self->BoxImage::Init(image, _Extra=extra, NColors=!D.Table_Size-4) THEN RETURN, 0

   ; Populate the rest of the self object.

self.max_value = max_value
self.datacolor = datacolor
self.binsize = binsize

RETURN, 1
END ;--------------------------------------------------------------------


PRO HistoImage__Define

; The definition of the HISTOIMAGE object class.

   struct = { HISTOIMAGE, $           ; The HISTOIMAGE object class.
              INHERITS BoxImage, $    ; Inherit the BoxImage object class.
              binsize: 0.0, $         ; The histogram bin size.
              max_value: 0.0, $       ; The maximum value of the histogram plot.
              datacolor: "" $         ; The data color name.
            }
END ;--------------------------------------------------------------------
