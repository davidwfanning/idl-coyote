;+
; NAME:
;       XSTRETCH
;
; PURPOSE:
;
;       The purpose of this program is to allow the user to perform a variety
;       of image pixel transformations, commonly known as "contrast stretching".
;       The program supports the following stretches:
;
;       LINEAR         Linear stretch between end points.
;       LINEAR 2%      Linear, except 2% of pixels are clipped at either end of histogram.
;       GAMMA          An exponential function.
;       LOG            An S-shaped log function.
;       ASINH          An inverse hyperbolic sine function (strong log function).
;       SQUARE ROOT    Another type of log function.
;       EQUALIZATION   Image histogram is equalized before stretching.
;       ADAPTIVE EQUALIZATION Image histogram is equalized with Adapt_Hist_Equal before stretching.
;       GAUSSIAN       A gaussian normal distribution function.
;
;       An image histogram is provided to the user as an aid in manipulating
;       the stretch parameters.
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
;       Image Processing, Widgets
;
; CALLING SEQUENCE:
;
;       XSTRETCH, image
;
; INPUT PARAMETERS:
;
;       image:         The image data to be stretched. It must be 2D array or a
;                      pointer to a 2D array.
;
; KEYWORD PARAMETERS:
;
;       BETA:          The "softening parameter" associated with the ASINH stretch. (See ASINHSCL for
;                      details.) Set to 3 by default.
;
;       BLOCK:         Set this keyword if you wish the program to be a blocking widget.
;       
;       BREWER:        Set this keyword if you wish to use the Brewer Colors, as explained
;                      in this reference: http://www.dfanning.com/color_tips/brewer.html. The
;                      Brewer color table file, fsc_brewer.tbl must be found somewhere in your
;                      IDL path for this option to be available. The file can be downloaded here.
;
;                      http://www.dfanning.com/programs/fsc_brewer.tbl
;
;       COLORS:        A five element string array, listing the FSC_COLORS colors for drawing the
;                      histogram plot. The colors are used as follows:
;
;                      colors[0] : Background color. Default: "white".
;                      colors[1] : Axis color. Default: "black".
;                      colors[2] : Min threshold color. Default: "firebrick".
;                      colors[3] : Max threshold  color. Default: "steel blue".
;                      colors[4] : ASinh color. Default: "grn6".
;                      colors[5] : Histogram color. Default: "charcoal".
;
;                      If a particular color is represented as a null string, then the
;                      default for that color is used.
;
;       COLORTABLE:    The index of a colortable you would like to load.
;                      A gray-scale colortable is used if this parameter is not provided.
;
;       EXPONENT:      The "exponent" parameter of a LOG stretch. (See LOGSCL for details.)
;                      Set to 4.0 by default.
;
;       _EXTRA:        This keyword collects any keyword appropriate for the
;                      Plot command, which is used to display the image histogram.
;
;       FILENAME:      If no image is supplied as a positional parameter, this keyword can be
;                      used to specify the name of an image file. The image must be capable of
;                      being read by SELECTIMAGE, so that means these kinds of files with these
;                      file extensions:
;
;                      TYPE      FILE EXTENSION
;                      BMP       *.bmp
;                      DICOM     *.dcm
;                      FITS      *.fits, *.fts (requires NASA ASTRO library on IDL Path)
;                      GIF       *.gif (IDL 6.2 and higher)
;                      JPEG      *.jpg, *.jpeg, *.jpe
;                      JPEG2000  *.jpf, *.jpx, *.jp2, *.j2c, *.j2k
;                      PICT      *.pict
;                      PNG       *.png
;                      TIFF      *.tif, *tiff
;
;       GAMMA:         The gamma scale factor. (See GMASCL for details.) Set to 1.5 by default.
;
;       GROUP_LEADER:  Keyword to assign a group leader (so this program can be
;                      called from within another widget program).
;
;       MAX_VALUE:     Keyword to assign a maximun value for the normalized Histogram Plot.
;                      Images with lots of pixels of one color (e.g. black) skew
;                      the histogram. This helps make a better looking plot. Set by default
;                      to the maximum value of the histogram data.
;
;       MINTHRESH:     The initial minimun threshold value for the stretch.
; 
;       MAXTHRESH:     The initial maximum threshold value for the stretch.  
;
;       MEAN:          The "mean" parameter of a LOG stretch. (See LOGSCL for details.)
;                      Set to 0.5 by default.
;
;       NEGATIVE:      Set this keyword if you prefer to see a negative image, rather
;                      that the normal positive image.
;
;       NO_WINDOW:     Set this keyword if you do no want the program to display an
;                      image window. This would be the case, for example, if you
;                      are displaying the image in your own window and your program
;                      is being notified of images changes via the NOTIFY_PRO or
;                      NOTIFY_OBJ keywords.
;
;       NOTIFY_OBJ:    Set this keyword to a structure containing the fields OBJECT
;                      and METHOD. When the image is changed, the object identified in
;                      the OBJECT field will have the method identified in the METHOD
;                      field called. The method should be written to accept one positional
;                      parameter. The parameter passed to the method is a structure as defined
;                      below.
;
;       NOTIFY_PRO:    Set this keyword to the name of a procedure that should
;                      be notified when the image is changed. The procedure should
;                      be defined with one positional parameter. The parameter passed
;                      to the procedure is a structure defined as below.
;
;       SIGMA:         The amount of width applied to the Gaussian stretch. Default is 1.
;
;       TITLE:         The title of the histogram window. By default: 'Drag Vertical Lines to STRETCH Image Contrast'.
;
;       TYPE:          The type of stretch to be applied. May be either a string (e.g, 'GAMMA') or a number from
;                      the table below:
;
;           Number   Type of Stretch

;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;
;       UVALUE:        Simply a holder for any IDL variable. If this is defined, then the structure
;                      passed via NOTIFY_OBJ or NOTIFY_PRO will contain an extra field named UVALUE.
;                      
;       XPOS:          The X position of the histogram window in pixels from upper-left
;                      of display. By default, 100.
;
;       YPOS:          The Y position of the histogram window in pixels from upper-left
;                      of display. By default, 100.
;
; OUTPUTS:
;
;       The image and histogram windows can be save as BMP, JPEG, PNG, PICT, TIFF, GIF, and PostScript output
;       files from the Control pull-down menu.
;
;       The stretched image, the stretched image histogram (in 256-element format, suitable for
;       input into HISTOMATCH), and the current stretch parameters can be saved as main-level IDL
;       variables from the Control pull-down menu. If you choose to save "parameters", a structure
;       of this form is saved:
;
;               struct = { minThresh: info.minThresh, $
;                          maxThresh: info.maxThresh, $
;                          gamma: info.gamma, $
;                          beta: info.beta, $
;                          mean: info.mean, $
;                          exponent: info.exponent, $
;                          type: info.type }
;
;      The TYPE field is a string that reflects the current stretch type at the time the
;      parameters were saved. This should be used to tell you which other fields in the structure
;      are pertinent for a stretch operation.
;
;      If you choose to save "everything", the saved variable is a structure similar to the one above,
;      except there are two additional fields: (1) IMAGE contains the stretched image, and HISTOGRAM
;      contains the stretch image histogram.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; NOTIFICATION STRUCTURE:
;
;       If you choose to be notified of changes in the image (NOTIFY_PRO or NOTIFY_OBJ),
;       the following structure will be sent as the first positional parameter:
;
;    struct = { image: stretchedImage, $       ; The stretched image.
;               r: info.r, $                   ; The R color vector associated with the image
;               g: info.g, $                   ; The G color vector associated with the image
;               b: info.b, $                   ; The B color vector associated with the image
;               type: info.type, $             ; The TYPE of stretch applied to the image.
;               minThresh: info.minThresh, $   ; The minimum threshold value.
;               maxThresh: info.maxThresh, $   ; The maximum threshold value.
;               beta: info.beta, $             ; The current BETA value.
;               gamma: info.gamma, $           ; The current GAMMA value.
;               mean: info.mean, $             ; The current MEAN value.
;               exponent: info.exponent, $     ; The current EXPONENT value.
;               sigma: info.sigma }            ; The current SIGMA value.
;               
;       A "uvalue" field might also be in this structure if something was passed with the UVALUE keyword.
;
; DEPENDENCIES:
;
;       Requires a number of files from the Coyote Library:
;
;       http://www.dfanning.com/programs/xstretch.zip
;          or
;       http://www.dfanning.com/programs/coyoteprograms.zip
;
;
; EXAMPLE:
;
;       If you have a 2D image in the variable "image", you can run this
;       program like this:
;
;       XSTRETCH, image
;       XSTRETCH, image, TYPE='GAMMA'
;       XSTRETCH, image, TYPE='LOG', EXPONENT=5.5
;       XSTRETCH, image, TYPE='ASINH', BETA=0.1
;
;       For a good ASINH image, try the FITS files located here:
;
;           http://cosmo.nyu.edu/hogg/visualization/rgb/
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, April 1996.
;       October, 1996 Fixed a problem with not restoring the color
;          table when the program exited. Substituted a call to XCOLORS
;          instead of XLOADCT.
;       October, 1998. Added NO_BLOCK keyword and modified to work with
;          24-bit color devices.
;       April, 1999. Made lines thicker. Offered default image. DWF.
;       April, 1999. Replaced TV command with TVIMAGE. DWF.
;       April, 1999. Made both windows resizeable. DWF.
;       April, 2000. Made several modifications to histogram plot and to
;          the way colors were handled. Added ability to pass pointer to
;          the image as well as image itself. DWF.
;       February 2001. Removed GIF file support for IDL 5.4 and fixed
;          a problem with cleaning up the pixmap. DWF.
;       October 29, 2002. Added ability to load an image file with GETIMAGE. DWF.
;       Added ability to store stretched image as main-level variable. Oct 30, 2002. DWF.
;       Fixed a problem with the image window aspect ratio being calculated
;          incorrectly. 2 Nov 2002. DWF.
;       Added ability to open formatted images as well as raw data files. 2 Nov 2002. DWF.
;       Fixed a couple of minor problems with resizing the histogram window. 4 Nov 2002. DWF.
;       Added NO_WINDOW and NOTIFY_PRO keywords. 4 Nov 2002. DWF.
;       Fixed a problem with the histogram plot when the minimum image value
;          is greater than 0. 8 Nov 2002. DWF.
;       Added NOTIFY_OBJ and BLOCK keywords. 16 Nov 2002. DWF.
;       Fixed some problems in which images that are NOT byte valued could not be
;          displayed property. 29 Dec 2005. DWF.
;       Added GIF images back in. 29 Dec 2005. DWF.
;       EXTENSIVE modifications, primarily so it would work perfectly with perverse
;          FITS images. 20 Feb. 2006. DWF.
;       Added NEGATIVE keyword. 20 Feb. 2006. DWF.
;       Added GAMMA stretch. 20 Feb. 2006. DWF.
;       Added LOG and ASINH stretches. 25 Feb. 2006. DWF.
;       Added ability to save the stretched image, the stretched image histogram, and
;          image stretch parameters as main-level IDL variables. 1 March 2006. DWF.
;       Removed restrictions for 2D images. Now allows 24-bit images (MxNx3) to
;          be loaded. 3 March 2006.
;       Modified the histogram plot xrange to use the output min and max from
;          the histogram. 3 March 2006.
;       Renamed IMGSCL to GMASCL to avoid name space conflicts. 8 March 2006. DWF.
;       Modifications to GUI for updated ASINHSCL function. Removed ALPHA keyword
;          and changed the definition of BETA. 25 April 2006. DWF.
;       Added LINEAR 2%, SQUARE ROOT, GAUSSIAN and EQUALIZATION stretches. 4 Sept 2007. DWF
;       Added MINTHRESH and MAXTHRESH keywords. 25 June 2008. DWF.
;       Fixed a problem with the plot coordinate system not being re-established correctly
;          under certain circumstances. 9 July 2008. DWF.
;       Fixed several small errors in using the TYPE keyword, and added the ability to
;          restore the original stretch to the image from the Controls menu. Bug here
;          is identified as bug in FSC_INPUTFIELD, so be sure to get this program, too. 1 October 2008. DWF.
;       Fixed a problem with the Linear 2% stretch giving incorrect results. 14 October 2008. DWF.
;       Added a NAN keyword to every HISTOGRAM call. 6 November 2008. DWF.
;       Modifed the way the histogram was drawn. 14 March 2009. DWF.
;       Add BREWER and UVALUE keywords to the program. 15 April 2009. DWF.
;       Improvements made to histogram plotting functionality to better handle bin size and
;           problems caused by NANs in the input image. 9 June 2009. DWF.
;        Whoops! Left a NAN keyword out of a STDDEV call, and this caused havoic with binsize. 25 Aug 2009 DWF.
;        Modifications for EQUALIZATION stretch and for working with large images. 26 Aug 2009. DWF.
;        Fixed a problem with too strict interpretation of PLOT keywords passed to it from legacy code. Changed
;           _STRICT_EXTRA on Plot command to _EXTRA. 26 Aug 2009. DWF.
;        Fixed a problem when choosing binsize for integer data types GE 12. 25 Jan 2010. DWF.
;        Fixed a small typo that caused the histogram to be taken of the square-root of the 
;           image, rather than the image itself. 31 March 2010, DWF.
;        Byte type data is causing me heartburn. Internally, use INT data when byte type data
;           is passed in. 10 April 2010. DWF.
;        Fixed a general problem when working with unsigned integer images. I now make sure
;           threshold values stay within the data range appropriate for the data type. 21 April 2010. DWF.
;        Fixed another problem with integer data types and bin size. 23 April 2010. DWF.
;        Fixed a problem with the display image when starting with a Square Root stretch. 23 April 2010. DWF.
;        Added ADAPTIVE EQUALIZATION stretch and changed default colors. 24 Nov 2010. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008-2010, by Fanning Software Consulting, Inc.                           ;
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
FUNCTION XSTRETCH_VALIDATE_THRESHOLD, threshold, info

   ; Catch any errors.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, Cancel=1
      void = Error_Message(/Traceback)
      RETURN, threshold
   ENDIF

   ; Make sure threshold is inside of the plot.
   threshold = info.xmin > threshold < info.xmax

   ; Make sure threshold doesn't exceed range of data type.
   CASE info.dataType OF
        'BYTE': threshold = 0B > threshold < 255B
        'INT': threshold = (-2L^15+1) > threshold < (2L^15-1)
        'UINT': threshold = 0 > threshold < (2L^16-1)
        'LONG': threshold = (-2L^31+1) > threshold < (2L^31-1)
        'ULONG': threshold = 0 > threshold < (2L^32-1)
        'LONG64': threshold = (-2LL^64+1) > threshold < (2LL^64-1)
        'ULONG64': threshold = 0 > threshold < (2LL^64-1)
        ELSE:
   ENDCASE

   ; Make sure threshold has the same type as the image, unless SQUARE ROOT stretch.
   IF info.type NE 'SQUARE ROOT' THEN $
      threshold = Convert_to_Type(threshold, Size(*info.image, /Type))
   IF Size(threshold, /TNAME) EQ 'BYTE' THEN threshold = Fix(threshold)

   RETURN, threshold
END ;--------------------------------------------------------------------



FUNCTION XSTRETCH_SCALEIMAGE, info

; Scales the image data appropriately, depending on scale type.

   ; Catch any errors.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, Cancel=1
      void = Error_Message(/Traceback)
      RETURN, *info.image
   ENDIF

   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   ; Turn floating underflow warnings off.
   thisExcept = !Except
   !Except = 0


   CASE info.type OF

      'LINEAR': BEGIN
         scaledImage = BytScl(*info.image, Max=info.maxThresh, Min=info.minThresh, /NAN)
         IF info.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
         END

      'LINEAR 2%': BEGIN
      
         scaledImage = BytScl(*info.image, Max=info.maxThresh, Min=info.minThresh, /NAN)
         IF info.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
         END

      'ADAPTIVE EQUALIZATION': BEGIN
         scaledImage = BytScl(Adapt_Hist_Equal(*info.image), Max=info.maxThresh, Min=info.minThresh, /NAN)
         IF info.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
         END

      'EQUALIZATION': BEGIN
         scaledImage = BytScl(Hist_Equal(*info.image), Max=info.maxThresh, Min=info.minThresh, /NAN)
         IF info.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
         END

      'GAMMA': BEGIN
         scaledImage = GmaScl(*info.image, Max=info.maxThresh, Min=info.minThresh, $
                   Gamma=info.gamma, Negative=info.negative)
         RETURN, scaledImage
         END

      'GAUSSIAN': BEGIN
         scaledImage = GaussScl(*info.image, Max=info.maxThresh, Min=info.minThresh, $
                   Sigma=info.sigma, Negative=info.negative)
         RETURN, scaledImage
         END

      'SQUARE ROOT': BEGIN
         scaledImage = BytScl(SQRT(*info.image), Max=info.maxThresh, Min=info.minThresh, /NAN)
         IF info.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
         RETURN, scaledImage
         END

      'LOG': BEGIN
         scaledImage =  LogScl(*info.image, Max=info.maxThresh, Min=info.minThresh, $
                   Mean=info.mean, Exponent=info.exponent, Negative=info.negative)
         RETURN, scaledImage
         END

      'ASINH' :BEGIN
         scaledImage = ASinhScl(*info.image, Max=info.maxThresh, Min=info.minThresh, $
                  BETA=info.beta, Negative=info.negative)
         RETURN, scaledImage
         END

   ENDCASE

   ; Turn warning back on.
   void = Check_Math()
   !Except = thisExcept

END ;--------------------------------------------------------------------



PRO XSTRETCH_IMAGEWINDOWKILLED, imageWindowID

; Turn the Save As, Print, and Image Colors buttons off.

   Widget_Control, imageWindowID, Get_UValue=buttonIDs
   IF Widget_Info(buttonIDs[0], /Valid_ID) THEN BEGIN
      Widget_Control, buttonIDs[0], Sensitive=0
      Widget_Control, buttonIDs[1], Sensitive=0
      Widget_Control, buttonIDs[2], Sensitive=0
   ENDIF

END ;--------------------------------------------------------------------



PRO XSTRETCH_SAVETOMAIN, event

; Handle events from the SAVE to MAIN LEVEL buttons.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy   
   info.event_handler = 'XSTRETCH_SAVETOMAIN'

   Widget_Control, event.id, Get_UValue=buttonValue
   CASE buttonValue OF

      'IMAGE': BEGIN

            varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
               Label='Image Variable Name: ', Cancel=cancelled, XSize=200, Value='stretched_image')

            ; The ROUTINE_NAMES function is not documented in IDL,
            ; so it may not always work. This capability has been
            ; tested in IDL versions 5.3 through 5.6 and found to work.
            ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
            ; set main-level variables. I use the older, undocumented version
            ; to stay compatible with more users.

            IF NOT cancelled THEN BEGIN
               displayImage = XStretch_ScaleImage(info)
               dummy = Routine_Names(varname, displayImage, Store=1)
            ENDIF
            END

      'HISTOGRAM': BEGIN

            varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
               Label='Histogram Variable Name: ', Cancel=cancelled, XSize=200, Value='stretched_histogram')

            ; The ROUTINE_NAMES function is not documented in IDL,
            ; so it may not always work. This capability has been
            ; tested in IDL versions 5.3 through 5.6 and found to work.
            ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
            ; set main-level variables. I use the older, undocumented version
            ; to stay compatible with more users.

            IF NOT cancelled THEN BEGIN

               ; Calculate binsize.
               displayImage = XStretch_ScaleImage(info)
               maxval = Max(displayImage, MIN=minval)
               range = maxval - minval
               IF Size(displayImage, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 256.

               ; Normalized pixel density.
               histdata = Histogram(/NAN, displayImage, Binsize=binsize)

               dummy = Routine_Names(varname, histdata, Store=1)
            ENDIF
            END

      'PARAMETERS': BEGIN

            varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
               Label='Parameter Structure Name: ', Cancel=cancelled, XSize=200, Value='stretched_params')

            ; The ROUTINE_NAMES function is not documented in IDL,
            ; so it may not always work. This capability has been
            ; tested in IDL versions 5.3 through 5.6 and found to work.
            ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
            ; set main-level variables. I use the older, undocumented version
            ; to stay compatible with more users.

            IF NOT cancelled THEN BEGIN

               struct = { minThresh: info.minThresh, $
                          maxThresh: info.maxThresh, $
                          gamma: info.gamma, $
                          beta: info.beta, $
                          mean: info.mean, $
                          exponent: info.exponent, $
                          type: info.type }

               dummy = Routine_Names(varname, struct, Store=1)
            ENDIF
            END

         'EVERYTHING': BEGIN

            varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
               Label='Stretched Structure Name: ', Cancel=cancelled, XSize=200, Value='stretched_struct')

            ; The ROUTINE_NAMES function is not documented in IDL,
            ; so it may not always work. This capability has been
            ; tested in IDL versions 5.3 through 5.6 and found to work.
            ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
            ; set main-level variables. I use the older, undocumented version
            ; to stay compatible with more users.

            IF NOT cancelled THEN BEGIN

               displayImage = XStretch_ScaleImage(info)

               ; Calculate binsize.
               maxval = Max(displayImage, MIN=minval)
               range = maxval - minval
               IF Size(displayImage, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 256.

               ; Normalized pixel density.
               histdata = Histogram(/NAN, displayImage, Binsize=binsize)

               struct = { minThresh: info.minThresh, $
                          maxThresh: info.maxThresh, $
                          gamma: info.gamma, $
                          beta: info.beta, $
                          mean: info.mean, $
                          exponent: info.exponent, $
                          type: info.type, $
                          image: displayImage, $
                          histogram: histdata }

               dummy = Routine_Names(varname, struct, Store=1)
            ENDIF
            END
ENDCASE

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;--------------------------------------------------------------------


PRO XSTRETCH_DRAWLINES, minThresh, maxThresh, info

; Procedure to draw threshold lines and asinh function on histogram plot.

   ; Catch any errors.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, Cancel=1
      void = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Make sure you have the latest values of the parameters
   CASE info.type OF

      'GAMMA': BEGIN
         theValue = Widget_Info(info.gamma_comboID, /Combobox_GetText)
         info.gamma = Double(theValue)
      END

      'GAUSSIAN': BEGIN
         IF N_Elements(info.sigmaObj -> Get_Value()) NE 0 THEN $
            info.sigma = info.sigmaObj -> Get_Value() ELSE info.sigma = 1.0
         info.sigmaObj -> Set_Value, info.sigma
      END

      'LOG': BEGIN
         IF N_Elements(info.param1Obj -> Get_Value()) NE 0 THEN $
            info.mean = info.param1Obj -> Get_Value() ELSE info.mean = 0.5
         info.param1Obj -> Set_Value, info.mean
         IF N_Elements(info.param2Obj -> Get_Value()) NE 0 THEN $
            info.exponent = info.param2Obj -> Get_Value() ELSE info.exponent = 4.0
         info.param2Obj -> Set_Value, info.exponent
      END

      'ASINH': BEGIN
         theValue = Widget_Info(info.asinh_comboID, /Combobox_GetText)
         info.beta = Double(theValue)
      END

      ELSE:

   ENDCASE

   ; Make histogram window active..
   IF (!D.Flags AND 256) NE 0 THEN WSet, info.histo_wid

   ; Draw threshold lines.
   !P = info.pbang
   !X = info.xbang
   !Y = info.ybang
   PlotS, [minThresh, minThresh], [!Y.CRange(0), !Y.CRange(1)], $
      Color=FSC_Color(info.colors[2]), Thick=3
   PlotS, [maxThresh, maxThresh], [!Y.CRange(0), !Y.CRange(1)], $
      Color=FSC_Color(info.colors[3]), Thick=3

   ; Label the lines.
   cmax = Convert_Coord(maxThresh, 0, /Data, /To_Normal)
   cmin = Convert_Coord(minThresh, 0, /Data, /To_Normal)
   minThresh = XStretch_Validate_Threshold(minThresh, info)
   maxThresh = XStretch_Validate_Threshold(maxThresh, info)
   IF info.type EQ 'SQUARE ROOT' THEN BEGIN
      minThresh = Float(minThresh)
      maxThresh = Float(maxThresh)
   ENDIF
   XYOuts, cmin[0], 0.90, /Normal, Number_Formatter(minThresh, Decimals=3), $
      Color=FSC_Color(info.colors[2]), Alignment=1.0, Font=0
   XYOuts, cmax[0], 0.90, /Normal, Number_Formatter(maxThresh, Decimals=3), $
      Color=FSC_Color(info.colors[3]), Alignment=0.0, Font=0

   CASE info.type OF

      'LINEAR': BEGIN
            line = BytScl(Findgen(101))
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'LINEAR 2%': BEGIN
            line = BytScl(Findgen(101))
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'ADAPTIVE EQUALIZATION': BEGIN
            line = BytScl(Findgen(101))
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'EQUALIZATION': BEGIN
            line = BytScl(Findgen(101))
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'LOG': BEGIN
            line = LogScl(Findgen(101), Mean=info.mean, Exponent=info.exponent)
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'GAMMA': BEGIN
            ; Draw the gamma function.
            line = Scale_Vector(Findgen(101), 0.0, 1.0)
            line = Double(line)^info.gamma
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'GAUSSIAN': BEGIN
            ; Draw the gaussian function.
            line = Scale_Vector(Findgen(101), -!PI, !PI)
            line = (1/(2*!PI*info.sigma^2))*EXP(-(line^2/(2*info.sigma^2)))
            line = Scale_Vector(line, 0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'SQUARE ROOT': BEGIN
            line = BytScl(Findgen(101))
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
            x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

      'ASINH': BEGIN
            ; Draw the asinh function.
            line = ASinhScl(Findgen(101), BETA=info.beta)
            line = Scale_Vector(line, 0.0, !Y.CRange[1])
             x = Scale_Vector(Findgen(101), minThresh, maxThresh)
            OPlot, x, line, Color=FSC_Color(info.colors[4]), LineStyle=2, Thick=2
         END

   ENDCASE
END ;--------------------------------------------------------------------



PRO XSTRETCH_NOTIFYOTHERS, info

; This is the procedure that notifies others of an image change.

   ; Catch any error in the histogram display routine.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, Cancel=1
      void = Error_Message()
      RETURN
   ENDIF

    stretchedImage = XStretch_ScaleImage(info)

    ; The structure parameter.
    struct = { image: stretchedImage, $       ; The stretched image.
               r: info.r, $                   ; The R color vector associated with the image
               g: info.g, $                   ; The G color vector associated with the image
               b: info.b, $                   ; The B color vector associated with the image
               event_handler:  info.event_handler, $ ; Where did the event come from?
               type: info.type, $             ; The TYPE of stretch applied to the image.
               minThresh: info.minThresh, $   ; The minimum threshold value.
               maxThresh: info.maxThresh, $   ; The maximum threshold value.
               beta: info.beta, $             ; The current BETA value.
               gamma: info.gamma, $           ; The current GAMMA value.
               mean: info.mean, $             ; The current MEAN value.
               exponent: info.exponent, $     ; The current EXPONENT value.
               sigma: info.sigma }            ; The current SIGMA value.
               
   ; Is there a user value?
   IF N_Elements(*info.uvalue) NE 0 THEN BEGIN
       struct = Create_Struct(struct, "uvalue", *info.uvalue)
   ENDIF

   ; Notify a procedure.
   IF info.notify_pro NE "" THEN Call_Procedure, info.notify_pro, struct

   ; Notify an object.
   IF Obj_Valid(info.notify_obj.object) THEN $
      Call_Method, info.notify_obj.method, info.notify_obj.object, struct

END ;--------------------------------------------------------------------


PRO XSTRETCH_HISTOPLOT, info, $
   WID=wid, $
   MAXVALUE=maxvalue, $
   _Extra=extra

; This is a utility program to draw a histogram plot in a
; display window.

   ; Catch any error in the histogram display routine.
   Catch, theError
   IF theError NE 0 THEN BEGIN
       Catch, Cancel=1
       void = Error_Message()
       RETURN
   ENDIF

   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   ; Proper number formatting.
   format = '(F0.2)'
   IF N_Elements(maxvalue) THEN maxvalue = info.maxvalue
   IF maxvalue LT 0.1 THEN  format = '(F0.3)'
   IF maxvalue LT 0.05 THEN format = '(F0.4)'
   
   ; What kind of data are we working with?
   dataType = Size(*info.image, /TYPE)

   ; Normalized pixel density.
   CASE info.type OF
   
      'ADAPTIVE EQUALIZATION': BEGIN
         goodIndices = Where(Finite(*info.image), NCOMPLEMENT=nanCount, count)
         IF nanCount GT 0 THEN BEGIN
             binsize = (3.5 * StdDev(ADAPT_HIST_EQUAL(*info.image), /NAN))/N_Elements((*info.image)[goodIndices])^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             IF binsize LT 1 THEN binsize = 1
             histdata = Histogram(/NAN, ADAPT_HIST_EQUAL((*info.image)[goodIndices]), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(count)         
         ENDIF ELSE BEGIN
             binsize = (3.5 * StdDev(ADAPT_HIST_EQUAL(*info.image), /NAN))/N_Elements(*info.image)^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             IF binsize LT 1 THEN binsize = 1
             histdata = Histogram(/NAN, ADAPT_HIST_EQUAL(*info.image), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(N_Elements(*info.image))
         ENDELSE
         imageTitle = 'Adapted Equalization Image Value'
         END
   

      'EQUALIZATION': BEGIN
         goodIndices = Where(Finite(*info.image), NCOMPLEMENT=nanCount, count)
         IF nanCount GT 0 THEN BEGIN
             binsize = (3.5 * StdDev(HIST_EQUAL(*info.image), /NAN))/N_Elements((*info.image)[goodIndices])^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             IF binsize LT 1 THEN binsize = 1
             histdata = Histogram(/NAN, Hist_Equal((*info.image)[goodIndices]), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(count)         
         ENDIF ELSE BEGIN
             binsize = (3.5 * StdDev(HIST_EQUAL(*info.image), /NAN))/N_Elements(*info.image)^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             IF binsize LT 1 THEN binsize = 1
             histdata = Histogram(/NAN, Hist_Equal(*info.image), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(N_Elements(*info.image))
         ENDELSE
         imageTitle = 'Equalized Image Value'
         END
      'SQUARE ROOT': BEGIN
         goodIndices = Where(Finite(*info.image), NCOMPLEMENT=nanCount, count)
         IF nanCount GT 0 THEN BEGIN
             binsize = (3.5 * StdDev(SQRT(*info.image, /NAN)))/N_Elements((*info.image)[goodIndices])^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             histdata = Histogram(/NAN, SQRT((*info.image)[goodIndices]), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(count)
         ENDIF ELSE BEGIN
             binsize = (3.5 * StdDev(SQRT(*info.image), /NAN))/N_Elements(*info.image)^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             histdata = Histogram(/NAN, SQRT(*info.image), Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(N_Elements(*info.image))
         ENDELSE
         imageTitle = 'Square Root of Image Value'
         END

      ELSE: BEGIN
         goodIndices = Where(Finite(*info.image), NCOMPLEMENT=nanCount, count)
         IF nanCount GT 0 THEN BEGIN
             binsize = (3.5 * StdDev(*info.image, /NAN))/N_Elements((*info.image)[goodIndices])^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             histdata = Histogram(/NAN, (*info.image)[goodIndices], Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(count)
         ENDIF ELSE BEGIN
             binsize = (3.5 * StdDev(*info.image, /NAN))/N_Elements(*info.image)^(1./3)
             IF (dataType LE 3) OR (dataType GE 12) THEN binsize = Round(binsize) > 1
             binsize = Convert_To_Type(binsize, dataType)
             histdata = Histogram(/NAN, *info.image, Binsize=binsize, $
                OMIN=omin, OMAX=omax)/Float(N_Elements(*info.image))
         ENDELSE
         imageTitle = 'Image Value'
         END
   ENDCASE

   ; Save the current window index.
   cWinID = !D.Window

   ; Calculate the range of the plot output.
   ymin = 0
   ymax = Max(histData) * 1.05
   xmin = Double(omin) - binsize
   xmax = Double(omax) + (binsize * 2)
   
   ; Plot the histogram of the display image.
   IF N_Elements(wid) NE 0 THEN WSet, wid
   Plot, [0,0], [1,1], $             
          Background=FSC_Color(info.colors[0]), $
          Color=FSC_Color(info.colors[1]), $       ; The color of the axes.
          NoData=1, $                              ; Draw the axes only. No data.
          XRange=[xmin, xmax], $                   ; The X data range.          
          XStyle=9, $                              ; Exact axis scaling. No autoscaled axes.
          YMinor=1, $                              ; No minor tick mark on X axis.
          YRange=[ymin, ymax < maxValue], $        ; The Y data range.
          YStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
          YTitle='Relative Frequency', $           ; Y Title
          XTicklen=-0.025, $
          Max_Value=maxValue, $
          YTickformat=format, $
          Position=[0.15, 0.20, 0.85, 0.85], $
          _Extra=extra                      ; Pass any extra PLOT keywords.
             
    Axis, !X.CRange[0], !Y.CRange[1], XAXIS=1, XTickformat='(A1)', XMINOR=1, COLOR=FSC_Color(info.colors[1])
       
    step = (!X.CRange[1] - !X.CRange[0]) / (binsize + 1)
    start = !X.CRange[0] + binsize
    endpt = start + binsize
    FOR j=0,N_Elements(histdata)-1 DO BEGIN
        x = [start, start, endpt, endpt, start]
        y = [0, histdata[j], histdata[j], 0, 0]
        PolyFill, x, y, COLOR=FSC_Color('rose'), NOCLIP=0
        PlotS, x, y, COLOR=FSC_Color(info.colors[5]), NOCLIP=0
        start = start + binsize
        endpt = start + binsize
    ENDFOR
   
   ; Store the plotting system variables for later recall.
   info.pbang = !P
   info.xbang = !X
   info.ybang = !Y
   info.ymin = !Y.CRange[0]
   info.ymax = !Y.CRange[1]
   info.xmin = !X.CRange[0]
   info.xmax = !X.CRange[1]

   ; Validate the threshold.
   info.maxThresh = XStretch_Validate_Threshold(info.maxThresh, info)
   info.minThresh = XStretch_Validate_Threshold(info.minThresh, info)
   info.minThreshObj -> Set_Value,$
      Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(info.minThresh, info), Decimals=3), /FloatValue
   info.maxThreshObj -> Set_Value,$
      Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(info.maxThresh, info), Decimals=3), /FloatValue

   ; Restore previous graphics window.
   IF cWinID GT 0 THEN IF (!D.Flags AND 256) NE 0 THEN WSet, cWinID

END ;--------------------------------------------------------------------------------


PRO XSTRETCH_PARAMETERS, event

; Handle events from the log parameter widgets.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_PARAMETERS'

   ; Make sure you have the latest values for alpha and beta.
   CASE info.type OF

      'LOG': BEGIN
         IF N_Elements(info.param1Obj -> Get_Value()) NE 0 THEN $
            info.mean = info.param1Obj -> Get_Value() ELSE info.mean = 0.5
            info.mean = 0.0 > info.mean < 1.0
         info.param1Obj -> Set_Value, info.mean
         IF N_Elements(info.param2Obj -> Get_Value()) NE 0 THEN $
            info.exponent = info.param2Obj -> Get_Value() ELSE info.exponent = 4.0
         info.param2Obj -> Set_Value, info.exponent
      END

      'ASINH': BEGIN
         theText = Widget_Info(info.asinh_comboID, /Combobox_GetText)
         info.beta = Float(theText)
      END

      'GAMMA': BEGIN
         theText = Widget_Info(info.gamma_comboID, /Combobox_GetText)
         info.gamma = Float(theText)
      END

      'GAUSSIAN': BEGIN
         IF N_Elements(info.sigmaObj -> Get_Value()) NE 0 THEN $
            info.sigma = info.sigmaObj -> Get_Value() ELSE info.sigma = 1.0
         info.sigmaObj -> Set_Value, info.sigma
      END

      ELSE:

   ENDCASE

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
      WSet, info.windex
      WShow, info.windex
      TVLCT, info.r, info.g, info.b
      TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   ; Copy histogram from pixmap.
   WSet, info.histo_wid
   Device, Copy=[0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.pixmap]

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   Widget_Control, event.top, Set_UValue=info, /No_Copy


END ;--------------------------------------------------------------------


PRO XSTRETCH_FLIPIMAGE, event

; Handle events from the "Flip Image" button.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_FLIPIMAGE'
   
   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   ; Switch the value of the button.
   *info.image = Reverse(*info.image, 2)

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
      WSet, info.windex
      WShow, info.windex
      TVLCT, info.r, info.g, info.b
      TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;--------------------------------------------------------------------------------




PRO XSTRETCH_GAMMA, event

; Handler events from the GAMMA pull-down menu.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_GAMMA'
   Widget_Control, event.id, Get_UValue=gamma
   info.gamma = gamma
   Widget_Control, info.cgammaID, Set_Button=0
   info.cgammaID = event.id
   Widget_Control, event.id, Set_Button=1

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
      WSet, info.windex
      WShow, info.windex
      TVLCT, info.r, info.g, info.b
      TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   ; Copy histogram from pixmap.
   WSet, info.histo_wid
   Device, Copy=[0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.pixmap]

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;--------------------------------------------------------------------------------


PRO XSTRETCH_NEGATIVE, event

; Handle events from the "Positive Image/Negative Image" button.

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_NEGATIVE'

   ; Switch the value of the button.
   Widget_Control, event.id, Get_Value=buttonValue
   CASE buttonValue OF
      'Negative Image': Widget_Control, event.id, Set_Value='Positive Image'
      'Positive Image': Widget_Control, event.id, Set_Value='Negative Image'
   ENDCASE
   info.negative = 1-info.negative

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
      WSet, info.windex
      WShow, info.windex
      TVLCT, info.r, info.g, info.b
      TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;--------------------------------------------------------------------------------


PRO XSTRETCH_OPENIMAGE, event

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.ID, Get_Value=buttonValue
   needcolors = 0

   CASE buttonValue OF
      'Raw Binary Image File...': BEGIN

         newImage = GetImage(Group_Leader=event.top, Cancel=cancelled, Catch=0)
         IF cancelled THEN RETURN
         END

      'Formatted Image File...': BEGIN

         newImage = SelectImage(Cancel=cancelled, Palette=palette, Group_Leader=event.top)
         IF cancelled THEN RETURN
         END

   ENDCASE

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_OPENIMAGE'

   ; Hourglass cursor.
   IF Widget_Info(info.image_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   dims = Image_Dimensions(newimage, XSize=xsize, YSize=ysize, TrueIndex=true)
   IF N_Elements(dims) LT 2 OR N_Elements(dims) GT 3 THEN Message, 'Must pass a 2D or 24-bit image'
   IF true NE -1 THEN BEGIN
      CASE true OF
         0: newimage = Transpose(newimage, [1, 2, 0])
         1: newimage = Transpose(newimage, [0, 2, 1])
         ELSE:
      ENDCASE
   ENDIF

   IF N_Elements(palette) NE 0 THEN BEGIN
      info.r = palette[*,0]
      info.g = palette[*,1]
      info.b = palette[*,2]
   ENDIF

   ; Restore the color table vectors.
   TVLCT, info.r, info.g, info.b

   *info.image = newImage

   ; Start with linear stretch on both ends.
   info.maxVal = Max(Double(newImage), Min=minVal)
   info.maxThresh =  Float(info.maxVal)
   info.minVal = minVal
   info.minThresh = Float(info.minVal)
   info.dataType = Size(newImage, /TNAME)

   ; Calculate a value to tell you if you are "close" to a threshold line.
   info.close = 0.05 * (info.maxval-info.minval)

   cWinID = !D.Window
   WSet, info.histo_wid
   XStretch_Histoplot, info, WID=info.histo_wid, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Store the plotting system variables for later recall.
   info.pbang = !P
   info.xbang = !X
   info.ybang = !Y
   info.ymin = !Y.CRange[0]
   info.ymax = !Y.CRange[1]
   info.xmin = !X.CRange[0]
   info.xmax = !X.CRange[1]

   ; Validitate the threshold values. Have to do this AFTER setting xmin/xmax.
   info.minThresh = XSTRETCH_VALIDATE_THRESHOLD(info.minThresh, info)
   info.maxThresh = XSTRETCH_VALIDATE_THRESHOLD(info.maxThresh, info)
   info.minThreshObj -> Set_Value, Number_Formatter(info.minThresh, Decimals=3), /FloatValue
   info.maxThreshObj -> Set_Value, Number_Formatter(info.maxThresh, Decimals=3), /FloatValue


   ; Put the same plot in the pixmap.
   WSet, info.pixmap
   Device, Copy=[0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.histo_wid]

   ; Update the image display by appling the threshold parameters.
   ; Be sure the image draw widget is still around. Make it if it isn't.
   displayImage = XStretch_ScaleImage(info)

   IF NOT info.no_window THEN BEGIN
      IF Widget_Info(info.image_draw, /Valid_ID) THEN BEGIN
         WSet, info.windex
         TVLCT, info.r, info.g, info.b
         imageSize = Size(*info.image)
         xsize = imageSize(1)
         ysize = imageSize(2)
         aspect = Float(xsize)/ysize
         IF xsize GT 512 OR ysize GT 512 THEN BEGIN
            IF xsize NE ysize THEN BEGIN
               aspect = Float(ysize) / xsize
               IF aspect LT 1 THEN BEGIN
                  xsize = 512
                  ysize = (512 * aspect) < 512
               ENDIF ELSE BEGIN
                  ysize = 512
                  xsize = (512 / aspect) < 512
               ENDELSE
            ENDIF ELSE BEGIN
               ysize = 512
               xsize = 512
            ENDELSE
         ENDIF
         Widget_Control, info.image_draw, Draw_XSize=xsize, Draw_YSize=ysize

      ENDIF ELSE BEGIN

         imageSize = Size(*info.image)
         xsize = imageSize(1)
         ysize = imageSize(2)
         aspect = Float(xsize)/ysize
         IF xsize GT 512 OR ysize GT 512 THEN BEGIN
            IF xsize NE ysize THEN BEGIN
               aspect = Float(ysize) / xsize
               IF aspect LT 1 THEN BEGIN
                  xsize = 512
                  ysize = (512 * aspect) < 512
               ENDIF ELSE BEGIN
                  ysize = 512
                  xsize = (512 / aspect) < 512
               ENDELSE
            ENDIF ELSE BEGIN
               ysize = 512
               xsize = 512
            ENDELSE
         ENDIF
         Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=sizes
         xoff = offsets[0] + sizes[0] + 20
         yoff = offsets[1]
         image_tlb = Widget_Base(Row=1, Group=event.top, Title='XStretch Image', $
            XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
         image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize)
         Widget_Control, image_tlb, /Realize, Set_UValue=event.top
         Widget_Control, image_draw, Get_Value=windex
         info.image_draw = image_draw
         info.windex = windex

         XManager, 'xstretch_image', image_tlb, Event_Handler='XStretch_Image_Resize', /No_Block
         Widget_Control, info.saveas, Sensitive=1
         Widget_Control, info.printit, Sensitive=1
         Widget_Control, info.colorsID, Sensitive=1
      ENDELSE
   ENDIF

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
     WSet, info.windex
     WShow, info.windex
     TVLCT, info.r, info.g, info.b
     TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   IF cWinID GT 0 THEN WSet, cWinID
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;--------------------------------------------------------------------------------




PRO XSTRETCH_SAVEAS, event

   ; Errors caused by incorrect IDL versions or missing Coyote files.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Save as various file types.

   Widget_Control, event.top, Get_UValue=info, /No_Copy
  info.event_handler = 'XSTRETCH_SAVEAS'
   Widget_Control, event.id, Get_UValue=saveAsType

   ; Set the current graphics window.

   cWinID = !D.Window
   WSet, info.windex
   TVLCT, info.r, info.g, info.b

   ; What kind of file do you want?

   filename = 'xstretch'
   CASE saveAsType OF

      'JPEG': dummy = TVRead(Filename=filename, /JPEG)
      'PNG': dummy = TVRead(Filename=filename, /PNG)
      'TIFF': dummy = TVRead(Filename=filename, /TIFF)
      'GIF': dummy = TVRead(Filename=filename, /GIF)
      'PICT': dummy = TVRead(Filename=filename, /PICT)
      'BMP': dummy = TVRead(Filename=filename, /BMP)
      'PS': BEGIN

            WSet, info.windex
            keys = PSWindow()
            configureIt = PSConfig(Group_Leader=event.top, Cancel=cancelled, $
               Color=1, Filename='xstretch.ps', _Extra=keys)
            IF NOT cancelled THEN BEGIN
                  thisDevice = !D.Name
                  Set_Plot, 'PS', /Copy
                  Device, _Extra=configureIt
                  displayImage = XStretch_ScaleImage(info)
                  TVImage, displayImage, /NoInterp
                  Device, /Close_File
                  Set_Plot, thisDevice
            ENDIF

            ENDCASE
   ENDCASE

   IF cWinID GT 0 THEN WSet, cWinID

   ; Put the info structure back.

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;-------------------------------------------------------------------



PRO XSTRETCH_SAVEHISTOAS, event

   ; Errors caused by incorrect IDL versions or missing Coyote files.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Save as various file types.

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_SAVEHISTOAS'
   Widget_Control, event.id, Get_UValue=saveAsType

   ; Set the current graphics window.

   cWinID = !D.Window
   WSet, info.histo_wid
   TVLCT, info.r, info.g, info.b

   ; What kind of file do you want?

   filename = 'xstretch_histogram'
   CASE saveAsType OF

      'JPEG': dummy = TVRead(Filename=filename, /JPEG)
      'PNG': dummy = TVRead(Filename=filename, /PNG)
      'TIFF': dummy = TVRead(Filename=filename, /TIFF)
      'GIF': dummy = TVRead(Filename=filename, /GIF)
      'PICT': dummy = TVRead(Filename=filename, /PICT)
      'BMP': dummy = TVRead(Filename=filename, /BMP)
      'PS': BEGIN

            keys = PSWindow()
            configureIt = PSConfig(Group_Leader=event.top, Cancel=cancelled, $
               Color=1, Filename='xstretch_histrogram.ps', _Extra=keys)
            IF NOT cancelled THEN BEGIN
                  thisDevice = !D.Name
                  thisFont=!P.Font
                  !P.Font = 0
                  Set_Plot, 'PS', /Copy
                  Device, _Extra=configureIt
                  XStretch_Histoplot, info, MaxValue=info.maxValue, _Extra=*info.extra
                  XStretch_DrawLines, info.minThresh, info.maxThresh, info
                  Device, /Close_File
                  !P.Font = thisFont
                  Set_Plot, thisDevice
            ENDIF

            ENDCASE



   ENDCASE

   IF cWinID GT 0 THEN WSet, cWinID

   ; Put the info structure back.

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;-------------------------------------------------------------------



PRO XSTRETCH_SETTHRESHOLD, event

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Get the min and max thresholds. Make sure they
   ; don't overlap each other.
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_SETTHRESHOLD'

   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   minThresh = info.minThreshObj -> Get_Value()
   info.minThresh = XSTRETCH_VALIDATE_THRESHOLD(minThresh < (info.maxThresh - (info.range/200.)), info)
   maxThresh = info.maxThreshObj -> Get_Value()
   info.maxThresh = XSTRETCH_VALIDATE_THRESHOLD((info.minThresh + (info.range/200.)) > maxThresh, info)
   info.minThreshObj -> Set_Value, Number_Formatter(info.minThresh, Decimals=3), /FloatValue
   info.maxThreshObj -> Set_Value, Number_Formatter(info.maxThresh, Decimals=3), /FloatValue

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT info.no_window THEN BEGIN
      WSet, info.windex
      WShow, info.windex
      TVLCT, info.r, info.g, info.b
      TVImage, displayImage, /NoInterp
   ENDIF
   XStretch_NotifyOthers, info

   ; Copy histogram from pixmap.
   WSet, info.histo_wid
   Device, Copy=[0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.pixmap]

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;-------------------------------------------------------------------



PRO XSTRETCH_PRINT, event

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Printing and printer setup handled here.
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_PRINT'

   ; Configure printer and print if user OKs.
   result = Dialog_PrinterSetup()
   IF result EQ 1 THEN BEGIN

      ; Are you printing the image or the histogram?
      Widget_Control, event.id, Get_Value=TARGET

      ; I want the output on the page to have the same aspect ratio
      ; as I see in the display window.
      cWinID = !D.Window
      IF TARGET EQ 'IMAGE' THEN BEGIN
         WSet, info.windex
         TVLCT, info.r, info.g, info.b
      ENDIF ELSE BEGIN
         WSet, info.histo_wid

         ; Have to set up drawing colors *before* we go into the PRINTER device.
         FOR j=0,N_Elements(info.colors)-1 DO color = FSC_Color(info.colors[j])
      ENDELSE
      configurePrinter = PSWindow(/Printer)

      ; Print the image.
      thisDevice = !D.Name
      Set_Plot, 'PRINTER', /Copy
      Device, _Extra=configurePrinter
      Widget_Control, Hourglass=1
      IF TARGET EQ 'IMAGE' THEN BEGIN
         displayImage = XStretch_ScaleImage(info)
         TVImage, displayImage, /NoInterp
      ENDIF ELSE BEGIN
            XStretch_Histoplot, info, MaxValue=info.maxValue, _Extra=*info.extra
            XStretch_DrawLines, info.minThresh, info.maxThresh, info
      ENDELSE
      Widget_Control, Hourglass=0
      Device, /Close_Document
      Set_Plot, thisDevice
      IF cWinID GT 0 THEN WSet, cWinID
   ENDIF

   ; Put the info structure back.
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;-------------------------------------------------------------------



PRO XSTRETCH_PROCESS_EVENTS, event

; This event handler ONLY responds to button down events from the
; draw widget. If it gets a DOWN event, it does two things: (1) finds
; out which threshold line is to be moved, and (2) changes the
; event handler for the draw widget to XSTRETCH_MOVELINE.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
   thisEvent = possibleEventTypes[event.type]
   IF thisEvent NE 'DOWN' THEN RETURN

      ; Must be DOWN event to get here, so get info structure.
      Widget_Control, event.top, Get_UValue=info, /No_Copy
      info.event_handler = 'XSTRETCH_PROCESS_EVENTS'

      ; Make sure you have the correct plotting environment.
      current_bangp = !P
      current_bangx = !X
      current_bangy = !Y

      !P = info.pbang
      !X = info.xbang
      !Y = info.ybang

      ; Convert the device coordinates to data coordinates.
      ; Have to have scaling factors for conversion.
      cWinID = !D.Window
      Wset, info.histo_wid
      TVLCT, info.r, info.g, info.b
      coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Data)

      ; Is this event close to a line? If not, ignore it.
      ; Click has to be inside the graph in the y direction.
      IF coords[1] LT info.ymin OR coords[1] GT info.ymax THEN BEGIN
         Widget_Control, event.top, Set_UValue=info, /No_Copy

         ; Put the info structure back into its storage location.; Set things back.
         !P = current_bangp
         !X = current_bangx
         !Y = current_bangy
         IF cWinID GT 0 THEN WSet, cWinID
         RETURN
      ENDIF

      ; How close to either line are you?
       closemin = Abs(info.minthresh - coords[0])
       closemax = Abs(info.maxthresh - coords[0])
       IF closemin LE closemax THEN info.lineby = 'MIN' ELSE info.lineby = 'MAX'

       ; If you are not close to a line, goodbye!
       CASE info.lineby OF
          'MIN': BEGIN
                 IF closemin GT info.close THEN BEGIN
                     Widget_Control, event.top, Set_UValue=info, /No_Copy

                     ; Put the info structure back into its storage location.; Set things back.
                     !P = current_bangp
                     !X = current_bangx
                     !Y = current_bangy
                     IF cWinID GT 0 THEN WSet, cWinID
                     RETURN
                 ENDIF
                 END

          'MAX': BEGIN
                 IF closemax GT info.close THEN BEGIN
                     Widget_Control, event.top, Set_UValue=info, /No_Copy

                     ; Put the info structure back into its storage location.; Set things back.
                     !P = current_bangp
                     !X = current_bangx
                     !Y = current_bangy
                     IF cWinID GT 0 THEN WSet, cWinID
                     RETURN
                 ENDIF
                 END
       ENDCASE

    ; Change the event handler for the draw widget and turn MOTION
    ; events ON.
    Widget_Control, event.id, Event_Pro='XSTRETCH_MOVELINE', Draw_Motion_Events=1

   ; Put the info structure back into its storage location.; Set things back.
   !P = current_bangp
   !X = current_bangx
   !Y = current_bangy
   IF cWinID GT 0 THEN WSet, cWinID

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; of XSTRETCH_PROCESS_EVENTS *********************************************



PRO XSTRETCH_MOVELINE, event

; This event handler continuously draws and erases a threshold line
; until it receives an UP event from the draw widget. Then it turns
; draw widget motion events OFF and changes the event handler for the
; draw widget back to XSTRETCH_PROCESS_EVENTS.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF
   
   ; Get the info structure out of the top-level base.
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   
   info.event_handler = 'XSTRETCH_MOVELINE'

   ; Learn the type of the image data for later testing.
   imageType = Size(*info.image, /TYPE)

   ; Make sure you have the correct plotting environment.
   IF Size(*info.image, /Type) LE 3 THEN format = '(I0)' ELSE format='(F0)'
   current_bangp = !P
   current_bangx = !X
   current_bangy = !Y

   !P = info.pbang
   !X = info.xbang
   !Y = info.ybang

   cWinID = !D.Window

   ; Load image colors.
   TVLCT, info.r, info.g, info.b

   ; What type of an event is this?
   possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
   thisEvent = possibleEventTypes[event.type]

   IF thisEvent EQ 'UP' THEN BEGIN

      ; If this is an UP event, set the draw widget's event handler back
      ; to XSTRETCH_PROCESS_EVENTS, turn MOTION events OFF, and apply the
      ; new threshold parameters to the image.

      ; Erase the last theshold line drawn.
      cWinID = !D.Window
      WSet, info.histo_wid
      TVLCT, info.r, info.g, info.b
      Device, Copy = [0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.pixmap]

      ; Turn motion events off and redirect the events to XSTRETCH_PROCESS_EVENTS.
       Widget_Control, event.id, Draw_Motion_Events=0, $
          Event_Pro='XStretch_Process_Events'

      ; Convert the event device coordinates to data coordinates.
      coord = Convert_Coord(event.x, event.y, /Device, /To_Data)
      coord = XStretch_Validate_Threshold(coord, info)

      ; Make sure the coordinate is between the other line and
      ; still inside the plot.
      range = info.xmax - info.xmin
      closest = range * 0.005
      CASE info.lineby OF
         'MIN': BEGIN
                coord[0] = coord[0] > info.xmin
                coord[0] = coord[0] < (info.maxThresh - closest)
                END
         'MAX': BEGIN
                coord[0] = coord[0] > (info.minThresh + closest)
                coord[0] = coord[0] < info.xmax
                END
      ENDCASE
      
      ; You can get into trouble is this value goes beyond certain bounds and
      ; is of different type from the data.
      IF (imageType EQ 1)  THEN coord[0] = 0 > coord[0] < 255
      IF (imageType EQ 2)  THEN coord[0] = Round(coord[0]) 
      IF (imageType EQ 3)  THEN coord[0] = Round(coord[0]) 
      IF (imageType GE 12) THEN coord[0] = Round(coord[0]) 

      ; Draw both of the threshold lines again.
      CASE info.lineby OF
         'MIN': BEGIN
             XStretch_DrawLines, coord[0], info.maxThresh, info
            info.minThresh = coord[0]
            info.minThreshObj -> Set_Value, $
               Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(info.minThresh, info), Decimals=3), /FloatValue
            END
         'MAX': BEGIN
            XStretch_DrawLines, info.minThresh, coord[0], info
            info.maxThresh = coord[0]
            info.maxThreshObj -> Set_Value, $
               Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(info.maxThresh, info), Decimals=3), /FloatValue
            END
      ENDCASE

   ; Update the image display by appling the threshold parameters.
   ; Be sure the image draw widget is still around. Make it if it isn't.
   displayImage = XStretch_ScaleImage(info)
   XStretch_NotifyOthers, info

   IF NOT info.no_window THEN BEGIN
   IF Widget_Info(info.image_draw, /Valid_ID) THEN BEGIN
         WSet, info.windex
         WShow, info.windex
         TVLCT, info.r, info.g, info.b
         TVImage, displayImage, /NoInterp
      ENDIF ELSE BEGIN

         imageSize = Size(*info.image)
         xsize = imageSize(1)
         ysize = imageSize(2)
         aspect = Float(xsize)/ysize
         IF xsize GT 512 OR ysize GT 512 THEN BEGIN
            IF xsize NE ysize THEN BEGIN
               aspect = Float(ysize) / xsize
               IF aspect LT 1 THEN BEGIN
                  xsize = 512
                  ysize = (512 * aspect) < 512
               ENDIF ELSE BEGIN
                  ysize = 512
                  xsize = (512 / aspect) < 512
               ENDELSE
            ENDIF
         ENDIF
         Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=sizes
         xoff = offsets[0] + sizes[0] + 20
         yoff = offsets[1]
         image_tlb = Widget_Base(Row=1, Group=event.top, Title='XStretch Image', $
            XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
         image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize)
         Widget_Control, image_tlb, /Realize, Set_UValue=event.top
         Widget_Control, image_draw, Get_Value=windex
         info.image_draw = image_draw
         info.windex = windex
         TVImage, displayImage, /NoInterp

         XManager, 'xstretch_image', image_tlb, Event_Handler='XStretch_Image_Resize', /No_Block
         Widget_Control, info.saveas, Sensitive=1
         Widget_Control, info.printit, Sensitive=1
         Widget_Control, info.colorsID, Sensitive=1
      ENDELSE
   ENDIF

      ; Update the pixmap with histogram with no threshold lines.
      XStretch_Histoplot, info, WID=info.pixmap, $
         MaxValue=info.maxValue, _Extra=*info.extra

      ; Put the info structure back into its storage location and then,
      ; out of here!
      Widget_Control, event.top, Set_UValue=info, /No_Copy
       IF cWinID GT 0 THEN WSet, cWinID
      RETURN
   ENDIF ; thisEvent = UP


   ; Most of the action in this event handler occurs here while we are waiting
   ; for an UP event to occur. As long as we don't get it, keep erasing the
   ; old threshold line and drawing a new one.

   ; Get current window and scaling parameters in order.
   WSet, info.histo_wid
   TVLCT, info.r, info.g, info.b
   !P = info.pbang
   !X = info.xbang
   !Y = info.ybang
   coord = Convert_Coord(event.x, event.y, /Device, /To_Data)
   coord[0] = XStretch_Validate_Threshold(coord[0], info)

   ; Draw the "other" line on the pixmap (so you don't have to draw
   ; it all the time).
   WSet, info.pixmap
   CASE info.lineby OF
      'MIN': BEGIN
         cmax = Convert_Coord(info.maxThresh, 0, /Data, /To_Normal)
         PlotS, [info.maxthresh, info.maxthresh],[info.ymin, info.ymax],  $
            Color=FSC_Color(info.colors[3]), Thick=2
         XYOuts, cmax[0], 0.90, /Normal, Number_Formatter(XStretch_Validate_Threshold(info.maxThresh, info), Decimals=3), $
            Color=FSC_Color(info.colors[3]), Alignment=0.0, Font=0
         END
      'MAX': BEGIN
         cmin = Convert_Coord(info.minThresh, 0, /Data, /To_Normal)
         PlotS, [info.minthresh, info.minthresh],[info.ymin, info.ymax],  $
            Color=FSC_Color(info.colors[2]), Thick=2
         XYOuts, cmin[0], 0.90, /Normal, Number_Formatter(XStretch_Validate_Threshold(info.minThresh, info), Decimals=3), $
            Color=FSC_Color(info.colors[2]), Alignment=1.0, Font=0
         END
   ENDCASE

   ; Erase the old threshold line.
   WSet, info.histo_wid
   TVLCT, info.r, info.g, info.b
   Device, Copy = [0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.pixmap]

   ; You can get into trouble is this value goes beyond certain bounds and
   ; is of different type from the data.
   IF (imageType EQ 1)  THEN coord[0] = 0 > coord[0] < 255
   IF (imageType EQ 2)  THEN coord[0] = Round(coord[0]) 
   IF (imageType EQ 3)  THEN coord[0] = Round(coord[0]) 
   IF (imageType GE 12) THEN coord[0] = Round(coord[0]) 

   ; Draw the new line at the new coordinate. Make sure the coordinate
   ; is inside the plot and doesn't go over the other line.
   CASE info.lineby OF
      'MIN': BEGIN
             coord[0] = coord[0] > (info.xmin)
             coord[0] = coord[0] < (info.maxThresh)
             info.minThreshObj -> Set_Value, Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(coord[0], info), Decimals=3), /FloatValue
             END
      'MAX': BEGIN
             coord[0] = coord[0] > (info.minThresh)
             coord[0] = coord[0] < (info.xmax )
             info.maxThreshObj -> Set_Value, Number_Formatter(XSTRETCH_VALIDATE_THRESHOLD(coord[0], info), Decimals=3), /FloatValue
             END
   ENDCASE

   cmax = Convert_Coord(info.maxThresh, 0, /Data, /To_Normal)
   cmin = Convert_Coord(info.minThresh, 0, /Data, /To_Normal)

   theCoord = XStretch_Validate_Threshold(coord[0], info)
   CASE info.lineby OF
      'MIN': BEGIN
         PlotS, [coord[0], coord[0]],[info.ymin, info.ymax], Color=FSC_Color(info.colors[2]), Thick=2
         XYOuts, Float(event.x)/!D.X_Size, 0.90, /Normal, Number_Formatter(thecoord, Decimals=3), $
            Color=FSC_Color(info.colors[2]), Alignment=1.0, Font=0
         END
      'MAX': BEGIN
         PlotS, [coord[0], coord[0]],[info.ymin, info.ymax], Color=FSC_Color(info.colors[3]), Thick=2
         XYOuts, Float(event.x)/!D.X_Size, 0.90, /Normal,  Number_Formatter(thecoord, Decimals=3), $
            Color=FSC_Color(info.colors[3]), Alignment=0.0, Font=0
         END
   ENDCASE

   ; Set things back.
   !P = current_bangp
   !X = current_bangx
   !Y = current_bangy

   IF cWinID GT 0 THEN WSet, cWinID

   ; Put the info structure back into its storage location.
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; of XSTRETCH_MOVELINE **************************************************



PRO XSTRETCH_RESTORE, event

; Restores the original stretch parameters.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Get the info structure out of the top-level base.
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_RESTORE'
   
   ; Get the current mapped base.
   currentMappedBase = info.currentMappedBase
   IF Widget_Info(currentMappedBase, /VALID_ID) THEN Widget_Control, info.currentMappedBase, Map=0
   
   ; Get the originalSetup pointer
   originalSetup = info.originalSetup
   
   info = *info.originalSetup
   info = Create_Struct(info, 'OriginalSetup', originalSetup)
   
    ; Validitate the threshold values. Have to do this AFTER setting xmin/xmax.
   info.minThresh = XSTRETCH_VALIDATE_THRESHOLD(info.minThresh, info)
   info.maxThresh = XSTRETCH_VALIDATE_THRESHOLD(info.maxThresh, info)
   info.minThreshObj -> Set_Value, Number_Formatter(info.minThresh, Decimals=3), /FloatValue
   info.maxThreshObj -> Set_Value, Number_Formatter(info.maxThresh, Decimals=3), /FloatValue
   
   ; Determine scaling type.
   type = info.type
   possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', 'LINEAR 2%', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN']
   IF N_Elements(type) EQ 0 THEN type = 'LINEAR'
   IF Size(type, /TName) EQ 'STRING' THEN BEGIN
      type = StrUpCase(type)
      index = WHERE(possibleTypes EQ type, count)
      IF count EQ 0 THEN Message, 'Cannot find specified stretch type: ' + type
   ENDIF ELSE BEGIN
      type = 0 > Fix(type) < 7
      type = possibleTypes[type]
   ENDELSE
   
   ; Realize the proper controls.
   CASE type OF
      'LOG': BEGIN
         Widget_Control, info.logBaseID, Map=1
         info.currentMappedBase = info.logBaseID
         info.param1Obj -> Set_Value, info.mean
         info.param2Obj -> Set_Value, info.exponent
         END
      'GAMMA': BEGIN
         Widget_Control, info.gammaBaseID, Map=1
         info.currentMappedBase = info.gammaBaseID
         Widget_Control, info.gamma_comboID, Set_Value=StrTrim(info.gamma,2)
         END
      'ASINH': BEGIN
         Widget_Control, info.asinhBaseID, Map=1
         info.currentMappedBase = info.asinhBaseID
         Widget_Control, info.asinh_comboID, Set_Value=StrTrim(info.beta,2)
         END
      'GAUSSIAN': BEGIN
         Widget_Control, info.gaussBaseID, Map=1
         info.currentMappedBase = info.gaussBaseID
         info.sigmaObj -> Set_Value, info.sigma
         END
      ELSE: info.currentMappedBase = -1L
   ENDCASE
   
   types = StrUpCase(['Linear', 'Linear 2%', 'Gamma', 'Log', 'Square Root', 'Asinh', 'Equalization', 'Gaussian'])
   index = Where(types EQ type) ; Necessary for backward compatibility and for my ordering in pull-down.
   info.scaleID -> SetIndex, index[0] > 0
   

   ; Draw histogram.
   XStretch_Histoplot, info, WID=info.histo_wid, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Draw histogram in pixmap
   XStretch_Histoplot, info, WID=info.pixmap, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT Keyword_Set(info.no_window) THEN BEGIN
      WSet, info.windex
      TVLCT, info.r, info.g, info.b
      WShow, info.windex
      TVImage, displayImage, /NoInterp, _Extra=*info.extra
   ENDIF
   
   ; Notify others.
   XStretch_NotifyOthers, info
   
   ; Put the info structure back.
   Widget_Control, event.top, Set_UValue=info, /No_Copy
END


PRO XSTRETCH_STRETCHTYPE, event

; Handles events from the Stretch Type buttons.

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   ; Get the info structure out of the top-level base.
   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_STRETCHTYPE'

   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   ; What is the new type?
   selection = event.self -> GetSelection()

   ; Always start min and max thresholds fresh when moving from one stretch to another.
   IF info.type NE selection THEN BEGIN
      CASE StrUpCase(selection) OF
         'SQUARE ROOT': info.minThresh = MIN(SQRT(*info.image), MAX=maxthresh)
         'EQUALIZATION': info.minThresh = MIN(HIST_EQUAL(*info.image), MAX=maxthresh)
         'LINEAR 2%': BEGIN
            ; Calculate binsize.
            maxr = Max(Double(*info.image), MIN=minr, /NAN)
            range = maxr - minr
            CASE Size(*info.image, /TName) OF
                'BYTE': binsize = 1
                'INT': binsize = 1 > Round(range / 300.)
                'LONG': binsize = 1 > Round(range / 300.)
                'UINT': binsize = 1 > Round(range / 300.)
                'ULONG': binsize = 1 > Round(range / 300.)
                'LONG64': binsize = 1 > Round(range / 300.)
                'ULONG64': binsize = 1 > Round(range / 300.)
                ELSE: binsize = range / 300.
            ENDCASE
            h = Histogram(/NAN, *info.image, BINSIZE=binsize, OMIN=omin, OMAX=omax)
            n = N_Elements(*info.image)
            cumTotal = Total(h, /CUMULATIVE)
            minIndex = Value_Locate(cumTotal, n * 0.02)
            IF minIndex EQ -1 THEN minIndex = 0
            WHILE cumTotal[minIndex] EQ cumTotal[minIndex + 1] DO BEGIN
                 minIndex = minIndex + 1
            ENDWHILE
            info.minThresh = minIndex * binsize + omin

            maxIndex  = Value_Locate(cumTotal, n * 0.98)
            WHILE cumTotal[maxIndex] EQ cumTotal[maxIndex - 1] DO BEGIN
                maxIndex = maxIndex - 1
            ENDWHILE
            maxThresh = maxIndex * binsize + omin
            
          END
         ELSE: info.minThresh = MIN(*info.image, MAX=maxthresh)
      ENDCASE
      info.maxThresh = maxthresh
   ENDIF

   ; Store the selection type.
   info.type = StrUpCase(selection)

   CASE info.type OF

      'LINEAR': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         END

      'LINEAR 2%': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         END

      'ADAPTIVE EQUALIZATION': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         END

      'EQUALIZATION': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         END

      'SQUARE ROOT': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         END

      'GAMMA': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         Widget_Control, info.gammaBaseID, Map=1
         info.currentMappedBase = info.gammaBaseID
         END

      'GAUSSIAN': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         Widget_Control, info.gaussBaseID, Map=1
         info.currentMappedBase = info.gaussBaseID
         END

      'LOG': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
           Widget_Control, info.currentMappedBase, Map=0
         Widget_Control, info.logBaseID, Map=1
         info.currentMappedBase = info.logBaseID
         END

      'ASINH': BEGIN
         IF Widget_Info(info.currentMappedBase, /Valid_ID) THEN $
            Widget_Control, info.currentMappedBase, Map=0
         Widget_Control, info.asinhBaseID, Map=1
         info.currentMappedBase = info.asinhBaseID
         END


   ENDCASE


   ; Draw histogram.
   XStretch_Histoplot, info, WID=info.histo_wid, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Draw histogram in pixmap
   XStretch_Histoplot, info, WID=info.pixmap, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Draw threshold lines.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   ; Display the image after thresholding.
   displayImage = XStretch_ScaleImage(info)
   IF NOT Keyword_Set(info.no_window) THEN BEGIN
      WSet, info.windex
      TVLCT, info.r, info.g, info.b
      WShow, info.windex
      TVImage, displayImage, /NoInterp, _Extra=*info.extra
   ENDIF

   ; Notify others of image change.
   XStretch_NotifyOthers, info

   ; Put the info structure back into the top-level base.
   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;-------------------------------------------------------------------


PRO XSTRETCH_QUIT, event
   Widget_Control, event.top, /Destroy
END ; of XSTRETCH_QUIT ******************************************************



PRO XSTRETCH_COLORS, event

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_COLORS'
   cWinID = !D.Window

   thisEvent = Tag_Names(event, /Structure_Name)
   CASE thisEvent OF

      'WIDGET_BUTTON': BEGIN
          TVLCT, info.r, info.g, info.b
          XColors, Group=event.top, NotifyID=[event.id, event.top], BREWER=info.brewer
          END
      'XCOLORS_LOAD': BEGIN
          Device, Get_Visual_Depth=thisDepth
          IF thisDepth GT 8 THEN BEGIN
             displayImage = XStretch_ScaleImage(info)
             IF info.no_window EQ 0 THEN BEGIN
                info.r = event.r
                info.g = event.g
                info.b = event.b
                TVLCT, info.r, info.g, info.b
                WShow, info.windex
                WSet, info.windex
                TVImage, displayImage, /NoInterp
             ENDIF
             XStretch_NotifyOthers, info

          ENDIF
          END
   ENDCASE
   IF cWinID GT 0 THEN WSet, cWinID

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; of XSTRETCH_COLORS ****************************************************


PRO XSTRETCH_MAXVALUE, event

    ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_MAXVALUE'
   cWinID = !D.Window

   ; Get the new max value.
   Widget_Control, info.pixelDensityID, SET_BUTTON=0
   Widget_Control, event.id, Get_UValue=maxValue, SET_BUTTON=1
   info.maxValue = maxValue
   info.pixelDensityID = event.id

   ; Update the histogram plot.
   XStretch_Histoplot, info, WID=info.histo_wid, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Draw threshold lines on the histogram plot.
   XStretch_DrawLines, info.minthresh, info.maxthresh, info

   ; Update the pixmap with histogram with no threshold lines.
   XStretch_Histoplot, info, WID=info.pixmap, $
      MaxValue=info.maxValue, _Extra=*info.extra

   IF cWinID GT 0 THEN WSet, cWinID

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ;-------------------------------------------------------------------------



PRO XSTRETCH_IMAGE_RESIZE, event

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   cWinID = !D.Window
   Widget_Control, event.top, Get_UValue=histoTLB
   Widget_Control, histoTLB, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_IMAGE_RESIZE'

   ; Hourglass cursor.
   IF Widget_Info(info.histo_draw, /VALID_ID) THEN Widget_Control, /HOURGLASS

   ; I would like to maintain the window aspect ratio the same as the
   ; image aspect ratio.
   dims = Size(*info.image, /Dimensions)
   xsize = dims[0]
   ysize = dims[1]
   maxWindowSize = Max([event.x, event.y])
   IF xsize NE ysize THEN BEGIN
      aspect = Float(ysize) / xsize
      IF aspect LT 1 THEN BEGIN
         xsize = maxWindowSize
         ysize = (maxWindowSize * aspect) < maxWindowSize
      ENDIF ELSE BEGIN
         ysize = maxWindowSize
         xsize = (maxWindowSize / aspect) < maxWindowSize
      ENDELSE
   ENDIF ELSE BEGIN
      ysize = maxWindowSize
      xsize = maxWindowSize
   ENDELSE


   Widget_Control, info.image_draw, Draw_XSize=xsize, Draw_YSize=ysize
   WSet, info.windex
   displayImage = XStretch_ScaleImage(info)
   TVLCT, info.r, info.g, info.b
   TVImage, displayImage, /NoInterp
   XStretch_NotifyOthers, info

   Widget_Control, histoTLB, Set_UValue=info, /No_Copy
   IF cWinID GT 0 THEN WSet, cWinID

END ; of XSTRETCH_IMAGE_RESIZE **********************************************


PRO XSTRETCH_HISTOGRAM_RESIZE, event

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF N_Elements(info) NE 0 THEN Widget_Control, event.top, Set_UValue=info, /No_Copy
      RETURN
   ENDIF

   Widget_Control, event.top, Get_UValue=info, /No_Copy
   info.event_handler = 'XSTRETCH_HISTOGRAM_RESIZE'
   cWinID = !D.Window

   Widget_Control, info.histo_draw, Draw_XSize=event.x > info.min_xsize, Draw_YSize=(event.y - info.pbase_ysize) > 150

   ; Draw the plot.
   XStretch_Histoplot, info, WID=info.histo_wid, $
      MaxValue=info.maxValue, _Extra=*info.extra

   ; Put the same plot in the pixmap.
   WDelete, info.pixmap
   Window, /Free, XSize=event.x > info.min_xsize, YSize=(event.y - info.pbase_ysize) > 150, /Pixmap
   info.pixmap = !D.Window
   info.pix_xsize = event.x > info.min_xsize
   info.pix_ysize = (event.y - info.pbase_ysize) > 150
   Device, Copy=[0, 0, info.pix_xsize, info.pix_ysize, 0, 0, info.histo_wid]

   ; Save the scaling factors for calculating data coordinates.
   info.pbang = !P
   info.xbang = !X
   info.ybang = !Y
   info.ymin = !Y.CRange[0]
   info.ymax = !Y.CRange[1]
   info.xmin = !X.CRange[0]
   info.xmax = !X.CRange[1]

   ; Draw threshold lines on the histogram plot.
   XStretch_DrawLines, info.minThresh, info.maxThresh, info

   IF cWinID GT 0 THEN WSet, cWinID

   Widget_Control, event.top, Set_UValue=info, /No_Copy

END ; of XSTRETCH_COLORS ****************************************************



PRO XSTRETCH_CLEANUP, tlb

   Widget_Control, tlb, Get_UValue=info
   IF N_Elements(info) NE 0 THEN BEGIN
      IF info.newPointer THEN Ptr_Free, info.image
      Ptr_Free, info.uvalue
      Ptr_Free, info.originalSetup
      Ptr_Free, info.extra
      WDelete, info.pixmap
   ENDIF

END ;---------------------------------------------------------------------



PRO XSTRETCH, theImage, $
   Beta=beta, $
   Block=block, $
   Brewer=brewer, $
   Colors=colors, $
   Colortable=ctable, $
   Exponent=exponent, $
   Filename=filename, $
   Gamma=gamma, $
   Group_Leader=group, $
   MinThresh=minThresh, $
   MaxThresh=maxThresh, $
   Max_Value=maxvalue, $
   Mean=mean, $
   Negative=negative, $
   No_Window=no_window, $
   Notify_Obj=notify_obj, $
   Notify_Pro=notify_pro, $
   Sigma=sigma, $
   Title=title, $
   Type=type, $
   UValue=uvalue, $
   XPos=xpos, $
   YPos=ypos, $
   _EXTRA=extra

   ; Error Handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Set up environment.
   Compile_Opt idl2

   ; Initial values of some variables.
   cWinID = !D.Window
   histXsize = 400
   histYsize = 220

   ; Did you specify a filename?
   IF N_Elements(filename) NE 0 AND N_Elements(theImage) EQ 0 THEN BEGIN
      IF filename NE "" THEN BEGIN
         theImage = SelectImage(Filename=filename, Cancel=cancelled, Palette=palette, /Silent)
         IF cancelled THEN RETURN
      ENDIF
   ENDIF

   ; Need an image?
   IF N_Elements(theImage) EQ 0  THEN BEGIN
      file = Filepath(SubDir=['examples', 'data'], 'ctscan.dat')
      theImage = BytArr(256, 256)
      OpenR, lun, file, /GET_LUN
      ReadU, lun, theImage
      Free_LUN, lun
   ENDIF

   ; Is image a pointer? If not, make it one.
   IF Size(theImage, /TName) NE 'POINTER' THEN BEGIN
      image = Ptr_New(theImage)
      newPointer = 1
      datatype = Size(theImage, /TNAME)
   ENDIF ELSE BEGIN
      image = theImage
      newPointer = 0
      datatype = Size(*theImage, /TNAME)
   ENDELSE
   
   ; All kinds of havoc if I work with BYTE data, so do conversion here.
   IF datatype EQ 'BYTE' THEN *image = Temporary(Fix(*image))

   ; Check for underflow of values near 0. Yuck! Necessary with gnarly data.
   curExcept = !Except
   !Except = 0
   i = Where(*image GT -1e-35 AND *image LT 1e-35, count)
   IF count GT 0 THEN (*image)[i] = 0.0
   void = Check_Math()
   !Except = curExcept

   dims = Image_Dimensions(*image, XSize=xsize, YSize=ysize, TrueIndex=true)
   IF N_Elements(dims) LT 2 OR N_Elements(dims) GT 3 THEN Message, 'Must pass a 2D or 24-bit image'
   IF true NE -1 THEN BEGIN
      CASE true OF
         0: *image = Transpose(*image, [1, 2, 0])
         1: *image = Transpose(*image, [0, 2, 1])
         ELSE:
      ENDCASE
   ENDIF

   ; Default values for keywords.
  IF N_Elements(beta) EQ 0 THEN beta = 3 ELSE beta = beta > 0.0
  brewer = Keyword_Set(brewer)
  IF N_Elements(colors) EQ 0 THEN BEGIN
      colors = ['white', 'black', 'firebrick', 'steel blue', 'grn6', 'black']
   ENDIF ELSE BEGIN
      IF N_Elements(colors) NE 6 THEN Message, 'Incorrect number of colors in COLORS vector.'
      defcolors = ['white', 'black', 'firebrick', 'steel blue', 'grn6', 'black']
      i = Where(colors EQ "", count)
      IF count GT 0 THEN colors[i] = defcolors[i]
   ENDELSE
   IF N_Elements(ctable) EQ 0 THEN ctable = 0
   IF N_Elements(exponent) EQ 0 THEN exponent = 4.0
   IF N_Elements(extra) EQ 0 THEN extra = Ptr_New(/Allocate_Heap) ELSE extra = Ptr_New(extra)
   IF N_Elements(gamma) EQ 0 THEN gamma = 1.5
   IF N_Elements(maxvalue) EQ 0 THEN maxvalue = 0.70
   IF N_Elements(mean) EQ 0 THEN mean = 0.5
   mean = 0.0 > mean < 1.0
   IF N_Elements(minThresh) EQ 0 THEN minThresh = Min(*image)
   IF N_Elements(maxThresh) EQ 0 THEN maxThresh = Max(*image)    
   IF N_Elements(notify_pro) EQ 0 THEN notify_pro = ""
   IF N_Elements(negative) EQ 0 THEN negative = 0
   IF N_Elements(notify_obj) EQ 0 THEN notify_obj = {object:Obj_New(), method:""} ELSE BEGIN
      IF Size(notify_obj, /TNAME) NE 'STRUCT' THEN $
         Message, 'NOTIFY_OBJ keyword requires structure variable'
      names = Tag_Names(notify_obj)
      index = Where(names EQ "METHOD", count)
      IF count EQ 0 THEN Message, 'NOTIFY_OBJ structure requires METHOD field.'
      index = Where(names EQ "OBJECT", count)
      IF count EQ 0 THEN Message, 'NOTIFY_OBJ structure requires OBJECT field.'
      IF Obj_Valid(notify_obj.object) EQ 0 THEN Message, 'NOTIFY_OBJ object is invalid.'
   ENDELSE
   no_window = Keyword_Set(no_window)
   IF N_Elements(sigma) EQ 0 THEN sigma = 1.0 ELSE sigma = sigma > 0.1
   IF N_Elements(uvalue) EQ 0 THEN uvalueptr = Ptr_New(/Allocate_Heap) ELSE uvalueptr = Ptr_New(uvalue)
   IF N_Elements(xpos) EQ 0 THEN xpos = 100
   IF N_Elements(ypos) EQ 0 THEN ypos = 100
   IF N_Elements(title) EQ 0 THEN title = 'Drag Vertical Lines to STRETCH Image Contrast'

   ; Determine scaling type.
   possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', 'LINEAR 2%', 'SQUARE ROOT', $
        'EQUALIZATION', 'ADAPTIVE EQUALIZATION', 'GAUSSIAN']
   IF N_Elements(type) EQ 0 THEN type = 'LINEAR'
   IF Size(type, /TName) EQ 'STRING' THEN BEGIN
      type = StrUpCase(type)
      index = WHERE(possibleTypes EQ type, count)
      IF count EQ 0 THEN Message, 'Cannot find specified stretch type: ' + type
   ENDIF ELSE BEGIN
      type = 0 > Fix(type) < 8
      type = possibleTypes[type]
   ENDELSE

   ; Check for availability of GIF files.
   thisVersion = Float(!Version.Release)
   IF ((thisVersion LT 5.4) OR (thisVersion GE 6.2)) THEN haveGif = 1 ELSE haveGIF = 0

   ; Create the histogram widget.
   histo_tlb = Widget_Base(Column=1, Title=title, XPad=0, YPad=0, $
      MBar=menubaseID, TLB_Size_Events=1, XOffset=xpos, YOffset=ypos, Base_Align_Center=1)

   histo_draw = Widget_Draw(histo_tlb, XSize=histXsize, YSize=histYsize, $
      Button_Events=1, Event_Pro='XStretch_Process_Events')
   controlID = Widget_Button(menubaseID, Value='Controls', Event_Pro='XStretch_MaxValue')
   openit = Widget_Button(controlID, Value='Open', /MENU)
   dummy = Widget_Button(openit, Value='Formatted Image File...', Event_Pro='XStretch_OpenImage')
   dummy = Widget_Button(openit, Value='Raw Binary Image File...', Event_Pro='XStretch_OpenImage')
   mainsaveID = Widget_Button(controlID, Value='Save as Main IDL Variable', /Menu, Event_Pro='XSTRETCH_SAVETOMAIN')
   dummy = Widget_Button(mainsaveID, Value='Stretched Image', UValue='IMAGE')
   dummy = Widget_Button(mainsaveID, Value='Stretched Histogram', UValue='HISTOGRAM')
   dummy = Widget_Button(mainsaveID, Value='Current Stretch Parameters (structure)', UValue='PARAMETERS')
   dummy = Widget_Button(mainsaveID, Value='All Stretch Information (structure)', UValue='EVERYTHING')
   saveAs = Widget_Button(controlID, Value='Save Image As', Event_Pro="XStretch_SaveAs", /Menu)
   dummy = Widget_Button(saveAs, Value='BMP File', UValue='BMP')
   dummy = Widget_Button(saveAs, Value='JPEG File', UValue='JPEG')
   dummy = Widget_Button(saveAs, Value='PNG File', UValue='PNG')
   dummy = Widget_Button(saveAs, Value='PICT File', UValue='PICT')
   dummy = Widget_Button(saveAs, Value='TIFF File', UValue='TIFF')
   IF havegif THEN dummy = Widget_Button(saveAs, Value='GIF File', UValue='GIF')
   dummy = Widget_Button(saveAs, Value='PostScript File', UValue='PS')
   saveHistoAs = Widget_Button(controlID, Value='Save Histogram As', Event_Pro="XStretch_SaveHistoAs", /Menu)
   dummy = Widget_Button(saveHistoAs, Value='BMP File', UValue='BMP')
   dummy = Widget_Button(saveHistoAs, Value='JPEG File', UValue='JPEG')
   dummy = Widget_Button(saveHistoAs, Value='PNG File', UValue='PNG')
   dummy = Widget_Button(saveHistoAs, Value='PICT File', UValue='PICT')
   dummy = Widget_Button(saveHistoAs, Value='TIFF File', UValue='TIFF')
   IF havegif THEN dummy = Widget_Button(saveHistoAs, Value='GIF File', UValue='GIF')
   dummy = Widget_Button(saveHistoAs, Value='PostScript File', UValue='PS')
   printit = Widget_Button(controlID, Value='Print...', Event_Pro='XStretch_Print', /MENU)
   dummy = Widget_Button(printit, Value='Image', UValue='IMAGE')
   dummy = Widget_Button(printit, Value='Histogram', UValue='HISTOGRAM')

   maxID = Widget_Button(controlID, Value='Max Pixel Density', /Menu, /Separator)
   dummy = Widget_Button(maxID, Value='0.005', UValue=0.005, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.010', UValue=0.010, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.025', UValue=0.025, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.050', UValue=0.050, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.075', UValue=0.075, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.100', UValue=0.1, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.200', UValue=0.2, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.300', UValue=0.3, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.400', UValue=0.4, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.500', UValue=0.5, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.600', UValue=0.6, /CHECKED_MENU)
   pixelDensityID = Widget_Button(maxID, Value='0.700', UValue=0.7, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.800', UValue=0.8, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='0.900', UValue=0.9, /CHECKED_MENU)
   dummy = Widget_Button(maxID, Value='1.000', UValue=1.0, /CHECKED_MENU)
   Widget_Control, pixelDensityID, Set_Button=1
   colorsID = Widget_Button(controlID, Value='Image Colors...', $
      Event_Pro='XStretch_Colors', /Separator)
   button = Widget_Button(controlID, Value='Negative Image', Dynamic_Resize=1, Event_Pro='XStretch_Negative')
   button = Widget_Button(controlID, Value='Flip Image', Event_Pro='XStretch_FlipImage')
   button = Widget_Button(controlID, Value='Restore Original Stretch', Event_Pro='XStretch_Restore', /Separator)
   quitter = Widget_Button(controlID, Value='Quit', $
      Event_Pro='XStretch_Quit', /Separator)

   ; Stretch TYPE buttons.
   paramBaseID = Widget_Base(histo_tlb, XPAD=0, YPAD=0, Column=1, Base_Align_Left=1)
   rowID = Widget_Base(paramBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10)
   types = StrUpCase(['Linear', 'Linear 2%', 'Gamma', 'Log', 'Square Root', 'Asinh', $
        'Equalization', 'Adaptive Equalization', 'Gaussian'])
   index = Where(types EQ type) ; Necessary for backward compatibility and for my ordering in pull-down.
   scaleID = FSC_Droplist(rowID, Title='Scaling: ', Spaces=1, $
      Value=['Linear', 'Linear 2%', 'Gamma', 'Log', 'Square Root', 'Asinh', $
        'Equalization', 'Adaptive Equalization','Gaussian'], $
      Event_Pro='XStretch_StretchType')
   scaleID -> SetIndex, index[0] > 0

   minthreshObj = FSC_InputField(rowID, Title='Min: ', Value=Float(minThresh), $
      /FloatValue, Event_Pro='XStretch_SetThreshold', UValue='MINTHRESH', $
      XSize=10, /CR_Only)

   maxthreshObj = FSC_InputField(rowID, Title='Max: ', Value=Float(maxThresh) , $
      /FloatValue, Event_Pro='XStretch_SetThreshold', UValue='MAXTHRESH', $
      XSize=10, /CR_Only)

   ; Create the control base widgets.
   controlBaseID = Widget_Base(paramBaseID, XPAD=0, YPAD=0)

         ; LOG controls.
         logBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
         param1Obj = FSC_InputField(logBaseID, Title='Mean: ', Value=mean, /Positive, $
            /FoatValue, Event_Pro='XStretch_Parameters', /CR_Only, XSize=10)
         param2Obj = FSC_InputField(logBaseID, Title='Exponent: ', Value=exponent, /Positive, $
            /FloatValue, Event_Pro='XStretch_Parameters', /CR_Only, XSize=10)

         ; GAMMA controls.
         gammaBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
         label = Widget_Label(gammaBaseID, Value='Gamma: ', /Dynamic_Resize)
         gammas = ['0.040', '0.100', '0.200', '0.400', '0.667', '1.00', '1.500', '2.500', '5.000', '10.00', '25.00']
         gamma_comboID = Widget_Combobox(gammaBaseID, /Editable, Value=gammas, UVALUE='GAMMA', $
                   Event_Pro='XStretch_Parameters')
         index = Where(gammas EQ gamma, count)
         IF count EQ 0 THEN BEGIN
            Widget_Control, gamma_comboID, Combobox_AddItem=StrTrim(gamma,2), ComboBox_Index=0
         ENDIF ELSE Widget_Control, gamma_comboID, Set_Combobox_Select=index[0]

         ; ASINH controls.
         asinhBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
         label = Widget_Label(asinhBaseID, Value='Beta: ', /Dynamic_Resize)
         betas = ['0.0', '0.1', '0.5', '1.0', '3.0', '5.0', '10.0', '50.0', '100.0']
         asinh_comboID = Widget_Combobox(asinhBaseID, /Editable, Value=betas, $
            UVALUE='ASINH', Event_Pro='XStretch_Parameters')
         index = Where(betas EQ beta, count)
         IF count EQ 0 THEN BEGIN
            Widget_Control, asinh_comboID, Combobox_AddItem=Number_Formatter(beta,Decimals=2), ComboBox_Index=0
         ENDIF ELSE Widget_Control, asinh_comboID, Set_Combobox_Select=index[0]

         ; GAUSSIAN controls.
         gaussBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
         sigmaObj = FSC_InputField(gaussBaseID, Title='Sigma: ', Value=sigma, /Positive, $
            /FoatValue, Event_Pro='XStretch_Parameters', /CR_Only, XSize=10)

   ; Realize the proper controls.
   CASE type OF
      'LOG': BEGIN
         Widget_Control, logBaseID, Map=1
         currentMappedBase = logBaseID
         END
      'GAMMA': BEGIN
         Widget_Control, gammaBaseID, Map=1
         currentMappedBase = gammaBaseID
         END
      'ASINH': BEGIN
         Widget_Control, asinhBaseID, Map=1
         currentMappedBase = asinhBaseID
         END
      'GAUSSIAN': BEGIN
         Widget_Control, gaussBaseID, Map=1
         currentMappedBase = gaussBaseID
         END      
      ELSE: currentMappedBase = -1L
   ENDCASE
   Widget_Control, histo_tlb, /Realize

   ; Create a pixmap window for moving and erasing the histogram
   ; threshold bars.
   Window, XSize=histXsize, YSize=histYsize, /Free, /Pixmap
   pixmap = !D.Window

   ; Create an image window for displaying the image.
   IF NOT Keyword_Set(no_window) THEN BEGIN

      Widget_Control, histo_tlb, TLB_Get_Offset=offsets, TLB_Get_Size=sizes
      xoff = offsets[0] + sizes[0] + 20
      yoff = offsets[1]
      aspect = Float(xsize)/ysize
      IF xsize GT 512 OR ysize GT 512 THEN BEGIN
         IF xsize NE ysize THEN BEGIN
            aspect = Float(ysize) / xsize
            IF aspect LT 1 THEN BEGIN
               xsize = 512
               ysize = (512 * aspect) < 512
            ENDIF ELSE BEGIN
               ysize = 512
               xsize = (512 / aspect) < 512
            ENDELSE
         ENDIF ELSE BEGIN
            ysize = 512
            xsize = 512
         ENDELSE
      ENDIF
      image_tlb = Widget_Base(Row=1, Group_Leader=histo_tlb, Title='XStretch Image', $
         XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
      image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize, $
         Kill_Notify='XStretch_ImageWindowKilled', UValue=[saveAs, printit, colorsID])
      Widget_Control, image_tlb, /Realize

      ; Get window index numbers for the draw widgets.
      Widget_Control, image_draw, Get_Value=windex

     ; If this window closes, the whole application exits.
     Widget_Control, histo_tlb, Group_Leader=image_tlb

   ENDIF ELSE BEGIN

      ; Must have values for info structure.
      image_tlb = -1L
      image_draw = -1L
      windex = -1L

   ENDELSE

   ; Need identifier of histogram window.
   Widget_Control, histo_draw, Get_Value=histo_wid

   ; Load the color table.
   IF N_Elements(palette) EQ 0 THEN $
      CTLoad, 0 > ctable < 40, Brewer=brewer ELSE $
      TVLCT, palette
   TVLCT, r, g, b, /Get

   ; Start with no stretch on both ends.
   maxVal = Max(Double(*image)) > maxthresh
   minVal = Min(Double(*image)) < minthresh
   range = maxVal - minVal

   ; Calculate a value to tell you if you are "close" to a threshold line.
   close = 0.05 * (maxval-minval)

   ; How big is the parameter base.
   geo = Widget_Info(paramBaseID, /Geometry)
   pbase_ysize = geo.scr_ysize + 2
   min_xsize = geo.scr_xsize

   ; Make an info structure with all info to run the program.
   info = {image:image, $                   ; A pointer to the image data
           minThresh:minThresh, $           ; The minimum threshold
           maxThresh:maxThresh, $           ; The maximum threshold
           maxValue:maxValue, $             ; The MAX_VALUE for the Histogram Plot.
           colors: colors, $                ; The histogram plot drawing colors.
           histo_wid:histo_wid, $           ; The histogram window index number
           histo_draw:histo_draw, $         ; The histogram draw widget ID.
           image_draw:image_draw, $         ; The image draw widget ID.
           windex:windex, $                 ; The image window index
           ymin:!Y.CRange[0], $                     ; The ymin in data coordinates
           ymax:!Y.CRange[1], $                     ; The ymax in data coordinates
           xmin:!X.CRange[0], $                     ; The xmin in data coordinates
           xmax:!X.CRange[1], $                     ; The xmax in data coordinates
           r:r, $                           ; The R color vector for the image
           g:g, $                           ; The G color vector for the image
           b:b, $                           ; The B color vector for the image
           pbang:!P, $                   ; The !P system variable.
           xbang:!X, $                   ; The !X system variable.
           ybang:!Y, $                   ; The !Y system variable.
           lineby:'MIN', $                  ; The line you are close to.
           linex:minThresh, $               ; The x coordinate of line (data coords).
           pixmap:pixmap, $                 ; The pixmap window index
           minval:minval, $                 ; The minimum intensity value of the data
           maxval:maxval, $                 ; The maximum intensity value of the data
           notify_pro:notify_pro, $         ; The name of a procedure to notify when the image is stretched.
           notify_obj:notify_obj, $         ; The object reference and method to notify when image is stretched.
           no_window:no_window, $           ; A flag that, if set, means no image window.
           extra:extra, $                   ; The extra keywords for the Plot command.
           pix_xsize:histXsize, $           ; The X size of the pixmap.
           pix_ysize:histYsize, $           ; The Y size of the pixmap.
           pixelDensityID:pixelDensityID, $ ; The ID of the current pixel density button.
           newPointer:newPointer, $         ; A flag that indicates if we made a pointer or not.
           saveAs:saveAs, $                 ; The SaveAs button widget identifier.
           printIt:printIt, $               ; The Print button widget identifier.
           gamma: gamma, $                  ; The gamma value.
           beta: beta, $                    ; The "softenting parameter" for ASINH scaling.
           logBaseID: logBaseID, $          ; The base widget ID of the LOG parameters.
           gammaBaseID: gammaBaseID, $      ; The base widget ID of the GAMMA parameters.
           asinhBaseID: asinhBaseID, $      ; The base widget ID of the ASINH parameters.
           gaussBaseID: gaussBaseID, $      ; The base widget ID of the GAUSSIAN parameters.
           currentMappedBase: currentMappedBase, $ The current base mapped into the control base.
           exponent: exponent, $            ; The exponent value.
           mean: mean, $                    ; The mean value.
           param1Obj: param1Obj, $          ; The first parameter widget.
           param2Obj: param2Obj, $          ; The second parameter widget.
           gamma_comboID: gamma_comboID, $  ; The gamma control combobox widget ID.
           asinh_comboID: asinh_comboID, $  ; The asinh control combobox widget ID.
           pbase_ysize: pbase_ysize, $      ; The y size of the parameter base.
           min_xsize: min_xsize, $          ; The minimum X size for the draw widget.
           negative: negative, $            ; Want a negative image.
           type: type, $                    ; The type of scaling requested.
           datatype: datatype, $            ; The data type of the input image.
           minthreshObj: minthreshObj, $    ; The minThresh object widget.
           maxthreshObj: maxthreshObj, $    ; The maxThresh object widget.
           sigmaObj: sigmaObj, $            ; The sigma object widget.
           sigma: sigma, $                  ; The sigma value for Gaussian stretch.
           event_handler: "", $             ; The name of the event handler processing the event
           colorsID:colorsID, $             ; The Image Colors button widget identifier.
           range: range, $                  ; The image data range.
           scaleID:scaleID, $               ; The droplist that display scale type.
           brewer:brewer, $                 ; A flag that indicates the Brewer color tables should be used.
           uvalue:uvalueptr, $              ; A pointer to the user value (may be undefined variable).
           close:close}                     ; A value to indicate closeness to line

   ; Scale the image. Special processing for linear 2%.
   CASE type OF 
      'LINEAR 2%': BEGIN
   
            ; Calculate binsize.
            maxr = Max(Double(*info.image), MIN=minr, /NAN)
            range = maxr - minr
            CASE Size(*info.image, /TName) OF
                'BYTE': binsize = 1
                'INT': binsize = 1 > Round(range / 300.)
                'LONG': binsize = 1 > Round(range / 300.)
                'UINT': binsize = 1 > Round(range / 300.)
                'ULONG': binsize = 1 > Round(range / 300.)
                'LONG64': binsize = 1 > Round(range / 300.)
                'ULONG64': binsize = 1 > Round(range / 300.)
                ELSE: binsize = range / 300.
            ENDCASE
            h = Histogram(/NAN, *info.image, BINSIZE=binsize, OMIN=omin, OMAX=omax)
            n = N_Elements(*info.image)
            cumTotal = Total(h, /CUMULATIVE)
            minIndex = Value_Locate(cumTotal, n * 0.02)
            IF minIndex EQ -1 THEN minIndex = 0
            WHILE cumTotal[minIndex] EQ cumTotal[minIndex + 1] DO BEGIN
                 minIndex = minIndex + 1
            ENDWHILE
            info.minThresh = minIndex * binsize + omin

            maxIndex  = Value_Locate(cumTotal, n * 0.98)
            WHILE cumTotal[maxIndex] EQ cumTotal[maxIndex - 1] DO BEGIN
                maxIndex = maxIndex - 1
            ENDWHILE
            info.maxThresh = maxIndex * binsize + omin
   
            END
        
        'SQUARE ROOT': BEGIN
            min = Min(SQRT(*info.image), MAX=max)
            info.minThresh = min
            info.maxThresh = max
            END
            
        ELSE: 
        
   ENDCASE
   displayImage = XStretch_ScaleImage(info)
   minThresh = info.minThresh
   maxThresh = info.maxThresh
   
   ; Draw the histogram. Keep this in FRONT of storing plotting system variables!
   WSet, histo_wid
   XStretch_Histoplot, info, WID=histo_wid, $
      MaxValue=maxValue, _Extra=*extra

   ; Put the same plot in the pixmap.
   WSet, pixmap
   Device, Copy=[0, 0, histXsize, histYsize, 0, 0, histo_wid]

   ; Store the plotting system variables for later recall.
   info.pbang = !P
   info.xbang = !X
   info.ybang = !Y
   info.ymin = !Y.CRange[0]
   info.ymax = !Y.CRange[1]
   info.xmin = !X.CRange[0]
   info.xmax = !X.CRange[1]

   ; Display the image.
   IF NOT Keyword_Set(no_window) THEN BEGIN
      WSet, windex
      CTLoad, ctable, Brewer=brewer
      WShow, windex
      TVImage, displayImage, /NoInterp, _Extra=*extra
   ENDIF

   ; Set proper threshold values.
   minThreshObj -> Set_Value, XSTRETCH_VALIDATE_THRESHOLD(minThresh, info), /FloatValue
   maxThreshObj -> Set_Value, XSTRETCH_VALIDATE_THRESHOLD(maxThresh, info), /FloatValue

   ; Draw threshold lines.
   XStretch_DrawLines, minThresh, maxThresh, info

   ; Notify others of image change.
   info.event_handler = 'XSTRETCH_STRETCHTYPE'
   XStretch_NotifyOthers, info

   ; Save a pointer to the current set up, so you can restore it, if necessary.
   originalSetup = Ptr_New(info)
   info = Create_Struct(info, 'originalSetup', originalSetup)

   ; Save the info structure and bring the histogram window forward with SHOW.
   Widget_Control, histo_tlb, Set_UValue=info, /No_Copy, /Show
   IF cWinID GE 0 THEN WSet, cWinID
   

   IF NOT no_window THEN BEGIN
      Widget_Control, image_tlb, Set_UValue=histo_tlb
      XManager, 'xstretch_image', image_tlb, Event_Handler='XStretch_Image_Resize', $
         No_Block=1
   ENDIF
   XManager, 'xstretch', histo_tlb, Group=group, No_Block=1-Keyword_Set(block), $
      Event_Handler='XStretch_Histogram_Resize', Cleanup='XStretch_Cleanup'

END
