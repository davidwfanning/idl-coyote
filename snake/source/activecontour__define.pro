; docformat = 'rst'
;
; NAME:
;  ActiveContour__Define
;
; PURPOSE:
;  This program allows the user to experiment with a Gradient Vector Flow
;  active contour algorithm, as described by Chenyang Xu and Jerry L. Prince
;  in "Snakes, Shapes, and Gradient Vector Flow" in the March 1998 IEEE
;  Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's
;  web page: http://iacl.ece.jhu.edu/projects/gvf/.
;
;  Active contours are also described as "snakes", because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
;
;  The Coyote Library (http://www.idlcoyote.com/documents/programs.php) is required 
;  to run this program.
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
;+--------------------------------------------------------------------------
;  This program allows the user to experiment with a Gradient Vector Flow
;  active contour algorithm, as described by Chenyang Xu and Jerry L. Prince
;  in "Snakes, Shapes, and Gradient Vector Flow" in the March 1998 IEEE
;  Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's
;  `web page <http://iacl.ece.jhu.edu/projects/gvf/>`.
;
;  Active contours are also described as "snakes", because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
;
;  The `Coyote Library <http://www.idlcoyote.com/documents/programs.php>` is required 
;  to run this program.
;
;  This program set can be purchased from the 
;  `Coyote Store <http://www.idlcoyote.com/store/index.php?act=viewCat&catId=3>`.
;
; :Categories:
;    Graphics, Analysis
;    
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive 
;       Fort Collins, CO 80526 USA 
;       Phone: 970-221-0438 
;       E-mail: david@idlcoyote.com 
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;    Modification History::
;       Written by David W. Fanning, 1 December 2003.
;       Removed the Coyote Library from the ActiveContour distribution. 15 June 2005.
;       Extensive updates for the latest Coyote Library routines and to make the 
;          program easier to use. 24 Oct 2011. DWF.
;       When an initial ROI was being specified (via X and Y parameters to INIT method),
;          the ROI was not being drawn properly. Fixed. Now assume initial ROI values are
;          in image pixel coordinates. 13 October 2013. DWF.
;        
; :Copyright:
;    Copyright (c) 2003-2013, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
;
;+
; This procedure takes a closed curve in X and Y and re-samples it in equal arc 
; lengths. The purpose is to have a curve with approximately equally-spaced
; intervals along its length. This is essential for getting a good fit with
; the ActiveContour algorithm.
; 
; :Params:
;    x_in: in, required, type=vector
;       The X locations of the input curve.
;    y_in: in, required, type=vector
;       The Y locations of the input curve.
;    x_out: out, optional, type=vector
;       The X locations of the output curve.
;    y_out: out, optional, type=vector
;       The Y locations of the output curve.
;
; :Keywords:
;    phase: in, optional, type=float, default=0.0
;       A parameter that takes the phase of the curve into account.
;       Its value should be between 0.0 and 1.0 and is confined to
;       be so.
;    points: in, optional, type=integer, default=50
;       The number of points in the output curve.
;-      
PRO ActiveContour::ArcSample, x_in, y_in, x_out, y_out, $
   PHASE=phase, $
   POINTS=points 

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Check parameters.
   IF N_Elements(points) EQ 0 THEN points = 50
   IF N_Elements(phase) EQ 0 THEN phase = 0.0 ELSE phase = 0.0 > phase < 1.0

   ; Make sure the curve is closed (first point same as last point).
   npts = N_Elements(x_in)
   IF (x_in[0] NE x_in[npts-1]) OR (y_in[0] NE y_in[npts-1]) THEN BEGIN
      x_in = [x_in, x_in[0]]
      y_in = [y_in, y_in[0]]
      npts = npts + 1
   ENDIF

   ; Interpolate very finely.
   nc = (npts -1) * 100
   t = DIndgen(npts)
   t1 = DIndgen(nc + 1) / 100
   x1 = Spl_Interp(t, x_in, Spl_Init(t, x_in), t1)
   y1 = Spl_Interp(t, y_in, Spl_Init(t, y_in), t1)

   avgslopex = (x1[1]-x1[0] + x1[nc]-x1[nc-1]) / (t1[1]-t1[0]) / 2
   avgslopey = (y1[1]-y1[0] + y1[nc]-y1[nc-1]) / (t1[1]-t1[0]) / 2


   dx1 = Spl_Init(t, x_in, yp0=avgslopex, ypn_1=avgslopex)
   dy1 = Spl_Init(t, y_in, yp0=avgslopey, ypn_1=avgslopey)
   x1 = Spl_Interp(t, x_in, dx1, t1)
   y1 = Spl_Interp(t, y_in, dy1, t1)

  ; Compute cumulative path length.
  ds = SQRT((x1[1:*]-x1)^2 + (y1[1:*]-y1)^2)
  ss = [0d, Total(ds, /Cumulative)]

  ; Invert this curve, solve for TX, which should be evenly sampled in
  ; the arc length space.
  sx = DIndgen(points) * Max(ss)/points + phase
  tx = Spl_Interp(ss, t1, Spl_Init(ss, t1), sx)

  ; Reinterpolate the original points using the new values of TX.
  x_out = Spl_Interp(t, x_in, dx1, tx)
  y_out = Spl_Interp(t, y_in, dy1, tx)

  x_out = [x_out, x_out[0]]
  y_out = [y_out, y_out[0]]

END


;+
; This function runs the GVF Active Contour code to completion.
; 
; :Returns:
;    Returns a structure containing the active contour results.
;    A value is only returned if the program is not running in demo mode::
;    
;    retValue = {NPTS:N_Elements(x), X:x, Y:y, $
;       PERIMETER:perimeter, AREA:area, $
;       VALUES:(*self.original)[Round(x),Round(y)]}
;
;          
; 
; :Keywords:
;    cancel: out, optional, type=boolean, default=0
;       On exit, this value will be set to 1 if the user cancelled the
;       active contour operation.
;    noblink: in, optional, type=boolean, default=0
;       Normally, the final contour is blinked with the starting
;       contour as an indication the active contouring is completed.
;       Setting this keyword will inhibit this blinking operation.
;-      
FUNCTION ActiveContour::ApplyGVFSnake, Cancel=cancel, NOBLINK=noblink

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF Obj_Valid(progressbar) THEN progressbar -> Destroy
      void = Check_Math()
      !Except = thisExcept
      RETURN, -1
   ENDIF

   ; Turn off exception handling to avoid underflow errors. Aaauughhh!
   thisExcept = !Except
   !Except = 0

   ; Make sure you have the current information from the control panel.
   self -> UpdateParameters

   ; Depending upon 24-bit color.
   Device, Decomposed=1
   
   ; Update the image. This is necessary to establish a data coordinate system.
   self -> UpdateImage
   WSet, self.wid
   cgPlotS, *self.xsnake_f, *self.ysnake_f, /Device, Color=self.color, Thick=2

   ; Anticipate no cancelling.
   cancel = 0

   ; Currently the points are in self.xsnake_f and self.ysnake_f. Move these to self.xsnake and self.ysnake,
   ; so you can perform the analysis over again from starting parameters.
   IF Ptr_Valid(self.xsnake) THEN BEGIN
      *self.xsnake = *self.xsnake_f
   ENDIF ELSE BEGIN
      self.xsnake = Ptr_New(*self.xsnake_f)
   ENDELSE
   Ptr_Free, self.xsnake_f

   IF Ptr_Valid(self.ysnake) THEN BEGIN
      *self.ysnake = *self.ysnake_f
   ENDIF ELSE BEGIN
      self.ysnake = Ptr_New(*self.ysnake_f)
   ENDELSE
   Ptr_Free, self.ysnake_f
   
   ; Do you have an edgemap?
   IF Ptr_Valid(self.edgemap) EQ 0 THEN self -> Edgemap

   ; IF the GVF u and v fields have already been calculated, no
   ; need to do it again here.
   IF Ptr_Valid(self.u) THEN BEGIN
      u = *self.u
      v = *self.v
   ENDIF ELSE BEGIN

      ;Initialize the GVF to the gradient
      self.u = Ptr_New(*self.fx)
      self.v = Ptr_New(*self.fy)

      ; Calculate the dot product of the gradient field.
      magSqr = (*self.fx)^2 + (*self.fy)^2

      ;Iteratively solve for the GVF u,v.
      progressbar = Obj_New('cgProgressbar', Title='GVF Iterations', /Cancel)
      progressbar -> Start
      FOR j=1,self.gvf_iterations DO BEGIN
         IF progressBar -> CheckCancel() THEN BEGIN
            ok = Dialog_Message('Operation cancelled.')
            cancel = 1
            Ptr_Free, self.xsnake
            Ptr_Free, self.ysnake
            self.npts = 0
            WSet, self.wid
            Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
            progressbar -> Destroy
            IF Widget_Info(self.applyID, /Valid_ID) THEN $
               Widget_Control, self.applyID, Sensitive=0
               void = Check_Math()
               !Except = thisExcept
            RETURN, -1

         ENDIF

         progressbar -> Update, Float(j)/self.iterations * 100.0
         u_lap = self -> Laplacian(*self.u)
         v_lap = self -> Laplacian(*self.v)
         *self.u = *self.u + (self.mu * 4 * u_lap) - (magSqr * (*self.u-*self.fx))
         *self.v = *self.v + (self.mu * 4 * v_lap) - (magSqr * (*self.v-*self.fy))
      ENDFOR
      progressbar -> Destroy

   ENDELSE

   ; Currently the initial snake points are in window device coordinates. They need
   ; to be converted to image device (or pixel) coordinates.
   xwinpixels = *self.xsnake
   ywinpixels = *self.ysnake
   normCoord = Convert_Coord(xwinpixels, ywinpixels, /TO_NORMAL, /DEVICE)
   xNorm = Transpose(normCoord[0,*])
   yNorm = Transpose(normCoord[1,*])
   
   ; The normalized coordinates describe the data range, which
   ; goes from 0 to the size of the image.
   s = Size(*self.original, /DIMENSIONS)
   xvec = Scale_Vector(Dindgen(s[0]), 0.0d, 1.0d)
   yvec = Scale_Vector(Dindgen(s[1]), 0.0d, 1.0d)
   xpixCoord = Value_Locate(xvec, xNorm)
   ypixCoord = Value_Locate(yvec, yNorm)

   ; Get the initial snake points. Change to 150 points on equally spaced arcs.
   self -> ArcSample, xpixCoord, ypixCoord, x, y, Points=150
   x = [x, x[0]]
   y = [y, y[0]]

   ; Display the points.
   WSet, self.wid
   WShow, self.wid
   Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
   cgPlotS, x, y, /Data, Color='green', Thick=2
   x_orig = x
   y_orig = y

   ; Do the iterations.
   progressbar = Obj_New('cgProgressbar', Title='Snake Iterations', /Cancel)
   progressbar -> Start
   FOR j=1,self.iterations DO BEGIN
      IF progressBar -> CheckCancel() THEN BEGIN

            ok = Dialog_Message('Operation cancelled.')
            cancel = 1
            self.xsnake_f = Ptr_New(*self.xsnake)
            self.ysnake_f = Ptr_New(*self.ysnake)
            Ptr_Free, self.xsnake
            Ptr_Free, self.ysnake
            self.npts = 0
            WSet, self.wid
            Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
            cgPlotS, *self.xsnake_f, *self.ysnake_f, /Device, Color=self.color, Thick=2
            progressbar -> Destroy
            void = Check_Math()
            !Except = thisExcept
            RETURN, -1

      ENDIF
      progressbar -> Update, Float(j)/self.iterations * 100.0

      ; Deform the snake.
      p = self -> SnakeDeform(x, y)

      ; Remove or add points, as necessary.
      p = self -> SnakeInterpolate(p)

      ; Close the contour.
      x = [p[*,0], p[0,0]]
      y = [p[*,1], p[0,1]]

      ; Plot it.
      WSet, self.wid
      Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
      cgPlotS, x_orig, y_orig, /Data, Color='Green', Thick=2
      cgPlotS, x, y, /Data, Color=self.color, Thick=2
   ENDFOR
   progressbar -> Destroy

   ; Set up for doing this again.
   IF Ptr_Valid(self.xsnake_f) THEN *self.xsnake_f = *self.xsnake ELSE self.xsnake_f = Ptr_New(*self.xsnake)
   IF Ptr_Valid(self.ysnake_f) THEN *self.ysnake_f = *self.ysnake ELSE self.ysnake_f = Ptr_New(*self.ysnake)
   Ptr_Free, self.xsnake
   Ptr_Free, self.ysnake

   ; Restore exception handling.
   void = Check_Math()
   !Except = thisExcept
   
   ; Calculate the perimeter and area of the ROI.
   roi = Obj_New('IDLanROI', x, y, TYPE=2)
   ok = roi -> ComputeGeometry(Perimeter=perimeter, Area=area, Spatial_Scale=[self.spatial_scale, 1.0])

   IF ok AND Widget_Info(self.control_TLB, /Valid_ID) THEN BEGIN
      Widget_Control, self.perimeterID, Set_Value=String(perimeter, Format='(F10.2)')
      Widget_Control, self.areaID, Set_Value=String(area, Format='(F10.2)')
   ENDIF

   ; Make sure there is only one ROI object defined at any one time.
   IF Obj_Valid(self.roi) THEN Obj_Destroy, self.roi
   self.roi = roi

   ; Make the SAVE ROI button sensitive.
   IF Widget_Info(self.control_TLB, /Valid_ID) THEN Widget_Control, self.roiSaveID, Sensitive=1

   ; Blink the original four times, then draw the final contour.
   IF 1 - Keyword_Set(noblink) THEN BEGIN
      FOR j=0,3 DO BEGIN
         WSet, self.wid
         Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
         cgPlotS, x_orig, y_orig, /Data, Color='Green', Thick=2
         cgPlotS, x, y, /Data, Color=self.color, Thick=2
         Wait, 0.2
         Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
         cgPlotS, x, y, /Data, Color=self.color, Thick=2
         Wait, 0.2
      ENDFOR
   ENDIF

   ; Final contour.
   Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
   cgPlotS, x, y, /Data, Color=self.color, Thick=2

   ; Return the final contour information.
   retValue = {NPTS:N_Elements(x), X:x, Y:y, $
       PERIMETER:perimeter, AREA:area, $
       VALUES:(*self.original)[Round(x),Round(y)]}
       
   IF self.demo_version THEN BEGIN
       RETURN, 0
   ENDIF ELSE BEGIN
        ; Send an ROI_COMPLETED event.
        self -> SendEvent, TYPE="ROI_COMPLETED", DATA=retValue
        RETURN, retValue
   ENDELSE
END



;+
; This procedure clears the display window and gets it ready for drawing a contour.
;-      
PRO ActiveContour::ClearWindow

   COMPILE_OPT idl2, Hidden

    WSet, self.WID
    WShow, self.WID
    Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]

    ; Set up for drawing another contour, if desired.
    self.npts = 0
    IF Widget_Info(self.applyID, /Valid_ID) THEN BEGIN
       Widget_Control, self.applyID, Sensitive=0
       Widget_Control, self.perimeterID, Set_Value=''
       Widget_Control, self.areaID, Set_Value=''
       Widget_Control, self.valueID, Set_Value=''
   ENDIF

END


;+
; This procedure receives stretch information from cgStretch when the image changes
; or is stretched. The image and stretch parameters are updated in the object.
; 
; :Params:
;     info: in, required, type=structure
;         The info structure returned from cgStretch when the image is stretched there.
;-      
PRO ActiveContour::Contrast_Stretch, info

   COMPILE_OPT idl2, Hidden

    *self.image = info.image
    self.max_v = info.maxthresh
    self.min_v = info.minThresh

    self -> UpdateImage
END


;+
; This procedure displays the active contour control panel.
;-      
PRO ActiveContour::Controls

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(tlb) NE 0 THEN Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Only one version of the controls at a time.
   IF XRegistered('activecontour_controls' + Get_Object_ID(self)) GT 0 THEN RETURN

   ; Add a group leader for the controls..

   parent = self.drawID
   WHILE Widget_Info(parent, /Parent) NE 0 DO $
      parent = Widget_Info(parent, /Parent)

   IF self.tlb_xoffset EQ -1 THEN BEGIN
      Widget_Control, parent, TLB_Get_Offset=offsets, TLB_Get_Size=theSize
      self.tlb_xoffset = offsets[0] + theSize[0] + 10
      self.tlb_yoffset = offsets[1]
   ENDIF
   tlb = Widget_Base(Title='Active Contour Controls', Column=1, Group_Leader=parent, $
      XOffset=self.tlb_xoffset, YOffset=self.tlb_yoffset, UValue=self, MBar=menuID)
   self.control_TLB = tlb

   ; Make it a reciprical group leader.
   IF self.draw_create THEN Widget_Control, parent, Group_Leader=tlb

   fileID = Widget_Button(menuID, Value='File')
   IF self.draw_create THEN button = Widget_Button(fileID, Value='New Image', UValue={object:self, action:"NEW_IMAGE"})

   self.roiSaveID = Widget_Button(fileID, Value='Save Contour', UValue={object:self, action:"CONTOUR_SAVE"}, /Separator)
   button = Widget_Button(fileID, Value='Load Contour', UValue={object:self, action:"CONTOUR_LOAD"})

   button = Widget_Button(fileID, Value='Save Parameters', UValue={object:self, action:"PARAMETERS_SAVE"}, /Separator)
   button = Widget_Button(fileID, Value='Load Parameters', UValue={object:self, action:"PARAMETERS_LOAD"})

   button = Widget_Button(fileID, Value='Quit', UValue={object:self, action:"DISMISS"}, /Separator)


   viewID = Widget_Button(menuID, Value='View')
   button = Widget_Button(viewID, Value='Edgemap', UValue={object:self, action:"VIEW_EDGEMAP"})
   button = Widget_Button(viewID, Value='Gradient Fields', UValue={object:self, action:"VIEW_GRADIENTS"})
   button = Widget_Button(viewID, Value='Vector Field', UValue={object:self, action:"VIEW_VELOVECT"})

   ; Create an image adjustment menu if you have control over where image is displayed.
   adjustID = Widget_Button(menuID, Value='Adjustments')
   self.undoID = Widget_Button(adjustID, Value='Undo', UValue={object:self, action:"UNDO"})
   Widget_Control, self.undoID, Sensitive=0
   smoothID = Widget_Button(adjustID, Value='Smooth Filter', /Menu, /Separator)
   button = Widget_Button(smoothID, Value='3x3 Neighborhood', UValue={object:self, action:"SMOOTH_FILTER", UValue:3})
   button = Widget_Button(smoothID, Value='5x5 Neighborhood', UValue={object:self, action:"SMOOTH_FILTER", UValue:5})
   button = Widget_Button(smoothID, Value='7x7 Neighborhood', UValue={object:self, action:"SMOOTH_FILTER", UValue:7})
   button = Widget_Button(smoothID, Value='9x9 Neighborhood', UValue={object:self, action:"SMOOTH_FILTER", UValue:9})
   medianID = Widget_Button(adjustID, Value='Median Filter', /Menu)
   button = Widget_Button(medianID, Value='3x3 Neighborhood', UValue={object:self, action:"MEDIAN_FILTER", UValue:3})
   button = Widget_Button(medianID, Value='5x5 Neighborhood', UValue={object:self, action:"MEDIAN_FILTER", UValue:5})
   button = Widget_Button(medianID, Value='7x7 Neighborhood', UValue={object:self, action:"MEDIAN_FILTER", UValue:7})
   button = Widget_Button(medianID, Value='9x9 Neighborhood', UValue={object:self, action:"MEDIAN_FILTER", UValue:9})
   button = Widget_Button(adjustID, Value='Sharpen Image', UValue={object:self, action:"SHARPEN"})
   button = Widget_Button(adjustID, Value='Contrast Enhance', UValue={object:self, action:"CONTRAST_ENHANCE"})
   button = Widget_Button(adjustID, Value='Original Image', /Separator, UValue={object:self, action:"ORIGINAL_IMAGE"})
   button = Widget_Button(adjustID, Value='Contour Color', /Separator, UValue={object:self, action:"COLOR"})

   ; Help button.
   helpID = Widget_Button(menuID, Value='Help', /Help)
   button = Widget_Button(helpID, Value='Active Contour User Guide', UValue={object:self, action:"HELP", uvalue:'GUIDE'})
   button = Widget_Button(helpID, Value='Active Contour Program Documentation', UValue={object:self, action:"HELP", uvalue:'DOCS'})


   firstTabID = Widget_Base(tlb, Title='Controls', Column=1, /Base_Align_Center, /Frame)

   topBase = Widget_Base(firstTabID, Column=2)
   labelsize = 130
   textsize = 10
   self.alphaObj =   FSC_Inputfield(topBase, Value=self.alpha, Title='Elasticity (alpha)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.betaObj  =   FSC_Inputfield(topBase, Value=self.beta, Title='Rigidity (beta)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.gammaObj  =  FSC_Inputfield(topBase, Value=self.gamma, Title='Viscosity (gamma)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.kappaObj  =  FSC_Inputfield(topBase, Value=self.kappa, Title='External Force (kappa)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.gscaleObj  = FSC_Inputfield(topBase, Value=self.gradientScale, Title='Gradient Scale Factor', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.dminObj  =   FSC_Inputfield(topBase, Value=self.dmin, Title='Delta Min (dmin)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.dmaxObj  =   FSC_Inputfield(topBase, Value=self.dmax, Title='Delta Max (dmax)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.muObj  =     FSC_Inputfield(topBase, Value=self.mu, Title='Noise Parameter (mu)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)
   self.gvfIterObj = FSC_Inputfield(topBase, Value=self.gvf_iterations, Title='GVF Iterations', $
      Labelsize=labelsize, XSize=textsize)
   self.iterObj =    FSC_Inputfield(topBase, Value=self.iterations, Title='Contour Iterations', $
      Labelsize=labelsize, XSize=textsize)

   blurBase = Widget_Base(firstTabID,Col=2, /Grid)
   leftBase = Widget_Base(blurbase, Row=1)
   rightBase = Widget_Base(blurbase, Row=1)
   label = Widget_Label(leftBase, Value='Gaussian Blur: ', Scr_Xsize=70)
   gaussBase = Widget_Base(leftBase,/Exclusive, Row=1)
   self.gaussOn = Widget_Button(gaussBase, Value='On', UValue={object:self, action:"GAUSSBLUR"}, /No_Release)
   IF self.blur THEN Widget_Control, self.gaussOn, Set_Button=1 ELSE Widget_Control, self.gaussOn, Set_Button=0
   self.gaussOff = Widget_Button(gaussBase, Value='Off', UValue={object:self, action:"GAUSSBLUR"}, /No_Release)
   IF self.blur THEN Widget_Control, self.gaussOff, Set_Button=0 ELSE Widget_Control, self.gaussOff, Set_Button=1
   self.sigmaObj  =  FSC_Inputfield(rightBase, Value=self.sigma, Title='Gaussian Sigma (sigma)', $
      Decimal=2, /FloatValue, Labelsize=labelsize, XSize=textsize)

   imageValueBase = Widget_Base(firstTabID, Row=1)
   label = Widget_Label(imageValueBase, Value='Image Value: ')
   self.valueID = Widget_Label(imageValueBase, Value=" ", Scr_XSize=50, /Sunken_Frame)
   spacer = Widget_Label(imageValueBase, Value=String(Replicate(32B, 10)))
   label = Widget_Label(imageValueBase, Value='Contour Perimeter: ')
   self.perimeterID = Widget_Label(imageValueBase, Value=" ", Scr_XSize=50, /Sunken_Frame)
   spacer = Widget_Label(imageValueBase, Value=String(Replicate(32B, 10)))
   label = Widget_Label(imageValueBase, Value='Contour Area: ')
   self.areaID = Widget_Label(imageValueBase, Value=" ", Scr_XSize=50, /Sunken_Frame)


   buttonBase = Widget_Base(firstTabID, Row=1, Grid_Layout=1)
   button = Widget_Button(buttonBase, Value='Dismiss', UValue={object:self, action:'DISMISS'})
   button = Widget_Button(buttonBase, Value='Default Parameters', UValue={object:self, action:'DEFAULTS'})
   button = Widget_Button(buttonBase, Value='Clear Window', UValue={object:self, action:'CLEAR_WINDOW'})
   self.applyID = Widget_Button(buttonBase, Value='Apply Active Contour', $
      /Dynamic_Resize, UValue={object:self, action:'APPLY'})
   IF Ptr_Valid(self.xsnake_f) THEN BEGIN
      Widget_Control, self.applyID, Sensitive=1
   ENDIF ELSE BEGIN
      Widget_Control, self.applyID, Sensitive=0
   ENDELSE

   Widget_Control, tlb, /Realize

   ; Draw the gradients and force field.
   self -> Edgemap

   XManager, 'activecontour_controls' + Get_Object_ID(self), tlb, /No_Block, $
      Event_Handler='ActiveContour_WidgetEvents', $
      Cleanup='ActiveContour_ControlsCleanup'

END


;+
; This procedure destroys the active contour control panel.
;-      
PRO ActiveContour::DestroyControls

   COMPILE_OPT idl2, Hidden

   ; Update the location of the TLB, so we can always recall it to the
   ; right location if the user moves it.
   IF Obj_Valid(self) THEN BEGIN
      IF Widget_Info(self.control_TLB, /Valid_ID) THEN BEGIN
         Widget_Control, self.control_TLB, TLB_Get_Offset=theOffsets
         self.tlb_xoffset = theOffsets[0]
         self.tlb_yoffset = theOffsets[1]
      ENDIF
   ENDIF

   ; Destroy the control panel.
   IF Widget_Info(self.control_TLB, /Valid_ID) THEN $
      Widget_Control, self.control_TLB, /Destroy

END



;+
; This event handler handles all events from the draw widget.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;         The events handled here are passed to the program from the draw widget.
;-      
PRO ActiveContour::Draw_Widget_Events, event

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(tlb) NE 0 THEN Widget_Control, tlb, /Destroy
   ENDIF

   ; If you don't have a valid draw widget. Disappear.
   IF Widget_Info(self.drawID, /Valid_ID) EQ 0 THEN RETURN

   ; Possible events and button information.
   possibleEvents=['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE', 'KEY_1', 'KEY_2', 'WHEEL']
   whichButton = ['NONE', 'LEFT', 'MIDDLE', 'NONE', 'RIGHT']

   ; Make this the current graphics window. Do everything in 24-bit mode.
   WSet, self.wid
   Device, Decomposed=1

   ; What kind of event was this? Do the right thing here. :-)
   thisEvent = possibleEvents[event.type]
   CASE thisEvent OF

      'DOWN':BEGIN ; Button DOWN event.

         ; Which button was pressed? Branch accordingly. RIGHT button closes the loop.
         ; Anything else adds to the selection.
         theButton = whichButton[event.press]

         CASE theButton OF

            'RIGHT':BEGIN

               ; No events unless we have clicked another button first.
               IF self.selectmap LT 0 THEN RETURN

               ; No double clicking.
               IF event.clicks EQ 2 THEN RETURN

               ; Close the loop.
               IF self.npts LT 2 THEN BEGIN
                 ok = Dialog_Message('Three points required to create initial snake boundary.')
                 self.npts = 0
                 Ptr_Free, self.xsnake
                 Ptr_Free, self.ysnake
                 IF Widget_Info(self.applyID, /Valid_ID) THEN Widget_Control, self.applyID, Sensitive=0
                 self -> ClearWindow
                 RETURN
               ENDIF

               ; Add point to snake.
               IF Ptr_Valid(self.xsnake) THEN *self.xsnake = [ *self.xsnake, event.x] ELSE $
                 self.xsnake = Ptr_New([event.x])
               IF Ptr_Valid(self.ysnake) THEN *self.ysnake = [ *self.ysnake, event.y] ELSE $
                 self.ysnake = Ptr_New([event.y])
               self.npts = self.npts + 1

               ; Draw the last line in the selection pixmap
               IF self.npts GE 2 THEN BEGIN
                 WSet, self.selectmap
                 cgPlotS, (*self.xsnake)[self.npts-2:self.npts-1], Color=self.color, $
                       (*self.ysnake)[self.npts-2:self.npts-1], /Device, Thick=2
               ENDIF

               ; Rubberband from last point to this one.
               IF self.npts GT 1 THEN BEGIN
                 WSet, self.wid
                 Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.selectMap]
                 cgPlotS, [(*self.xsnake)[self.npts-1], event.x], Color=self.color, $
                        [(*self.ysnake)[self.npts-1], event.y], /Device, Thick=2
               ENDIF

               ; Wrap the points around on each other.
               *self.xsnake = [*self.xsnake, (*self.xsnake)[0]]
               *self.ysnake = [*self.ysnake, (*self.ysnake)[0]]
                WSet, self.wid
                Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.selectMap]
                cgPlotS, *self.xsnake, *self.ysnake, Color=self.color, $
                        /Device, Thick=2

               ; Interpolate into equally-spaced arcs.
               self -> ArcSample, *self.xsnake, *self.ysnake, x, y, Points=150
               *self.xsnake = x
               *self.ysnake = y

               ; Plot the entire contour now and delete selection pixmap.
               WSet, self.wid
               Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixMap]
               cgPlotS, *self.xsnake, Color=self.color, $
                      *self.ysnake, /Device, Thick=2
               WDelete, self.selectmap

               ; Transfer current contour to final contour storage, so
               ; you can draw another contour if desired.
               IF Ptr_Valid(self.xsnake_f) THEN BEGIN
                  *self.xsnake_f = *self.xsnake
               ENDIF ELSE BEGIN
                  self.xsnake_f = Ptr_New(*self.xsnake)
               ENDELSE
               Ptr_Free, self.xsnake

               IF Ptr_Valid(self.ysnake_f) THEN BEGIN
                  *self.ysnake_f = *self.ysnake
               ENDIF ELSE BEGIN
                  self.ysnake_f = Ptr_New(*self.ysnake)
               ENDELSE
               Ptr_Free, self.ysnake

               ; Set up for drawing another contour, if desired.
               self.npts = 0
               IF Widget_Info(self.applyID, /Valid_ID) THEN $
                  Widget_Control, self.applyID, Sensitive=1

               ; Send an event to interested parties.
               self -> SendEvent, Type='POINTS_COLLECTED'

            END

            ELSE: BEGIN ; It is LEFT or MIDDLE mouse button.

               IF event.clicks EQ 1 THEN BEGIN ; Single click selects new point.

                  ; If selectmap doesn't exist, create it.
                  IF self.npts EQ 0 THEN BEGIN
                     WSet, self.wid
                     Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
                     Window, XSize=self.xsize, YSize=self.ysize, /Pixmap, /Free
                     self.selectmap = !D.Window
                     Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
                     WSet, self.wid

                     ; Free up pointers that indicate this is the first point.
                     Ptr_Free, self.xsnake_f
                     Ptr_Free, self.ysnake_f
                     IF Widget_Info(self.applyID, /Valid_ID) THEN $
                       Widget_Control, self.applyID, Sensitive=0
                     IF Widget_Info(self.roiSaveID, /Valid_ID) THEN $
                        Widget_Control, self.roiSaveID, Sensitive=0
                  ENDIF

                  ; Add point to snake.
                  IF Ptr_Valid(self.xsnake) THEN *self.xsnake = [ *self.xsnake, event.x] ELSE $
                     self.xsnake = Ptr_New([event.x])
                  IF Ptr_Valid(self.ysnake) THEN *self.ysnake = [ *self.ysnake, event.y] ELSE $
                     self.ysnake = Ptr_New([event.y])
                  self.npts = self.npts + 1
                  IF self.npts GE 2 THEN BEGIN
                     WSet, self.selectmap
                     cgPlotS, (*self.xsnake)[self.npts-2:self.npts-1], Color=self.color, $
                           (*self.ysnake)[self.npts-2:self.npts-1], /Device, Thick=2
                  ENDIF
                  IF self.npts GT 1 THEN BEGIN
                     WSet, self.wid
                     Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.selectMap]
                     cgPlotS, [(*self.xsnake)[self.npts-1], event.x], Color=self.color, $
                            [(*self.ysnake)[self.npts-1], event.y], /Device, Thick=2

                  ENDIF

                  ; Start everything over again.
                  IF Widget_Info(self.perimeterID, /Valid_ID) THEN BEGIN
                      Widget_Control, self.perimeterID, Set_Value=''
                      Widget_Control, self.areaID, Set_Value=''
                  ENDIF

                ENDIF ELSE BEGIN ; Double click to close loop.

                  ; Need at least thee points to close the loop.
                  IF self.npts LT 3 THEN BEGIN
                     ok = Dialog_Message('Three points required to create initial snake boundary.')
                     self.npts = 0
                     Ptr_Free, self.xsnake
                     Ptr_Free, self.ysnake
                     IF Widget_Info(self.applyID, /Valid_ID) THEN $
                        Widget_Control, self.applyID, Sensitive=0
                     self -> ClearWindow
                     RETURN
                  ENDIF

                  ; Add last point to snake.
                  IF Ptr_Valid(self.xsnake) THEN *self.xsnake = [ *self.xsnake, event.x] ELSE $
                    self.xsnake = Ptr_New([event.x])
                  IF Ptr_Valid(self.ysnake) THEN *self.ysnake = [ *self.ysnake, event.y] ELSE $
                    self.ysnake = Ptr_New([event.y])
                  self.npts = self.npts + 1

                  ; Draw the last line in the selection pixmap
                  IF self.npts GE 2 THEN BEGIN
                    WSet, self.selectmap
                    cgPlotS, (*self.xsnake)[self.npts-2:self.npts-1], Color=self.color, $
                          (*self.ysnake)[self.npts-2:self.npts-1], /Device, Thick=2
                  ENDIF

                  ; Rubberband from last point to this one.
                  IF self.npts GT 1 THEN BEGIN
                    WSet, self.wid
                    Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.selectMap]
                    cgPlotS, [(*self.xsnake)[self.npts-1], event.x], Color=self.color, $
                           [(*self.ysnake)[self.npts-1], event.y], /Device, Thick=2
                  ENDIF

                  ; Wrap the points around on each other.
                  *self.xsnake = [*self.xsnake, (*self.xsnake)[0]]
                  *self.ysnake = [*self.ysnake, (*self.ysnake)[0]]

                  ; Interpolate into equally-spaced arcs.
                  self -> ArcSample, *self.xsnake, *self.ysnake, x, y, Points=150
                  *self.xsnake = x
                  *self.ysnake = y

                  ; Plot the entire contour now and delete selection pixmap.
                  WSet, self.wid
                  Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixMap]
                  cgPlotS, *self.xsnake, Color=self.color, $
                         *self.ysnake, /Device, Thick=2
                  WDelete, self.selectmap

                  ; Transfer current contour to final contour storage, so
                  ; you can draw another contour if desired.
                  IF Ptr_Valid(self.xsnake_f) THEN BEGIN
                     *self.xsnake_f = *self.xsnake
                  ENDIF ELSE BEGIN
                     self.xsnake_f = Ptr_New(*self.xsnake)
                  ENDELSE
                  Ptr_Free, self.xsnake

                  IF Ptr_Valid(self.ysnake_f) THEN BEGIN
                     *self.ysnake_f = *self.ysnake
                  ENDIF ELSE BEGIN
                     self.ysnake_f = Ptr_New(*self.ysnake)
                  ENDELSE
                  Ptr_Free, self.ysnake

                  ; Set up for drawing another contour, if desired.
                  self.npts = 0
                  IF Widget_Info(self.applyID, /Valid_ID) THEN $
                     Widget_Control, self.applyID, Sensitive=1

                  ; Send an event to interested parties.
                  self -> SendEvent, Type='POINTS_COLLECTED'

                  END

               ENDELSE
         ENDCASE

         END

      'UP': BEGIN ; Button UP event.

         ; Which button was released?
         theButton = whichButton[event.release]

         ;
         CASE theButton OF

            'RIGHT': BEGIN
            END

            ELSE:

         ENDCASE

         END

      'MOTION': BEGIN

         ; Update the image value. The point in window space has to be converted
         ; to image pixel space.
         normCoord = Convert_Coord(event.x, event.y, /TO_NORMAL, /DEVICE)
         xNorm = Transpose(normCoord[0,*])
         yNorm = Transpose(normCoord[1,*])
       
         s = Size(*self.original, /DIMENSIONS)
         xvec = Scale_Vector(Dindgen(s[0]+1), 0, 1)
         yvec = Scale_Vector(Dindgen(s[1]+1), 0, 1)
         xpixCoord = Round(Value_Locate(xvec, xNorm))
         ypixCoord = Round(Value_Locate(yvec, yNorm))
         value = (*self.original)[xpixCoord, ypixCoord]
         IF Widget_Info(self.valueID, /Valid_ID) THEN BEGIN
            Widget_Control, self.valueID, Set_Value=cgNumber_Formatter(value, Decimals=3)
         ENDIF

         ; Copy and draw the last two points.
         IF self.npts GE 1 THEN BEGIN
            WSet, self.wid
            Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.selectMap]
            cgPlotS, [(*self.xsnake)[self.npts-1], event.x], Color=self.color, $
                   [(*self.ysnake)[self.npts-1], event.y], /Device, Thick=2
         ENDIF
         END

      ELSE:

   ENDCASE


END


;+
; This method calculates the edgemap for the object image. The edgemap is stored in 
; the pointer self.edgemap.
;-      
PRO ActiveContour::Edgemap

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      Ptr_Free, self.edgemap
      RETURN
   ENDIF

   ; An edgemap is the gradient of the image, scaled into 0 to 1.
   edgemap = 1.0 - Scale_Vector(Sobel(*self.image), 1.0, 0.0)

   IF Ptr_Valid(self.edgemap) THEN *self.edgemap = edgemap ELSE self.edgemap = Ptr_New(edgemap)

   ; Apply a gassian blur to the image?
   IF self.blur THEN *self.edgemap = self -> GaussianBlur(*self.edgemap)

   ; Calculate gradients fx and fy.
   IF Ptr_Valid(self.fx) THEN *self.fx = self -> Gradient(*self.edgemap, 0) ELSE $
      self.fx = Ptr_New(self -> Gradient(*self.edgemap, 0))
   IF Ptr_Valid(self.fy) THEN *self.fy = self -> Gradient(*self.edgemap, 1) ELSE $
      self.fy = Ptr_New(self -> Gradient(*self.edgemap, 1))

   ; Will need new GVF fields calculated. Freeing pointers will ensure it.
   Ptr_Free, self.u
   Ptr_Free, self.v

END


;+
; This event handler is the main event handler for the program. It handles
; everything but draw widget events.
; 
; :Params:
;     event: in, required, type=structure
;         The event structure passed by the window manager to this program when called.
;-      
PRO ActiveContour::Event_Handler, event

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(tlb) NE 0 THEN Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Make sure the image window is displayed.
   WShow, self.wid
   Device, Decomposed=1

   CASE event.action OF

      'APPLY': BEGIN

            ; Apply the snaking algorithm.
            info = self -> ApplyGVFSnake()

         END

      'CLEAR_WINDOW': self -> ClearWindow

      'COLOR':  self.color = cgPickColorName(self.color)

      'CONTOUR_LOAD': BEGIN

            ; Get a filename for reading.
            thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='roi', '')
            filename = Dialog_Pickfile(Path=thePath, Filter='*.roi', Title='Select Input Contour File...')
            IF filename EQ "" THEN RETURN

            ; Read the data.
            OpenR, lun, filename, /Get_Lun
            n = 0L
            ReadU, lun, n
            x = FltArr(n)
            y = FltArr(n)
            perimeter = 0.0D
            area = 0.0D
            ReadU, lun, x, y, perimeter, area
            Free_Lun, lun

            ; Load the appropriate vectors and display on the window.
            Ptr_Free, self.xsnake
            Ptr_Free, self.ysnake
            self.npts = 0 ; Can draw another contour, if you want
            IF Ptr_Valid(self.xsnake_f) THEN *self.xsnake_f = x ELSE $
               self.xsnake_f = Ptr_New(x)
            IF Ptr_Valid(self.ysnake_f) THEN *self.ysnake_f = y ELSE $
               self.ysnake_f = Ptr_New(y)
            WSet, self.wid
            Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.pixmap]
            cgPLOTS, *self.xsnake_f, *self.ysnake_f, $
               /Device, Color=self.color, Thick=2
            Widget_Control, self.applyID, Sensitive=1

         END

      'CONTOUR_SAVE': BEGIN

            ; Get a filename for writing.
            thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='roi', '')
            filename = Dialog_Pickfile(Path=thePath, File='contour.roi', $
               Title='Specify Output Contour File Name...', /Write)
            IF filename EQ "" THEN RETURN

            ; Get the ROI data.
            IF Obj_Valid(self.roi) THEN BEGIN

               self.roi -> GetProperty, Data=theRoi
               ok = self.roi -> ComputeGeometry(Perimeter=perimeter, Area=area, Spatial_Scale=self.spatial_scale)
               x = Reform(theROI[0,*])
               y = Reform(theROI[1,*])

               ; Write the file.
               OpenW, lun, filename, /Get_Lun
               WriteU, lun, N_Elements(x), Float(x), Float(y), perimeter, area
               Free_Lun, lun

            ENDIF ELSE ok = Dialog_Message('There is no valid final contour currently.')

         END

      'CONTRAST_ENHANCE': BEGIN

               ; Set up the undo images.
               IF Ptr_Valid(self.undo) EQ 0 THEN BEGIN
                  self.undo = Ptr_New([Ptr_New(*self.original)])
               ENDIF ELSE BEGIN
                  *self.undo = [Ptr_New(*self.original), *self.undo]
               ENDELSE
               Widget_Control, self.undoID, Sensitive=1

               Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=theSize
               xoffset = offsets[0]
               yoffset = offsets[1] + theSize[1] + 50

               ; Which filter should be apply?
               cgStretch, *self.original, Notify_Obj={object:self, method:'CONTRAST_STRETCH'}, $
                  /No_Window, Group_Leader=event.top, XPos=xoffset, YPos=yoffset

            END

      'DISMISS': BEGIN
            self -> ResetDisplay
            IF Widget_Info(event.top, /Valid_ID) THEN Widget_Control, event.top, /Destroy
         END

      'DEFAULTS': BEGIN
            ; Assign default values.
            self.alphaObj -> Set_Value, 0.1
            self.betaObj -> Set_Value, 0.25
            self.gammaObj -> Set_Value, 1.0
            self.kappaObj -> Set_Value, 1.25
            self.gscaleObj -> Set_Value, 1.75
            self.dminObj -> Set_Value, 0.25
            self.dmaxObj -> Set_Value, 5.5
            self.muObj -> Set_Value, 0.1
            self.gvfiterObj -> Set_Value, 30
            self.iterObj -> Set_Value, 120
            Widget_Control, self.gaussOn,  Set_Button=1
            Widget_Control, self.gaussOff, Set_Button=0
            self.sigmaObj -> Set_Value, 1.0
         END

      'DRAW_WIDGET_EVENTS': self -> Draw_Widget_Events, event

      'GAUSSBLUR': BEGIN

            ; Events come here from the Gaussian Blur ON/OFF buttons. There
            ; is nothing to do, except make sure the gradients are recalculated.
            self.blur = Widget_Info(self.gaussOn, /Button_Set)

            ; Calculate the edgemap.
            self -> Edgemap

            ; Apply a gassian blur to the image?
            IF self.blur THEN *self.edgemap = self -> GaussianBlur(*self.edgemap)

            ; Calculate gradients fx and fy.
            *self.fx = self -> Gradient(*self.edgemap, 0)
            *self.fy = self -> Gradient(*self.edgemap, 1)

            ; The GVF gradients will have to be updated now.
            Ptr_Free, self.u
            Ptr_Free, self.v
         END

      'HELP': BEGIN

            CASE event.uvalue OF

            'GUIDE': BEGIN
                  file = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='docs', 'activecontour.pdf')
                  IF File_Test(file) THEN OnLine_Help, Book=file ELSE $
                     ok = Dialog_Message('The on-line help file "activecontour.pdf" cannot be located')
               END

            'DOCS': BEGIN
                  IF self.demo_version THEN BEGIN
                     ok = Dialog_Message('Program documentation is not available in Demo Version.')
                  ENDIF ELSE BEGIN
                     file = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='docs', 'index.html')
                     IF File_Test(file) THEN OnLine_Help, Book=file ELSE $
                        ok = Dialog_Message('The on-line help file "activecontour.htm" cannot be located')
                  ENDELSE
               END

            ENDCASE

         END

      'MEDIAN_FILTER': BEGIN

               ; Set up the undo images.
               IF Ptr_Valid(self.undo) EQ 0 THEN BEGIN
                  self.undo = Ptr_New([Ptr_New(*self.image)])
               ENDIF ELSE BEGIN
                  *self.undo = [Ptr_New(*self.image), *self.undo]
               ENDELSE
               Widget_Control, self.undoID, Sensitive=1

               ; Which filter should be apply?
               *self.image = Median(*self.image, event.uvalue)

               ; Update the image.
               self -> UpdateImage

            END

      'NEW_IMAGE': BEGIN
            thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='images', '')
            image = ImageSelect(Directory=thePath, /Only2D, Cancel=cancelled, Group_Leader=event.top)
            IF cancelled THEN RETURN

            self -> SetProperty, IMAGE=image
            
            ; Clean up all the undo pointers.
            IF Ptr_Valid(self.undo) THEN BEGIN
               ptrs = *self.undo
               Ptr_Free, ptrs
               Ptr_Free, self.undo
            ENDIF
            Widget_Control, self.undoID, Sensitive=0

            ; Free important pointers.
            Ptr_Free, self.edgemap
            Ptr_Free, self.fx
            Ptr_Free, self.fy
            Ptr_Free, self.u
            Ptr_Free, self.v

            ; Calculate the edgemap.
            self -> Edgemap

            ; Clear the widnow.
            self -> ClearWindow

         END

      'NULL': ; Null events pass through without doing anything.

      'ORIGINAL_IMAGE': BEGIN

            ; Set up the undo images.
            IF Ptr_Valid(self.undo) EQ 0 THEN BEGIN
               self.undo = Ptr_New([Ptr_New(*self.image)])
            ENDIF ELSE BEGIN
               *self.undo = [Ptr_New(*self.image), *self.undo]
            ENDELSE
            Widget_Control, self.undoID, Sensitive=1

            *self.image = *self.original
            self.min_v = Min(*self.original)
            self.max_v = Max(*self.original)
            *self.image = *self.original
            self -> Edgemap
            self -> ClearWindow
            self -> UpdateImage

         END

      'PARAMETERS_SAVE': BEGIN
            thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='parameters', '')
            file = Dialog_Pickfile(Path=thePath, File='snake_param.dat', $
               Title='Specify Parameter Ouput File Name...', /Write)
            IF file EQ "" THEN RETURN

            ; Make sure everything is updated.
            self -> UpdateParameters

            ; Save the file parameters.
            OpenW, lun, file, /Get_Lun
            PrintF, lun, self.alpha, "    ", 'Elasticity (alpha)'
            PrintF, lun, self.beta, "    ", 'Rididity (beta)'
            PrintF, lun, self.gamma, "    ", 'Viscosity (gamma))'
            PrintF, lun, self.kappa, "    ", 'External Force (kappa)'
            PrintF, lun, self.dmin, "    ", 'Delta Min (dmin)'
            PrintF, lun, self.dmax, "    ", 'Delta Max (dmax)'
            PrintF, lun, self.mu, "    ", 'Noise Parameter (mu)'
            PrintF, lun, self.gradientScale, "    ", 'Gradient Scale Factor'
            PrintF, lun, self.gvf_iterations, "    ", 'GVF Iterations'
            PrintF, lun, self.iterations, "    ", 'Contour Iterations'
            PrintF, lun, self.blur, "    ", 'Gaussian Blur'
            PrintF, lun, self.sigma, "    ", 'Gaussian Sigma (sigma)'
            PrintF, lun, self.min_v, "    ", 'Minimum Scale Value'
            PrintF, lun, self.max_v, "    ", 'Maximum Scale Value'
            Free_Lun, lun
         END

      'PARAMETERS_LOAD': BEGIN
            thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='parameters', '')
            file = Dialog_Pickfile(Path=thePath, File='defaults.dat', Title='Select Input Parameter File...')
            IF file EQ "" THEN RETURN

            OpenR, lun, file, /Get_Lun
            temp = 0.0

            ReadF, lun, temp
            self.alpha = temp
            IF Obj_Valid(self.alphaObj) THEN self.alphaObj -> Set_Value, temp

            ReadF, lun, temp
            self.beta = temp
            IF Obj_Valid(self.betaObj) THEN self.betaObj -> Set_Value, temp

            ReadF, lun, temp
            self.gamma = temp
            IF Obj_Valid(self.gammaObj) THEN self.gammaObj -> Set_Value, temp

            ReadF, lun, temp
            self.kappa = temp
            IF Obj_Valid(self.kappaObj) THEN self.kappaObj -> Set_Value, temp

            ReadF, lun, temp
            self.dmin = temp
            IF Obj_Valid(self.dminObj) THEN self.dminObj -> Set_Value, temp

            ReadF, lun, temp
            self.dmax = temp
            IF Obj_Valid(self.dmaxObj) THEN self.dmaxObj -> Set_Value, temp

            ReadF, lun, temp
            self.mu = temp
            IF Obj_Valid(self.muObj) THEN self.muObj -> Set_Value, temp

            ReadF, lun, temp
            self.gradientScale = temp

            temp = 0
            ReadF, lun, temp
            self.gvf_iterations = temp
            IF Obj_Valid(self.gvfiterObj) THEN self.gvfiterObj -> Set_Value, temp

            temp = 0
            ReadF, lun, temp
            self.iterations = temp
            IF Obj_Valid(self.iterObj) THEN self.iterObj -> Set_Value, temp

            temp = 0
            ReadF, lun, temp
            self.blur = temp
            IF self.blur THEN BEGIN
               IF Obj_Valid(self.gaussON) THEN BEGIN
                  Widget_Control, self.gaussON, Button_Set=1
                  Widget_Control, self.gaussOFF, Button_Set=0
               ENDIF
            ENDIF ELSE BEGIN
               IF Obj_Valid(self.gaussON) THEN BEGIN
                  Widget_Control, self.gaussON, Button_Set=0
                  Widget_Control, self.gaussOFF, Button_Set=1
               ENDIF
            ENDELSE

            temp = 0.0
            ReadF, lun, temp
            self.sigma = temp
            IF Obj_Valid(self.sigmaObj) THEN self.sigmaObj -> Set_Value, temp

            temp = 0.0
            ReadF, lun, temp
            self.min_v = temp

            temp = 0.0
            ReadF, lun, temp
            self.max_v = temp

            self -> UpdateImage

            END

      'SHARPEN': BEGIN

               ; Set up the undo images.
               IF Ptr_Valid(self.undo) EQ 0 THEN BEGIN
                  self.undo = Ptr_New([Ptr_New(*self.image)])
               ENDIF ELSE BEGIN
                  *self.undo = [Ptr_New(*self.image), *self.undo]
               ENDELSE
               Widget_Control, self.undoID, Sensitive=1

               ; Which filter should be apply?
               *self.image = Sharpen(*self.image)

               ; Update the image.
               self -> UpdateImage

            END

      'SMOOTH_FILTER': BEGIN

               ; Set up the undo images.
               IF Ptr_Valid(self.undo) EQ 0 THEN BEGIN
                  self.undo = Ptr_New([Ptr_New(*self.image)])
               ENDIF ELSE BEGIN
                  *self.undo = [Ptr_New(*self.image), *self.undo]
               ENDELSE
               Widget_Control, self.undoID, Sensitive=1

               ; Which filter should be apply?
               *self.image = Smooth(*self.image, event.uvalue, /Edge_Truncate)

               ; Update the image.
               self -> UpdateImage

            END

      'UNDO': BEGIN

            IF Ptr_Valid(self.undo) EQ 0 THEN RETURN

            IF N_Elements(*self.undo) EQ 1 THEN BEGIN
               *self.image = *(*self.undo)[0]
               Ptr_Free, (*self.undo)[0]
               Ptr_Free, self.undo
               Widget_Control, self.undoID, Sensitive=0
            ENDIF ELSE BEGIN
               *self.image = *(*self.undo)[0]
               Ptr_Free, (*self.undo)[0]
               *self.undo = (*self.undo)[1:N_Elements(*self.undo)-1]
            ENDELSE

            self -> UpdateImage

         END

      'VIEW_EDGEMAP': BEGIN

            IF Ptr_Valid(self.edgemap) EQ 0 THEN self -> Edgemap

            tlb = Widget_Base(Title='Edgemap', Group_Leader=self.drawID)
            draw = Widget_Draw(tlb, XSize=self.xsize, YSize=self.ysize)
            cgCenterTLB, tlb
            Widget_Control, tlb, /Realize
            Widget_Control, draw, Get_Value=wid
            WSet, wid
            TVLCT, self.palette
            cgImage, *self.edgemap, /Scale, /Interp

         END

      'VIEW_GRADIENTS': BEGIN

            IF Ptr_Valid(self.edgemap) EQ 0 THEN self -> Edgemap

            tlb = Widget_Base(Title='Gradients: Fx and Fy', Group_Leader=self.drawID)
            draw = Widget_Draw(tlb, XSize=self.xsize*2, YSize=self.ysize)
            cgCenterTLB, tlb
            Widget_Control, tlb, /Realize
            Widget_Control, draw, Get_Value=wid
            WSet, wid
            LoadCT, 0, /Silent
            !P.Multi=[0,2,1]
            cgImage, *self.fx, /Scale, /Interp
            cgImage, *self.fy, /Scale, /Interp
            !P.Multi=0
         END

      'VIEW_VELOVECT': BEGIN

            IF Ptr_Valid(self.edgemap) EQ 0 THEN self -> Edgemap

            tlb = Widget_Base(Title='Vector Force Field', Group_Leader=self.drawID)
            draw = Widget_Draw(tlb, XSize=600, YSize=600)
            cgCenterTLB, tlb
            Widget_Control, tlb, /Realize
            Widget_Control, draw, Get_Value=wid
            WSet, wid
            LoadCT, 0, /Silent
            Velovect, Congrid(*self.fx, self.xsize/4, self.ysize/4), $
               Congrid(*self.fy, self.xsize/4, self.ysize/4);, Position=[0, 0, 1, 1]

         END

      ELSE: BEGIN
         ok = Dialog_Message('Found unhandled event')
         Help, event, /Structure
         END
   ENDCASE

   ; Update the location of the TLB, so we can always recall it to the
   ; right location if the user moves it.
   IF Obj_Valid(self) THEN BEGIN
      IF Widget_Info(self.control_TLB, /Valid_ID) THEN BEGIN
         Widget_Control, self.control_TLB, TLB_Get_Offset=theOffsets
         self.tlb_xoffset = theOffsets[0]
         self.tlb_yoffset = theOffsets[1]
      ENDIF
   ENDIF
END


;+
; This function applies a Gaussian filter function to the image.
; 
; :Returns:
;     Returns a blurred image.
;     
; :Params:
;     image: in, required,
;         The image to be blurred.
;         
; :Keywords:
;      scale: in, optional, type=float, default=1.0
;          A scale factor that is multiplied to the Gaussian mask.
;-      
FUNCTION ActiveContour::GaussianBlur, image, Scale=scale

   COMPILE_OPT idl2, Hidden

   IF N_Elements(scale) EQ 0 THEN scale=1.0

   ; Create the gaussian mask with a particular sigma value.
   r = Ceil(3 * self.sigma)
   mask = FltArr(2*r+1, 2*r+1)
   FOR i=-r, r DO BEGIN
      FOR j=-r,r DO BEGIN
      mask[i+r, j+r] = scale * Exp(-(i^2+j^2)/2/self.sigma/self.sigma)/(2*!PI * self.sigma^2)
      ENDFOR
   ENDFOR

   ; Normalize the mask so that Total(mask) = 1.
   mask = mask / Total(mask)

   ; Apply the mask to the image.
   blurred = Convol(Double(image), mask, Center=1, /Edge_Wrap)
   blurred = blurred - Min(blurred)
   blurred = blurred * (Max(image)/Max(blurred))

   RETURN, blurred

END



;+--------------------------------------------------------------------------
;   This method allows the user to get various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;   
; :Keywords:
;     alpha: out, optional, type=float, default=0.10
;        The elasticity parameter of the active contour. It reflects the contour's
;        ability to stretch along its length. 
;     beta: out, optional, type=float, default=0.25
;        The rigidity parameter of the active contour. It reflects the contour's
;        ability to bend, as, for example, around corners.
;     blur: out, optional, type=boolean, default=1
;        Set this keyword to 1 if you want a Gaussian Blur applied to image before
;        processing. Set it to 0 otherwise. 
;     color: out, optional, type=string, default="red"
;        The name of a color for the snake. See the documentation for cgColor for a
;        list of possible color names.
;     delta_max: out, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: out, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
;     drawid: out, optional, type=long
;        The draw widget identifier where the image is displayed. The image CANNOT
;        be displayed in a normal IDL graphics window. The event handler for this
;        draw widget will be changed for snake processing, and then returned to its
;        original state when the Active Contour control panel is destroyed. If
;        this parameter is not supplied or is invalid, the program will create
;        its own display window.
;     gamma: out, optional, type=float, default=1.0
;        The viscosity parameter. Larger values make it harder to deform the active
;        contour in space. 
;     gradientscale: out, optional, type=float, default=1.75
;        A multiplication factor for the gradient calculations. 
;     kappa: out, optional, type=float, default=1.25
;        The external force weight. 
;     gvf_iterations: out, optional, type=integer, default=30
;        The number of iterations for calculating the Gradient Vector Flow (GVF).
;     iterations: out, optional, type=integer, default=120
;        The number of iterations to use in calculating the snake positions. 
;     max_value: out, optional, type=varies
;        The maximum value for scaling the image data for display.
;     min_value: out, optional, type=varies
;        The minimum value for scaling the image data for display.  
;     mu: out, optional, type=float, default=0.10
;        The regularization parameter. This should be set according to the amount of
;        noise in the image. Use a larger value for noisier images. 
;     notify_event: out, optional, type=string
;        The name of an event handler that should be called when users interact with
;        the object. 
;     palette: out, optional, type=byte
;        A 256x3 byte array containing the color table vectors for display of the image.
;     roi: out, optional, type=float
;        The final contour, stored as an IDLanROI object. Not available in the demo version
;        of the program.
;     scale: out, optional, type=float, default=1.0                      
;        Set this keyword to a scaling factor for the image. This keyword is set
;        automatically according to the size of the input image, unless specified
;        explicitly. Scaling only applies when the program creates it own image
;        display window (i.e., the DRAWID keyword is not used).
;     sigma: out, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring. 
;     spatial_scale: out, optional, type=double, default=1.0D
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
;     x: out, optional, type=float
;        The X points of the current active contour or snake. 
;     y: out, optional, type=float
;        The Y points of the current active contour or snake
;---------------------------------------------------------------------------
PRO ActiveContour::GetProperty, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   COLOR=color, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DRAW_CREATE=draw_create, $
   DRAWID=drawid, $
   NOTIFY_EVENT=notify_event, $
   GAMMA=gamma, $
   IMAGE=image, $
   ITERATIONS = iterations, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_v, $
   MU=mu, $
   PALETTE=palette, $
   ROI=roi, $
   SCALE=scale, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale, $
   X=x, $
   Y=y

   COMPILE_OPT idl2, Hidden

   IF Arg_Present(alpha) THEN alpha = self.alpha
   IF Arg_Present(beta) THEN beta = self.beta
   IF Arg_Present(blur) THEN blur = self.blur
   IF Arg_Present(color) THEN color = self.color
   IF Arg_Present(dmax) THEN dmax = self.dmax
   IF Arg_Present(dmin) THEN dmin = self.dmin
   IF Arg_Present(draw_create) THEN draw_create = self.draw_create
   IF Arg_Present(drawid) THEN drawid = self.drawid
   IF Arg_Present(notify_event) THEN notify_event = self.notify_event
   IF Arg_Present(gamma) THEN gamma = self.gamma
   IF Arg_Present(image) THEN image = *self.image
   IF Arg_Present(iterations) THEN iterations = self.iterations
   IF Arg_Present(kappa) THEN kappa = self.kappa
   IF Arg_Present(gradientscale) THEN gradientscale = self.gradientscale
   IF Arg_Present(gvf_iterations) THEN gvf_iterations = self.gvf_iterations
   IF Arg_Present(mu) THEN mu = self.mu
   IF Arg_Present(min_v) THEN min_v = self.min_v
   IF Arg_Present(max_v) THEN max_v = self.max_v
   IF Arg_Present(palette) THEN palette = self.palette
   IF Arg_Present(roi) && (self.demo_version EQ 0) THEN BEGIN
      IF Obj_Valid(self.roi) THEN roi = self.roi
   ENDIF
   IF Arg_Present(sigma) THEN sigma = self.sigma
   IF Arg_Present(spatial_scale) THEN spatial_scale = self.spatial_scale
   IF Arg_Present(x) OR Arg_Present(y) THEN BEGIN
      IF self.demo_version THEN BEGIN
         ok = Dialog_Message('X and/or Y keywords not allowed in demo verson of ActiveContour.')
      ENDIF ELSE BEGIN
         IF Obj_Valid(self.roi) THEN BEGIN
            self.roi -> GetProperty, Data=theROI
            x = Reform(theROI[0,*])
            y = Reform(theROI[1,*])
         ENDIF ELSE ok = Dialog_Message('There is valid current final contour.')
      ENDELSE
   ENDIF

END


;+
; This function calculates and returns the gradient of an image.
; 
; :Returns:
;     Returns the gradient image.
;     
; :Params:
;     image: in, required,
;         The image for which the gradient is to be calculated.
;     direction: in, optional, type=boolean, default=0
;         The direction the gradient is to be applied. 0 indicates
;         the X direction and 1 indicates the Y direction.
;-      
FUNCTION ActiveContour::Gradient, image, direction

   COMPILE_OPT idl2, Hidden

   direction = Keyword_Set(direction)

   IF direction EQ 0 THEN BEGIN ; Fx

      ; Calculate gradient dx.
      xkernel = [ [-1.0,  0.0,  1.0], [-2.0, 0.0, 2.0], [-1.0, 0.0, 1.0] ]
      theGradient = Convol( image, xkernel, Center=1, /Edge_Wrap )

   ENDIF ELSE BEGIN ; Fy

      ; Calculate gradient dy.
      ykernel = [ [-1.0, -2.0, -1.0], [ 0.0, 0.0, 0.0], [ 1.0, 2.0, 1.0] ]
      theGradient = Convol( image, ykernel, Center=1, /Edge_Wrap )

   ENDELSE

   RETURN, theGradient * self.gradientScale

END


;+
; This function applies and returns the Laplacian filtered image.
; 
; :Returns:
;     Returns the Laplacian image.
;     
; :Params:
;     image: in, required,
;         The image for which the Laplacian filter is to be applied.
;-      
FUNCTION ActiveContour::Laplacian, image

   COMPILE_OPT idl2, Hidden

   kernel = Fltarr(3,3)
   kernel[*,1] = -1.0
   kernel[1,*] = -1.0
   kernel[1,1] =  4.0
   thisExcept = !Except
   !Except = 0
   filtered = Convol(Float(image) > 1e-6, kernel, Center=1, Edge_Wrap=1)
   !Except = thisExcept

   RETURN, filtered

END


;+
; This procedure resets the display window to its original condition.
;-      
PRO ActiveContour::ResetDisplay

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Did you create the draw widget? Resetting the display is only
   ; necessary if we didn't create the draw widget.
   IF self.draw_create EQ 0 THEN BEGIN

      ; Put everything back the way you found it.
      Widget_Control, self.drawID, /Clear_Events
      IF Widget_Info(self.drawID, /Valid_ID) THEN BEGIN
         Widget_Control, self.drawID, $
            Tracking_Events=self.draw_events[0], $
            Draw_Button_Events=self.draw_events[1], $
            Draw_Expose_Events=self.draw_events[2], $
            Draw_Keyboard_Events=self.draw_events[3], $
            Draw_Motion_Events=self.draw_events[4], $
            Draw_Viewport_Events=self.draw_events[5], $
            Event_Pro=self.draw_eh_pro, $
            Event_Func=self.draw_eh_func
         IF Ptr_Valid(self.draw_uvalue) THEN BEGIN
            Widget_Control, self.drawID, Set_UValue=*self.draw_uvalue
            Ptr_Free, self.draw_uvalue
         ENDIF
      ENDIF
   ENDIF

END


;+
; This procedure sends an event to the specified event handler.
; 
; :Keywords:
;    data: in, optional, type=varies
;       Set this keyword to an IDL variable that will be sent as the "data"
;       along with the event.
;    type: in, optional, type=string, default="POINTS_COLLECTED"
;       The type of event to send.
;-      
PRO ActiveContour::SendEvent, DATA=eventData, TYPE=type

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; This cannot be a demo version of the software.
   IF self.demo_version THEN RETURN

   ; Must have event handler to notify to proceed.
   IF self.notify_event EQ "" THEN RETURN

   IF N_Elements(type) EQ 0 THEN type = "POINTS_COLLECTED" ELSE type = StrUpCase(type)

   ; Get the ID of the TLB.
   parent = self.drawID
   WHILE Widget_Info(parent, /Parent) NE 0 DO parent = Widget_Info(parent, /Parent)

   ; Send the event.
   IF N_Elements(eventData) NE 0 THEN BEGIN
      theEvent = {ID:self.drawID, TOP:parent, HANDLER:0L, TYPE:type, DATA:eventData}
   ENDIF ELSE BEGIN
      theEvent = {ID:self.drawID, TOP:parent, HANDLER:0L, TYPE:type}
   ENDELSE
   Call_Procedure, self.notify_event, theEvent

END


;+
; This procedure sets up the display window to respond to active contour events.
;-      
PRO ActiveContour::SetDisplay

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(tlb) NE 0 THEN Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Do we need to create a draw widget?
   IF Widget_Info(self.drawID, /Valid_ID) EQ 0 THEN BEGIN

      tlb = Widget_Base(Title='Active Contour Image', XOffset=50, $
         YOffset=50)
      self.drawID = Widget_Draw(tlb, XSize=self.xsize, YSize=self.ysize, $
         Event_Pro='ACTIVECONTOUR_WIDGETEVENTS', $
         UValue={object:self, action:'DRAW_WIDGET_EVENTS'})

      ; Set create_draw flag.
      self.draw_create = 1

      ; Put the draw widget in the correct state.
      Widget_Control, self.drawID, Tracking_Events=0, Draw_Button_Events=1, $
         Draw_Expose_Events=1, Draw_Keyboard_Events=0, Draw_Motion_Events=1, $
         Draw_Viewport_Events=0, Event_Pro='ACTIVECONTOUR_WIDGETEVENTS', Event_Func="", $
         Set_UValue={object:self, action:'DRAW_WIDGET_EVENTS'}

      Widget_Control, tlb, /Realize

      ; Get the window index number.
      Widget_Control, self.drawID, Get_Value=wid
      self.wid = wid

      ; Create a pixmap window.
      Window, XSize=self.xsize, YSize=self.ysize, /Pixmap, /Free
      self.pixmap = !D.Window

      ; Draw the image in the window.
      Device, Decomposed=1
      WSet, self.wid
      TVLCT, self.palette
      cgImage, *self.image, /Interp, /Save, XRange=self.xrange, YRange=self.yrange
      WSet, self.pixmap
      Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
      WSet, self.wid
      
      ; Manage it.
      XManager, 'drawing window', tlb, /No_Block, /Just_Reg

   ENDIF ELSE BEGIN ; Just set up the already-existing draw widget to respond to our events.

      ; Gather information from the widget. What state is it in?
      self.draw_events[0] = Widget_Info(self.drawID, /Tracking_Events)
      self.draw_events[1] = Widget_Info(self.drawID, /Draw_Button_Events)
      self.draw_events[2] = Widget_Info(self.drawID, /Draw_Expose_Events)
      self.draw_events[3] = Widget_Info(self.drawID, /Draw_Keyboard_Events)
      self.draw_events[4] = Widget_Info(self.drawID, /Draw_Motion_Events)
      self.draw_events[5] = Widget_Info(self.drawID, /Draw_Viewport_Events)
      self.draw_eh_pro = Widget_Info(self.drawID, /Event_Pro)
      self.draw_eh_func = Widget_Info(self.drawID, /Event_Func)
      Widget_Control, self.drawID, Get_UValue=stuff
      IF N_Elements(stuff) NE 0 THEN self.draw_uvalue = Ptr_New(stuff)

      ; Put it in the correct state.
      Widget_Control, self.drawID, Tracking_Events=0, Draw_Button_Events=1, $
         Draw_Expose_Events=0, Draw_Keyboard_Events=0, Draw_Motion_Events=1, $
         Draw_Viewport_Events=0, Event_Pro='ACTIVECONTOUR_WIDGETEVENTS', Event_Func="", $
         Set_UValue={object:self, action:'DRAW_WIDGET_EVENTS'}

      ; Get the window index number.
      Widget_Control, self.drawID, Get_Value=wid
      self.wid = wid
      Device, Decomposed=1

      ; Create a pixmap window.
      IF self.pixmap GE 0 THEN WDelete, self.pixmap
      WSet, self.wid
      Window, XSize=!D.X_Size, YSize=!D.Y_Size, /Free, /Pixmap
      self.pixmap = !D.Window

      ; Copy the window to the pixmap.
      Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]

      ; Set create_draw flag.
      self.draw_create = 0 
   ENDELSE

   ; If you have an initial ROI, draw it now. Assume the initial ROI values are in the coordinate system of the image (pixels).
   ; These should be converted into the coordinate system of the current graphics window (device coordinates).
   IF self.initialROI THEN BEGIN
    
        
        ; Convert image pixel coordinates to normalized coordinates.
        dims = Size(*self.image, /Dimensions)
        norm_xsize = (*self.xsnake_f) / Float(dims[0])
        norm_ysize = (*self.ysnake_f) / Float(dims[1])
        WSet, self.wid
        xy = Convert_Coord(norm_xsize, norm_ysize, /Normal, /To_Device)
        *self.xsnake_f = Reform(xy[0,*])
        *self.ysnake_f = Reform(xy[1,*])
        
        ; Draw the initial contour.
        cgPLOTS, *self.xsnake_f, *self.ysnake_f, /Device, Color=self.color, Thick=2
    
       self.initialROI = 0
   ENDIF
END



;+--------------------------------------------------------------------------
;   This method allows the user to set various properties of the object. In general,
;   the same keywords that are used for the INIT method can be used here.
;   
; :Keywords:
;     alpha: in, optional, type=float, default=0.10
;        The elasticity parameter of the active contour. It reflects the contour's
;        ability to stretch along its length. 
;     beta: in, optional, type=float, default=0.25
;        The rigidity parameter of the active contour. It reflects the contour's
;        ability to bend, as, for example, around corners.
;     blur: in, optional, type=boolean, default=1
;        Set this keyword to 1 if you want a Gaussian Blur applied to image before
;        processing. Set it to 0 otherwise. 
;     color: in, optional, type=string, default="red"
;        The name of a color for the snake. See the documentation for cgColor for a
;        list of possible color names.
;     delta_max: in, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: in, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
;     drawid: in, optional, type=long
;        The draw widget identifier where the image is displayed. The image CANNOT
;        be displayed in a normal IDL graphics window. The event handler for this
;        draw widget will be changed for snake processing, and then returned to its
;        original state when the Active Contour control panel is destroyed. If
;        this parameter is not supplied or is invalid, the program will create
;        its own display window.
;     gamma: in, optional, type=float, default=1.0
;        The viscosity parameter. Larger values make it harder to deform the active
;        contour in space. 
;     gradientscale: in, optional, type=float, default=1.75
;        A multiplication factor for the gradient calculations. 
;     image: in, optional
;        A 2D image array.
;     kappa: in, optional, type=float, default=1.25
;        The external force weight. 
;     gvf_iterations: in, optional, type=integer, default=30
;        The number of iterations for calculating the Gradient Vector Flow (GVF).
;     iterations: in, optional, type=integer, default=120
;        The number of iterations to use in calculating the snake positions. 
;     max_value: in, optional, type=varies
;        The maximum value for scaling the image data for display.
;     min_value: in, optional, type=varies
;        The minimum value for scaling the image data for display.  
;     mu: in, optional, type=float, default=0.10
;        The regularization parameter. This should be set according to the amount of
;        noise in the image. Use a larger value for noisier images. 
;     palette: in, optional, type=byte
;        A 256x3 byte array containing the color table vectors for display of the image.
;     notify_event: in, optional, type=string
;        The name of an event handler that should be called when users interact with
;        the object. 
;     sigma: in, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring. 
;     spatial_scale: in, optional, type=double, default=1.0D
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
;     x: in, optional, type=float
;        The initial X points of the active contour or snake. Optional.
;        Must be used with Y.
;     y: in, optional, type=float
;        The initial Y points of the active contour or snake. Optional.
;        Must be used with X.
;---------------------------------------------------------------------------
PRO ActiveContour::SetProperty, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   COLOR=color, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DRAWID=drawid, $
   NOTIFY_EVENT=notify_event, $
   GAMMA=gamma, $
   IMAGE=image, $
   ITERATIONS = iterations, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_V, $
   MU=mu, $
   PALETTE=palette, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale, $
   X=x, $
   Y=y

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   IF N_Elements(alpha) NE 0 THEN BEGIN
      self.alpha = alpha
      IF Obj_Valid(self.alphaObj) THEN self.alphaObj -> Set_Value, self.alpha
   ENDIF

   IF N_Elements(beta) NE 0 THEN BEGIN
      self.beta = beta
      IF Obj_Valid(self.betaObj) THEN self.betaObj -> Set_Value, self.beta
   ENDIF

   IF N_Elements(blur) NE 0 THEN BEGIN
      self.blur = Keyword_Set(blur)
      IF Widget_Info(self.gaussOn, /Valid_ID) THEN BEGIN
         CASE self.blur OF
            0: Widget_Control, self.gaussOff, Set_Button=1
            1: Widget_Control, self.gaussOn, Set_Button=1
         ENDCASE
      ENDIF
   ENDIF

   IF N_Elements(color) NE 0 THEN self.color = color

   IF N_Elements(dmax) NE 0 THEN BEGIN
      self.dmax = dmax
      IF Obj_Valid(self.dmaxObj) THEN self.dmaxObj -> Set_Value, self.dmax
   ENDIF

   IF N_Elements(dmin) NE 0 THEN BEGIN
      self.dmin = dmin
      IF Obj_Valid(self.dminObj) THEN self.dminObj -> Set_Value, self.dmin
   ENDIF

   IF N_Elements(drawID) NE 0 THEN BEGIN

      IF Widget_Info(drawID, /Valid_ID) EQ 0 THEN Message, 'Draw ID is invalid.'

      IF self.demo_version THEN BEGIN
         ok = Dialog_Message('The DRAWID keyword is not allowed in the demo version of ActiveContour.')
         RETURN
      ENDIF

      self.drawID = drawID
      self -> ResetDisplay
      self -> SetDisplay
   ENDIF

   IF N_Elements(gamma) NE 0 THEN BEGIN
      self.gamma = gamma
      IF Obj_Valid(self.gammaObj) THEN self.gammaObj -> Set_Value, self.gamma
   ENDIF

   IF N_Elements(gradientScale) NE 0 THEN BEGIN
      self.gradientScale = gradientScale
      IF Obj_Valid(self.gscaleObj) THEN self.gscaleObj -> Set_Value, self.gradientScale
   ENDIF

   IF N_Elements(gvf_iterations) NE 0 THEN BEGIN
      self.gvf_iterations = gvf_iterations
      IF Obj_Valid(self.gvfiterObj) THEN self.gvfiterObj -> Set_Value, self.gvf_iterations
   ENDIF

   needscaling = 0
   IF N_Elements(min_v) NE 0 THEN BEGIN
        self.min_v = min_v
        needscaling = 1
   ENDIF
   IF N_Elements(max_v) NE 0 THEN BEGIN
        self.max_v = max_v
        needscaling = 1
   ENDIF

   IF (N_Elements(image) NE 0) AND (self.demo_version EQ 0) THEN BEGIN

      ; Check positional parameters.
      ndims = Size(image, /N_Dimensions)
      IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'

      ; Update image pointers.
      *self.original = image
      *self.image = ClipScl(image)

      ; The window aspect must be the same as the image aspect. Maximum size
      ; of the window is 800 pixels.
      s = Size(image, /Dimensions)
      aspectRatio = Float(s[1]) / s[0]
      IF aspectRatio GE 1 THEN BEGIN
            ysize = (400 > s[1] < 800)
            xsize =  ysize / aspectRatio
      ENDIF ELSE BEGIN
            xsize = (400 > s[0] < 800)
            ysize = xsize * aspectRatio
      ENDELSE
      self.xsize = xsize
      self.ysize = ysize
      Widget_Control, self.drawID, DRAW_XSIZE=xsize, DRAW_YSIZE=ysize
      WDelete, self.pixmap
      Window, XSIZE=xsize, YSIZE=ysize, /Pixmap
      self.pixmap = !D.Window

      ; Draw the image in the window.
      Device, Decomposed=1
      WSet, self.wid
      TVLCT, self.palette
      cgImage, *self.image, /Interp, /Save, XRange=self.xrange, YRange=self.yrange
      WSet, self.pixmap
      Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
      WSet, self.wid
   ENDIF ELSE BEGIN
      IF (N_Elements(image) NE 0) AND (self.demo_version) THEN $
         ok = Dialog_Message('IMAGE keyword not allowed in the demo version of ActiveContour.')
   ENDELSE
   IF needscaling THEN BEGIN
   
       ; Scale the image, but make sure it stays a float.
       *self.image = Float(Scale_Vector(*self.original, MIN=self.min_v, MAX=self.max_v, /NAN, 0, 255))
       
       ; Draw the scaled image in the window.
       Device, Decomposed=1
       WSet, self.wid
       TVLCT, self.palette
       cgImage, *self.image, /Interp, /Save, XRange=self.xrange, YRange=self.yrange
       WSet, self.pixmap
       Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
       WSet, self.wid
   ENDIF

   IF N_Elements(iterations) NE 0 THEN BEGIN
      self.iterations = iterations
      IF Obj_Valid(self.iterObj) THEN self.iterObj -> Set_Value, self.iterations
   ENDIF

   IF N_Elements(kappa) NE 0 THEN BEGIN
      self.kappa = kappa
      IF Obj_Valid(self.kappaObj) THEN self.kappaObj -> Set_Value, self.kappa
   ENDIF

   IF N_Elements(mu) NE 0 THEN BEGIN
      self.mu = mu
      IF Obj_Valid(self.muObj) THEN self.muObj -> Set_Value, self.mu
   ENDIF

   IF N_Elements(notify_event) NE 0 THEN self.notify_event = notify_event

   IF N_Elements(palette) NE 0 THEN self.palette = palette

   IF N_Elements(sigma) NE 0 THEN BEGIN
      self.sigma = sigma
      IF Obj_Valid(self.sigmaObj) THEN self.sigmaObj -> Set_Value, self.sigma
   ENDIF

   IF N_Elements(spatial_scale) NE 0 THEN BEGIN
      IF self.demo_version THEN BEGIN
         ok = Dialog_Message('The SPATIAL_SCALE keyword is not allowed in the demo version of ActiveContour.')
         RETURN
      ENDIF
      IF N_Elements(spatial_scale) EQ 1 THEN spatial_scale = [spatial_scale, spatial_scale]
      IF N_Elements(spatial_scale) NE 2 THEN Message, 'SPATIAL_SCALE keyword must be two-element array.'
   ENDIF

   IF (N_Elements(x) NE 0) OR (N_Elements(y) NE 0) THEN BEGIN
      IF Ptr_Valid(self.xsnake_f) THEN *self.xsnake_f = x ELSE self.xsnake_f = Ptr_New(x)
      IF Ptr_Valid(self.ysnake_f) THEN *self.ysnake_f = y ELSE self.ysnake_f = Ptr_New(y)
      WSet, self.wid
      Device, Decomposed=1
      cgPLOTS, *self.xsnake_f, *self.ysnake_f, $
          /Device, Color=self.color, Thick=2
   ENDIF


END


;+
; This function deforms the snake according to the pressures applied by the parameters.
; The method was adapted for IDL from Xu and Prince's MatLab program for Active Contouring,
; available on their web page: http://iacl.ece.jhu.edu/projects/gvf/.
; 
; :Returns:
;     Returns an [n,2] array of deformed data points.
;     
; :Params:
;     x: in, required, type=float
;        The X points of the current snake.
;     y: in, required, type=float
;        The Y points of the current snake.
;-      
FUNCTION ActiveContour::SnakeDeform, x, y

   COMPILE_OPT idl2, Hidden

   npts = N_Elements(x)

   alpha = self.alpha * Replicate(1,npts)
   beta = self.beta * Replicate(1,npts)

   a = beta
   b = -alpha - (2*beta) - (2*beta)
   c = (2*alpha) + (6*beta)
   d = -alpha -(4*beta)
   e = beta

   ARRAY = Diag_Matrix(a[0:npts-3], 2) + Diag_Matrix(a[npts-2:npts-1], -(npts-2))
   ARRAY = ARRAY + Diag_Matrix(b[0:npts-2], 1) + Diag_Matrix(b[npts-1], -(npts-1))
   ARRAY = ARRAY + Diag_Matrix(c)
   ARRAY = ARRAY + Diag_Matrix(d[0:npts-2], -1) + Diag_Matrix(d[npts-1], (npts-1))
   ARRAY = ARRAY + Diag_Matrix(e[0:npts-3], -2) + Diag_Matrix(e[npts-2:npts-1], (npts-2))
   invARRAY = Invert(ARRAY + (self.gamma * Diag_Matrix(Replicate(1,npts))) )

   ;Perform deformation five times to get enough deformation.
   FOR j=1,5 DO BEGIN

      vfx = Interpolate(*self.fx, x, y)
      vfy = Interpolate(*self.fy, x, y)

      ; Deform the snake.
      x = invARRAY ## (self.gamma*x + self.kappa*vfx)
      y = invARRAY ## (self.gamma*y + self.kappa*vfy)

   ENDFOR

   ; Return [n,2] array of points.
   RETURN, [[Transpose(x)],[Transpose(y)]]
END


;+
; This function interpolates the snake adaptively. If the distance between
; two adjacent points is greater than self.dmax, then a point is inserted
; between them. If the distance between two adjacent points is less than
; self.dmin, then either point i or point i+1 is removed.
; 
; :Returns:
;     Returns an [n,2] array of interpolated data points.
;     
; :Params:
;     pts: in, required, type=float
;        An [n,2] array containing the current active contour points.
;-      
FUNCTION ActiveContour::SnakeInterpolate, pts

   COMPILE_OPT idl2, Hidden

   On_Error, 2

   ; Do you have the right arrangement of points?
   IF N_Elements(pts) EQ 0 THEN Message, 'Array of points must be passed as an argument.'
   IF (Size(pts, /Dimensions))[1] NE 2 THEN Message, 'PTS array must be a [n,2] array.'
   x = pts[*,0]
   y = pts[*,1]

   ; Calculate the distance from one point to the next.
   d = SQRT( (x - Shift(x, 1))^2 + (y - Shift(y, 1))^2 )

   ; Where is distance less than self.dmin?
   i = Where((d LT self.dmin) EQ 0, count)
   IF count GT 0 THEN BEGIN
      x = x[i]
      y = y[i]

      ; Calculate the distance from one point to the next with new array.
      d = SQRT( (x - Shift(x, 1))^2 + (y - Shift(y, 1))^2 )
   ENDIF

   ; Do you need to insert a point?
   index = Where((d GT self.dmax) EQ 1, count)
   IF count GT 0 THEN BEGIN
      index = Reverse(index)
      n = N_Elements(d)
      FOR j=0,N_Elements(index)-1 DO BEGIN
         CASE index[j] OF
            0: BEGIN
                  newX = ((x[n-1] - x[0]) / 2) + x[0]
                  newY = ((y[n-1] - y[0]) / 2) + y[0]
                  x = [newX, Temporary(x)]
                  y = [newY, Temporary(y)]
               END
            ELSE: BEGIN
                  newX = [ (( x[index[j]-1] - x[index[j]] ) / 2) + x[index[j]] ]
                  newY = [ (( y[index[j]-1] - y[index[j]] ) / 2) + y[index[j]] ]
                  x = [ x[0:index[j]-1], newX, x[index[j]:*] ]
                  y = [ y[0:index[j]-1], newY, y[index[j]:*] ]
               END
         ENDCASE
      ENDFOR

      ; Calculate the distance from one point to the next with new array.
      d = SQRT( (x - Shift(x, 1))^2 + (y - Shift(y, 1))^2 )
   ENDIF

   ; Loop until no more points fail the distance check.
   WHILE (Max(d) GT self.dmax) DO BEGIN

      ; Do you need to insert a point?
      index = Where((d GT self.dmax) EQ 1, count)
      IF count GT 0 THEN BEGIN
         index = Reverse(index)
         n = N_Elements(d)
         FOR j=0,N_Elements(index)-1 DO BEGIN
            CASE index[j] OF
               0: BEGIN
                     newX = ((x[n-1] - x[0]) / 2) + x[0]
                     newY = ((y[n-1] - y[0]) / 2) + y[0]
                     x = [newX, Temporary(x)]
                     y = [newY, Temporary(y)]
                  END
               ELSE: BEGIN
                     newX = [ (( x[index[j]-1] - x[index[j]] ) / 2) + x[index[j]] ]
                     newY = [ (( y[index[j]-1] - y[index[j]] ) / 2) + y[index[j]] ]
                     x = [ x[0:index[j]-1], newX, x[index[j]:*] ]
                     y = [ y[0:index[j]-1], newY, y[index[j]:*] ]
                  END
            ENDCASE
         ENDFOR

         ; Calculate the distance from one point to the next with new array.
         d = SQRT( (x - Shift(x, 1))^2 + (y - Shift(y, 1))^2 )
      ENDIF

   ENDWHILE

   RETURN, [[x],[y]]

END


;+
; This methods updates the thresholded image.
; 
; :Params:
;     min_t: in, required, type=float
;        The minimum threshold value.
;     max_t: in, required, type=float
;        The maximum threshold value.
;-      
PRO ActiveContour::Threshold, min_t, max_t

   COMPILE_OPT idl2, Hidden

   ; Draw the image in the window.
   *self.image = BytScl(*self.original, Min=min_t, Max=max_t)
   Device, Decomposed=1
   WSet, self.wid
   TVLCT, self.palette
   cgImage, *self.image, /Save, /Interp, XRange=self.xrange, YRange=self.yrange
   WSet, self.pixmap
   Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
   WSet, self.wid

   ; Draw the initial snake, if you have one.
   IF self.npts GT 0 THEN $
      cgPLOTS, *self.xsnake, *self.ysnake, /Device, Color=self.color, Thick=2

END


;+
; This methods updates the image on the display
;-      
PRO ActiveContour::UpdateImage

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Clear the window.
   WSet, self.wid

   ; Redisplay the image.
   Device, Decomposed=1
   TVLCT, self.palette
   cgImage, *self.image, /Interp, /Save, XRange=self.xrange, YRange=self.yrange

   ; Update the pixmap.
   WSet, self.pixmap
   Device, Copy=[0, 0, !D.X_Size, !D.Y_Size, 0, 0, self.wid]
   WSet, self.wid

   ; Draw the contour if there is one.
   IF Ptr_Valid(self.xsnake_f) THEN cgPLOTS, *self.xsnake_f, *self.ysnake_f, /Device, Color=self.color, Thick=2
   
   ; Calculate a new edgemap.
   self -> Edgemap
END


;+
; This methods reads the parameters from the controls panel, if there is one.
;-      
PRO ActiveContour::UpdateParameters

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN
   ENDIF

   ; Make sure its here.
   IF Obj_Valid(self.alphaObj) EQ 0 THEN RETURN

   ; Gather info from control panel and update object.
   IF N_Elements(self.alphaObj -> Get_Value()) NE 0 THEN $
      self.alpha = self.alphaObj -> Get_Value() ELSE $
      self.alphaObj -> Set_Value, self.alpha

   IF N_Elements(self.betaObj -> Get_Value()) NE 0 THEN $
      self.beta = self.betaObj -> Get_Value() ELSE $
      self.betaObj -> Set_Value, self.beta

   IF N_Elements(self.gammaObj -> Get_Value()) NE 0 THEN $
      self.gamma = self.gammaObj -> Get_Value() ELSE $
      self.gammaObj -> Set_Value, self.gamma

   IF N_Elements(self.gscaleObj -> Get_Value()) NE 0 THEN $
      self.gradientScale = self.gscaleObj -> Get_Value() ELSE $
      self.gscaleObj -> Set_Value, self.gradientScale

   IF N_Elements(self.kappaObj -> Get_Value()) NE 0 THEN $
      self.kappa = self.kappaObj -> Get_Value() ELSE $
      self.kappaObj -> Set_Value, self.kappa

   IF N_Elements(self.dminObj -> Get_Value()) NE 0 THEN $
      self.dmin = self.dminObj -> Get_Value() ELSE $
      self.dminObj -> Set_Value, self.dmin

   IF N_Elements(self.dmaxObj -> Get_Value()) NE 0 THEN $
      self.dmax = self.dmaxObj -> Get_Value() ELSE $
      self.dmaxObj -> Set_Value, self.dmax

   IF N_Elements(self.muObj -> Get_Value()) NE 0 THEN $
      self.mu = self.muObj -> Get_Value() ELSE $
      self.muObj -> Set_Value, self.mu

   IF N_Elements(self.gvfiterObj -> Get_Value()) NE 0 THEN BEGIN

         theValue = self.gvfiterObj -> Get_Value()
         IF theValue NE self.gvf_iterations THEN BEGIN
            Ptr_Free, self.u
            Ptr_Free, self.v
         ENDIF
         self.gvf_iterations = theValue

   ENDIF ELSE BEGIN

         self.gvfiterObj -> Set_Value, self.gvf_iterations

   ENDELSE

   IF N_Elements(self.iterObj -> Get_Value()) NE 0 THEN $
      self.iterations = self.iterObj -> Get_Value() ELSE $
      self.iterObj -> Set_Value, self.iterations

   self.blur = Widget_Info(self.gaussOn, /Button_Set)

   IF N_Elements(self.sigmaObj -> Get_Value()) NE 0 THEN $
      self.sigma = self.sigmaObj -> Get_Value() ELSE $
      self.sigmaObj -> Set_Value, self.sigma

END



;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object. Anything that may leak 
;   memory is cleaned up or destroyed here.
;---------------------------------------------------------------------------
PRO ActiveContour::CLEANUP

   Ptr_Free, self.image
   Ptr_Free, self.xsnake
   Ptr_Free, self.ysnake
   Ptr_Free, self.xsnake_f
   Ptr_Free, self.ysnake_f
   Ptr_Free, self.draw_uvalue
   Ptr_Free, self.edgemap
   Ptr_Free, self.fx
   Ptr_Free, self.fy
   Ptr_Free, self.original
   Ptr_Free, self.u
   Ptr_Free, self.v
   IF Ptr_Valid(self.undo) THEN BEGIN
      ptrs = *self.undo
      Ptr_Free, ptrs
      Ptr_Free, self.undo
   ENDIF
   Obj_Destroy, self.roi
   IF self.pixmap GT 0 THEN WDelete, self.pixmap

END


;+--------------------------------------------------------------------------
;  This program allows the user to experiment with a Gradient Vector Flow
;  active contour algorithm, as described by Chenyang Xu and Jerry L. Prince
;  in "Snakes, Shapes, and Gradient Vector Flow" in the March 1998 IEEE
;  Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's
;  web page: http://iacl.ece.jhu.edu/projects/gvf/.
;
; :Params:
;     image: in, optional
;        The image for which the active contour (snake) will be applied.
;        This argument must be 2D. The user will be asked to select an
;        image file if this argument is not provided.
;     x: in, optional, type=float
;        The initial X points of the active contour or snake. Optional.
;        Must be used with Y. Assume values are pixel locations within image.
;     y: in, optional, type=float
;        The initial Y points of the active contour or snake. Optional.
;        Must be used with X. Assume values are pixel locations within image.
;       
; :Keywords:
;     alpha: in, optional, type=float, default=0.10
;        The elasticity parameter of the active contour. It reflects the contour's
;        ability to stretch along its length. 
;     beta: in, optional, type=float, default=0.25
;        The rigidity parameter of the active contour. It reflects the contour's
;        ability to bend, as, for example, around corners.
;     blur: in, optional, type=boolean, default=1
;        Set this keyword to 1 if you want a Gaussian Blur applied to image before
;        processing. Set it to 0 otherwise. 
;     color: in, optional, type=string, default="red"
;        The name of a color for the snake. See the documentation for cgColor for a
;        list of possible color names.
;     delta_max: in, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: in, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
;     drawid: in, optional, type=long
;        The draw widget identifier where the image is displayed. The image CANNOT
;        be displayed in a normal IDL graphics window. The event handler for this
;        draw widget will be changed for snake processing, and then returned to its
;        original state when the Active Contour control panel is destroyed. If
;        this parameter is not supplied or is invalid, the program will create
;        its own display window.
;     gamma: in, optional, type=float, default=1.0
;        The viscosity parameter. Larger values make it harder to deform the active
;        contour in space. 
;     gradientscale: in, optional, type=float, default=1.75
;        A multiplication factor for the gradient calculations. 
;     kappa: in, optional, type=float, default=1.25
;        The external force weight. 
;     gvf_iterations: in, optional, type=integer, default=30
;        The number of iterations for calculating the Gradient Vector Flow (GVF).
;     iterations: in, optional, type=integer, default=120
;        The number of iterations to use in calculating the snake positions. 
;     max_value: in, optional, type=varies
;        The maximum value for scaling the image data for display.
;     min_value: in, optional, type=varies
;        The minimum value for scaling the image data for display.  
;     mu: in, optional, type=float, default=0.10
;        The regularization parameter. This should be set according to the amount of
;        noise in the image. Use a larger value for noisier images. 
;     palette: in, optional, type=byte
;        A 256x3 byte array containing the color table vectors for display of the image.
;     notify_event: in, optional, type=string
;        The name of an event handler that should be called when users interact with
;        the object. Events are sent when the initial contour points are selected, and
;        when the active contour controls are destroyed. The event structure is an anonymous
;        structure defined like this:
;
;             event = {ID: 0L, TOP: 0L, HANDLER: 0L, TYPE: "", DATA: roiStruct)
;
;         Where ID is the identifier of the draw widget where the initial contour is drawn,
;         TOP is the widget at the top of that draw widget's hierarchy, HANDLER is filled
;         out by the window manager, TYPE is set to "POINTS_COLLECTED", "CONTROLS_KILLED" 
;         or "ROI_COMPLETED" to indicate the type of event. DATA is only available
;         if the TYPE is ROI_COMPLETED. DATA will be a structure that is identical to
;         the structure returned from the ApplyGVFSnake method:
;                       
;               roiStruct = { NPTS:N_Elements(x), X:x, Y:x, $
;                             PERIMETER:perimeter, AREA:area, $
;                             VALUES:(*self.original)[x,y]}
;
;         The X and Y fields are set to the floating point pixel locations of the
;         ROI in the image device coordinate system. The perimeter and area are given
;         in terms of the total pixels inside the ROI, multiplyed by the SPATIAL_SCALE
;         factors. The VALUES field contains the original image values at the locations 
;         specified in X and Y. NPTS, of course, contains the number of points in the ROI.
;     sigma: in, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring. 
;     spatial_scale: in, optional, type=double, default=1.0D
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
;---------------------------------------------------------------------------
FUNCTION ActiveContour::INIT, image, x, y, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   COLOR=color, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DISPLAY_IMAGE=display_image, $
   DRAWID=drawid, $
   GAMMA=gamma, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   ITERATIONS=iterations, $
   KAPPA=kappa, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_v, $
   NOTIFY_EVENT=notify_event, $
   MU=mu, $
   PALETTE=palette, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      RETURN, 0
   ENDIF

   ; Is this a demo version?
   self.demo_version = 0

   ; Need an image?
   IF (N_Elements(image) NE 0) AND self.demo_version THEN BEGIN
      ok = Dialog_Message('The IMAGE parameter is not allowed in the demo version of ActiveContour.')
      xx = Temporary(image)
   ENDIF
   IF (N_Elements(image) EQ 0) THEN BEGIN
      thePath = FilePath(Root_Dir=ProgramRootDir(/ONEUP), SubDir='images', '')
      image = ImageSelect(Directory=thePath, /Only2D, Cancel=cancelled, Title='Select Image to Contour', /Silent)
      IF cancelled THEN RETURN, 0
   ENDIF
   
   ; If X is used, Y must be present, too.
   IF (N_Elements(x) NE 0) AND (N_Elements(y) EQ 0) THEN Message, 'Both X and Y arguments must be present to be used.'
   IF N_Elements(x) NE 0 THEN initialROI = 1 ELSE initialROI = 0

   ; Check positional parameters.
   ndims = Size(image, /N_Dimensions)
   IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'
   s = Size(image, /Dimensions)
   
   ; The window aspect must be the same as the image aspect. Maximum size
   ; of the window is 800 pixels.
   aspectRatio = Float(s[1]) / s[0]
   IF aspectRatio GE 1 THEN BEGIN
        ysize = (400 > s[1] < 800)
        xsize =  ysize / aspectRatio
   ENDIF ELSE BEGIN
        xsize = (400 > s[0] < 800)
        ysize = xsize * aspectRatio
   ENDELSE

   IF N_Elements(x) NE 0 THEN xsnake_f = Ptr_New(x) ELSE xsnake_f = Ptr_New()
   IF N_Elements(y) NE 0 THEN ysnake_f = Ptr_New(y) ELSE ysnake_f = Ptr_New()

   ; Check keyword parameters.
   IF N_Elements(alpha) EQ 0 THEN alpha = 0.1
   IF N_Elements(beta) EQ 0 THEN beta = 0.5
   IF N_Elements(blur) EQ 0 THEN blur = 1 ELSE blur = Keyword_Set(blur)
   IF N_Elements(color) EQ 0 THEN color = 'Red'
   IF N_Elements(dmax) EQ 0 THEN dmax = 5.50
   IF N_Elements(dmin) EQ 0 THEN dmin = 0.25
   IF N_Elements(drawID) EQ 0 THEN drawID = -1L ELSE $
      IF self.demo_version THEN BEGIN
      ok = Dialog_Message('The DRAWID keyword is not allowed in the demo version of ActiveContour.')
      drawID = -1
      ENDIF
   IF N_Elements(gamma) EQ 0 THEN gamma = 4.0
   IF N_Elements(gradientscale) EQ 0 THEN gradientscale = 1.75
   IF N_Elements(kappa) EQ 0 THEN kappa = 1.25
   IF N_Elements(gvf_iterations) EQ 0 THEN gvf_iterations = 30
   IF N_Elements(mu) EQ 0 THEN mu = 0.1
   IF N_Elements(notify_event) EQ 0 THEN notify_event = ""
   IF N_Elements(sigma) EQ 0 THEN sigma = 1.0
   IF N_Elements(iterations) EQ 0 THEN iterations = 120
   IF N_Elements(palette) EQ 0 THEN BEGIN
      TVLCT, current_rgb, /GET
      LoadCT, 0, /SILENT
      TVLCT, palette, /GET
      TVLCT, current_rgb
   ENDIF
   IF N_Elements(spatial_scale) EQ 0 THEN spatial_scale=[1.0, 1.0] ELSE BEGIN
      IF self.demo_version THEN BEGIN
         ok = Dialog_Message('The SPATIAL_SCALE keyword is not allowed in the demo version of ActiveContour.')
         spatial_scale=[1.0, 1.0]
      ENDIF
      IF N_Elements(spatial_scale) EQ 1 THEN spatial_scale = [spatial_scale, spatial_scale]
      IF N_Elements(spatial_scale) NE 2 THEN Message, 'SPATIAL_SCALE keyword must be two-element array.'
   ENDELSE

   ; Load the object.
   self.alpha = alpha
   self.beta = beta
   self.blur = blur
   self.color = color
   self.dmin = dmin
   self.dmax = dmax
   self.drawid = drawid
   self.gamma = gamma
   self.gradientscale = gradientscale
   self.gvf_iterations = gvf_iterations
   self.initialROI = initialROI
   self.iterations = iterations
   self.kappa = kappa
   IF N_Elements(max_v) EQ 0 THEN self.max_v = Max(image) ELSE self.max_v = max_v
   IF N_Elements(min_v) EQ 0 THEN self.min_v = Min(image) ELSE self.min_v = min_v
   self.mu = mu
   self.original = Ptr_New(image)
   self.notify_event = notify_event
   self.palette = palette
   self.pixmap = -1L
   self.sigma = sigma
   self.selectmap = -1L
   self.spatial_scale = spatial_scale
   self.tlb_xoffset = -1L
   self.tlb_yoffset = -1L
   self.xrange = [0,s[0]]
   self.xsize = xsize
   self.ysize = ysize
   self.xsnake_f = xsnake_f
   self.yrange = [0,s[1]]
   self.ysize = ysize
   self.ysnake_f = ysnake_f
   
   ; Set up the image for display.
   IF N_Elements(display_image) EQ 0 THEN BEGIN
       IF Size(image, /TNAME) NE 'BYTE' THEN BEGIN
           self.image = Ptr_New(Float(Scale_Vector(*self.original, MIN=self.min_v, MAX=self.max_v, /NAN, 0, 255 )), /No_Copy)
       ENDIF ELSE BEGIN
           IF (N_Elements(max_v) NE 0) && (N_Elements(min_v) NE 0) THEN BEGIN
              self.image = Ptr_New(Float(Scale_Vector(*self.original, MIN=min_v, MAX=max_v, /NAN, 0, 255)), /No_Copy)
           ENDIF ELSE self.image = Ptr_New(Float(*self.original))
       ENDELSE
   ENDIF ELSE self.image = Ptr_New(Float(display_image))
   
   ; Finished.
   RETURN, 1

END


;+--------------------------------------------------------------------------
;   When the Controls TLB dies, we come here to clean up. If we created the
;   image display window, then we should destroy ourselves, since all windows
;   have now disappeared.
;       
; :Params:
;     controltlb: in, required
;         The widget identifier of the top-level base that just died.
;---------------------------------------------------------------------------
PRO ActiveContour_ControlsCleanup, controlTLB

   Widget_Control, controlTLB, Get_UValue=self
   IF Obj_Valid(self) THEN BEGIN
      self -> GetProperty, Draw_Create=draw_create
      IF draw_create THEN Obj_Destroy, self ELSE BEGIN
         self -> GetProperty, DrawID=drawID
         IF Widget_Info(drawID, /Valid_ID) THEN self -> SendEvent, TYPE='CONTROLS_KILLED'
      ENDELSE
   ENDIF

END


;+--------------------------------------------------------------------------
;   All widget events come through here first, where they are routed to the
;   appropriate event handler method.
;---------------------------------------------------------------------------
PRO ActiveContour_WidgetEvents, event

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(/Traceback)
      IF N_Elements(tlb) NE 0 THEN Widget_Control, tlb, /Destroy
      RETURN
   ENDIF

   ; Get the object reference out of the UValue.
   Widget_Control, event.ID, Get_UValue=cmd

   ; Add the action item to the event.
   event = Create_Struct(event, 'action', cmd.action)

   ; If there is a UVALUE field, add that, too.
    i = Where(Tag_Names(cmd) EQ 'UVALUE', count)
    IF count THEN event = Create_Struct(event, 'uvalue', cmd.uvalue)

   ; Call the event handler method.
   Call_Method, 'Event_Handler', cmd.object, event

END

;+--------------------------------------------------------------------------
;   This is the class definition module. Structures used to manipulate
;   map projectatum information are also created here.
;
; :Params:
;    class: out, optional, type=structure
;       Occasionally, it is useful to have an object class definition as
;       a structure variable. Using this output keyword will allow that.
;       
;---------------------------------------------------------------------------
PRO ActiveContour__Define, class

   class = { ACTIVECONTOUR, $
             INHERITS IDL_Object, $   ; A stub, unless in IDL 8.x.
             ; Object parameters.
             alpha: 0.0, $            ; Elasticity parameter (controls the snake's tension).
             beta: 0.0, $             ; Rigidity parameter (controls the snake's rigidity.
             blur: 0L, $              ; A flag that indicates the image should be Gaussian blurred.
             color: "", $             ; The color for displaying the snake.
             demo_version: 0L, $      ; A flag that indicates whether or not this is the demo version of the program.
             dmin: 0.0, $             ; The minimum distance between points on the snake.
             dmax: 0.0, $             ; The maximum distance between points on the snake.
             edgemap: Ptr_New(), $    ; The image edgemap.
             fx: Ptr_New(), $         ; The X gradient of the edgemap.
             fy: Ptr_New(), $         ; The Y gradient of the edgemap.
             draw_create: 0L, $       ; A flag that indicates we created our own draw widget.
             draw_eh_func: "", $      ; The name of the draw widget event handler function.
             draw_eh_pro: "", $       ; The name of the draw widget event handler procedure.
             draw_events: IntArr(6), $; A vector of possible draw widget events.
             draw_uvalue: Ptr_New(), $; A place to store the user value of the draw widget.
             drawid: 0L, $            ; The draw widget where the image is displayed.
             gamma: 0.0, $            ; Viscosity parameter.
             gradientScale: 0.0, $    ; The multiplication factor for gradient calculations.
             gvf_iterations: 0L, $    ; The number of iterations for the GVF field.
             initialROI: 0B, $        ; A flag that indicates if an initial ROI is provided.
             image: Ptr_New(), $      ; The image for which the snake is being calculated.
             iterations: 0L, $        ; The number of iterations for the snake to converge.
             kappa: 0.0, $            ; External force weight.
             max_v: 0.0, $            ; The maximum value of the image.
             min_v: 0.0, $            ; The minimum value of the image.
             mu: 0.0, $               ; The regularization parameter. Increase for noisy images.
             notify_event: "", $      ; The name of an event handler to call when initial and final contours are ready.
             npts: 0L, $              ; The number of points in the snake.
             original: Ptr_New(), $   ; The original image, so we can always restore.
             palette: BytArr(256,3), $; The color palette used to display the image. Gray-scale be default.
             pixmap: 0L, $            ; The pixmap window used for erasing snake iterations.
             roi: Obj_New(), $        ; The final contour, stored as an IDLanROI object.
             selectmap: 0L, $         ; The pixmap used to allow snake point selection.
             sigma: 0.0, $            ; The sigma factor for Gaussian blurring.
             spatial_scale: DblArr(2), $ ; The spatial scale factors for calculating ROI perimeter and area.
             tlb_xoffset: 0L, $       ; The X offset of the Control Panel top-level base.
             tlb_yoffset: 0L, $       ; The Y offset of the Control Panel top-level base.
             undo: Ptr_New(), $       ; An undo pointer to pointers of images.
             u: Ptr_New(), $          ; The GVF solution X gradient.
             v: Ptr_New(), $          ; The GVF solution Y gradient.
             wid: 0L, $               ; The window in which the image is displayed.
             xrange: FltArr(2), $     ; The X range of the image. [0,s[0]+1].
             xsize: 0L, $             ; The X size of the image window.
             xsnake: Ptr_New(), $     ; The initial X coordinates of the snake.
             xsnake_f: Ptr_New(), $   ; The final X coordinates of the snake.
             yrange: FltArr(2), $     ; The Y range of the image, [0,s[1]+1].
             ysize: 0L, $             ; The Y size of the image window.
             ysnake: Ptr_New(), $     ; The initial Y coordinates of the snake.
             ysnake_f: Ptr_New(), $   ; The final Y coordinates of the snake.

             ; Widget identifiers.
             alphaObj: Obj_New(), $   ; The identifier of the alpha parameter field widget.
             applyID: 0L, $           ; The identifier of the Apply Parameters button.
             areaID: 0L, $            ; The identifer of the perimenter area label widget.
             betaObj: Obj_New(), $    ; The identifier of the beta parameter field widget.
             control_TLB: 0L, $       ; The idetifier of the TLB of the Control Panel.
             dminObj: Obj_New(), $    ; The identifier of the dmin parameter field widget.
             dmaxObj: Obj_New(), $    ; The identifier of the dmax parameter field widget.
             forceWID: 0L, $          ; The identifier of the gradient force field.
             gammaObj: Obj_New(), $   ; The identifier of the gamma parameter field widget.
             gaussOn: 0L, $           ; the identifier of the Gaussian Blur ON button widget.
             gaussOff: 0L, $          ; the identifier of the Gaussian Blur OFF button widget.
             gradientWID: 0L, $       ; The identifier of the gradient window.
             gscaleObj: Obj_New(), $  ; The identifier of the gradient scale factor widget.
             gvfiterObj: Obj_New(), $ ; The identifier of the alpha gvf_iterations field widget.
             iterObj: Obj_New(), $    ; The identifier of the alpha iterations field widget.
             kappaObj: Obj_New(), $   ; The identifier of the kappa parameter field widget.
             muObj: Obj_New(), $      ; The identifier of the mu parameter field widget.
             perimeterID: 0L, $       ; The identifer of the perimenter length label widget.
             roisaveID: 0L, $         ; The identifier of the Save ROI button.
             sigmaObj: Obj_New(), $   ; The identifier of the gaussian sigma parameter field widget.
             undoID: 0L, $            ; The identifer of the UNDO button.
             valueID: 0L $            ; The identifier of the Image Value label widget.
           }

END
