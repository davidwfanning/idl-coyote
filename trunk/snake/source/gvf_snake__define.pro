; docformat = 'rst'
;
; NAME:
;  GVF_Snake
;
; PURPOSE:
;  This program applies the Gradient Vector Flow active contour algorithm, as described by 
;  Chenyang Xu and Jerry L. Prince in "Snakes, Shapes, and Gradient Vector Flow" in the March 
;  1998 IEEE Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's web page: 
;  http://iacl.ece.jhu.edu/projects/gvf/.
;
;  Active contours are often described as "snakes" because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2013, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;  This program applies the Gradient Vector Flow active contour algorithm, as described by 
;  Chenyang Xu and Jerry L. Prince in "Snakes, Shapes, and Gradient Vector Flow" in the March 
;  1998 IEEE Transactions on Image Processing, Vol. 7, No.3. Additional information,
;  including references to research papers, is available via Cheyang Xu's web page: 
;  http://iacl.ece.jhu.edu/projects/gvf/.
;
;  Active contours are often described as "snakes" because they writhe and move
;  under the influence of external and internal forces, toward a feature of interest
;  in an image, usually an edge. This program gives the user the opportunity
;  to control both external and internal forces to find an optimal set of active contour
;  parameters. Active contours are most often used with images to find and describe
;  regions of interest.
;
;  This program can be purchased at the
;  `Coyote Store <http://www.idlcoyote.com/coyotestore/index.php>`.
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
;       Written by David W. Fanning, 25 October 2013, based on ActiveContour program from 2003.
;        
; :Copyright:
;    Copyright (c) 2013, Fanning Software Consulting, Inc.
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
PRO GVF_Snake::ArcSample, x_in, y_in, x_out, y_out, $
   PHASE=phase, $
   POINTS=points 

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
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
;    
;    retValue = {NPTS:N_Elements(x), X:x, Y:y, $
;       PERIMETER:perimeter, AREA:area, $
;       VALUES:(*self.original)[Round(x),Round(y)]}
; 
; :Keywords:
;    cancel: out, optional, type=boolean, default=0
;       On exit, this value will be set to 1 if the user cancelled the
;       active contour operation.
;-      
FUNCTION GVF_Snake::ApplySnake, Cancel=cancel

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      IF Obj_Valid(progressbar) THEN progressbar -> Destroy
      void = Check_Math()
      !Except = thisExcept
      RETURN, -1
   ENDIF

   ; Turn off exception handling to avoid underflow errors. Aaauughhh!
   thisExcept = !Except
   !Except = 0

   ; Depending upon 24-bit color.
   Device, Decomposed=1
   
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

   ; Get the initial snake points. Change to 150 points on equally spaced arcs.
   self -> ArcSample, *self.xsnake, *self.ysnake, x, y, Points=150
   x = [x, x[0]]
   y = [y, y[0]]

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


   ; Make sure there is only one ROI object defined at any one time.
   IF Obj_Valid(self.roi) THEN Obj_Destroy, self.roi
   self.roi = roi

   ; Return the final contour information.
   roi = {NPTS:N_Elements(x), X:x, Y:y, $
       PERIMETER:perimeter, AREA:area, $
       VALUES:(*self.original)[Round(x),Round(y)]}
   RETURN, roi
   
END



;+
; This procedure receives stretch information from cgStretch when the image changes
; or is stretched. The image and stretch parameters are updated in the object.
; 
; :Params:
;     info: in, required, type=structure
;         The info structure returned from cgStretch when the image is stretched there.
;-      
PRO GVF_Snake::Contrast_Stretch, info

   COMPILE_OPT idl2, Hidden

    *self.image = info.image
    self.max_v = info.maxthresh
    self.min_v = info.minThresh

    self -> EdgeMap
END


;+
; This method calculates the edgemap for the object image. The edgemap is stored in 
; the pointer self.edgemap.
;-      
PRO GVF_Snake::Edgemap

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
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
FUNCTION GVF_Snake::GaussianBlur, image, Scale=scale

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
;     delta_max: out, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: out, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
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
PRO GVF_Snake::GetProperty, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DRAW_CREATE=draw_create, $
   GAMMA=gamma, $
   IMAGE=image, $
   ITERATIONS = iterations, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_v, $
   MU=mu, $
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
   IF Arg_Present(dmax) THEN dmax = self.dmax
   IF Arg_Present(dmin) THEN dmin = self.dmin
   IF Arg_Present(gamma) THEN gamma = self.gamma
   IF Arg_Present(image) THEN image = *self.image
   IF Arg_Present(iterations) THEN iterations = self.iterations
   IF Arg_Present(kappa) THEN kappa = self.kappa
   IF Arg_Present(gradientscale) THEN gradientscale = self.gradientscale
   IF Arg_Present(gvf_iterations) THEN gvf_iterations = self.gvf_iterations
   IF Arg_Present(mu) THEN mu = self.mu
   IF Arg_Present(min_v) THEN min_v = self.min_v
   IF Arg_Present(max_v) THEN max_v = self.max_v
   IF Arg_Present(roi) THEN BEGIN
      IF Obj_Valid(self.roi) THEN roi = self.roi
   ENDIF
   IF Arg_Present(sigma) THEN sigma = self.sigma
   IF Arg_Present(spatial_scale) THEN spatial_scale = self.spatial_scale
   IF Arg_Present(x) OR Arg_Present(y) THEN BEGIN
      
      IF Obj_Valid(self.roi) THEN BEGIN
         self.roi -> GetProperty, Data=theROI
         x = Reform(theROI[0,*])
         y = Reform(theROI[1,*])
      ENDIF ELSE ok = Dialog_Message('There is valid current final contour.')
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
FUNCTION GVF_Snake::Gradient, image, direction

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
FUNCTION GVF_Snake::Laplacian, image

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
;     delta_max: in, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: in, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
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
PRO GVF_Snake::SetProperty, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   GAMMA=gamma, $
   IMAGE=image, $
   ITERATIONS = iterations, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_V, $
   MU=mu, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale, $
   X=x, $
   Y=y

   COMPILE_OPT idl2, Hidden

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
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

   IF N_Elements(dmax) NE 0 THEN BEGIN
      self.dmax = dmax
      IF Obj_Valid(self.dmaxObj) THEN self.dmaxObj -> Set_Value, self.dmax
   ENDIF

   IF N_Elements(dmin) NE 0 THEN BEGIN
      self.dmin = dmin
      IF Obj_Valid(self.dminObj) THEN self.dminObj -> Set_Value, self.dmin
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

   IF N_Elements(min_v) NE 0 THEN self.min_v = min_v

   IF N_Elements(max_v) NE 0 THEN self.max_v = max_v

   IF (N_Elements(image) NE 0) THEN BEGIN

      ; Check positional parameters.
      ndims = Size(image, /N_Dimensions)
      IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'

      ; Update image pointers.
      *self.original = image
      *self.image = ClipScl(image)

      dims = Size(image, /Dimensions)
      self.xsize = dims[0]
      self.ysize = dims[1]
   
       ; Scale the image, but make sure it stays a float.
       *self.image = Scale_Vector(Float(*self.original), MIN=self.min_v, MAX=self.max_v, /NAN, 0, 255)
       
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
FUNCTION GVF_Snake::SnakeDeform, x, y

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
FUNCTION GVF_Snake::SnakeInterpolate, pts

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
PRO GVF_Snake::Threshold, min_t, max_t

   COMPILE_OPT idl2, Hidden

   ; Scale the original image to threshold values.
   *self.image = BytScl(*self.original, Min=min_t, Max=max_t)

END


;+--------------------------------------------------------------------------
;   This is the clean-up routine for the object. Anything that may leak 
;   memory is cleaned up or destroyed here.
;---------------------------------------------------------------------------
PRO GVF_Snake::CLEANUP

   Ptr_Free, self.image
   Ptr_Free, self.xsnake
   Ptr_Free, self.ysnake
   Ptr_Free, self.xsnake_f
   Ptr_Free, self.ysnake_f  
   Ptr_Free, self.edgemap
   Ptr_Free, self.fx
   Ptr_Free, self.fy
   Ptr_Free, self.original
   Ptr_Free, self.u
   Ptr_Free, self.v
   Obj_Destroy, self.roi

END


;+--------------------------------------------------------------------------
;  This program implements a Gradient Vector Flow active contour algorithm, as 
;  described by Chenyang Xu and Jerry L. Prince in "Snakes, Shapes, and Gradient 
;  Vector Flow" in the March 1998 IEEE Transactions on Image Processing, Vol. 7, 
;  No.3. Additional information, including references to research papers, is available 
;  via Cheyang Xu's web page: http://iacl.ece.jhu.edu/projects/gvf/.
;
; :Params:
;     image: in, required
;        The image for which the active contour (snake) will be applied.
;        This argument must be 2D. The user will be asked to select an
;        image file if this argument is not provided.
;     x_init: in, required, type=float
;        The initial X points of the active contour or snake. Must be paired with Y. 
;        Assume values are pixel locations within image.
;     y_init: in, required, type=float
;        The initial Y points of the active contour or snake. Must be paired with X. 
;        Assume values are pixel locations within image.
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
;     delta_max: in, optional, type=float, default=5.50
;        The maximum pixel distance for adaptive interpolation. 
;     delta_min: in, optional, type=float, default=0.25
;        The minimum pixel distance for adaptive interpolation. 
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
;     sigma: in, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring. 
;     spatial_scale: in, optional, type=double, default=1.0D
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
;---------------------------------------------------------------------------
FUNCTION GVF_Snake::INIT, image, x_init, y_init, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DISPLAY_IMAGE=display_image, $
   GAMMA=gamma, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   ITERATIONS=iterations, $
   KAPPA=kappa, $
   MAX_VALUE=max_v, $
   MIN_VALUE=min_v, $
   MU=mu, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN, 0
   ENDIF

   IF (N_Elements(image) EQ 0) THEN Message, 'Syntax: roiStruct = GVF_Snake(image, x_init, y_init)'
   
   ; If X is used, Y must be present, too.
   IF N_Elements(x_init) EQ 0 THEN Message, 'Syntax: roiStruct = GVF_Snake(image, x_init, y_init)'
   IF (N_Elements(x_init) NE 0) AND (N_Elements(y_init) EQ 0) THEN Message, 'Both X_INIT and Y_INIT arguments must be present to be used.'

   ; Check positional parameters.
   ndims = Size(image, /N_Dimensions)
   IF ndims NE 2 THEN Message, 'The image argument must be a 2D array.'
   s = Size(image, /Dimensions)
   
   ; Store initial ROI values.
   xsnake_f = Ptr_New(x_init) 
   ysnake_f = Ptr_New(y_init) 
   
   ; Check keyword parameters.
   IF N_Elements(alpha) EQ 0 THEN alpha = 0.1
   IF N_Elements(beta) EQ 0 THEN beta = 0.5
   IF N_Elements(blur) EQ 0 THEN blur = 1 ELSE blur = Keyword_Set(blur)
   IF N_Elements(dmax) EQ 0 THEN dmax = 5.50
   IF N_Elements(dmin) EQ 0 THEN dmin = 0.25
   IF N_Elements(gamma) EQ 0 THEN gamma = 4.0
   IF N_Elements(gradientscale) EQ 0 THEN gradientscale = 1.75
   IF N_Elements(kappa) EQ 0 THEN kappa = 1.25
   IF N_Elements(gvf_iterations) EQ 0 THEN gvf_iterations = 30
   IF N_Elements(mu) EQ 0 THEN mu = 0.1
   IF N_Elements(sigma) EQ 0 THEN sigma = 1.0
   IF N_Elements(iterations) EQ 0 THEN iterations = 120
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
   self.dmin = dmin
   self.dmax = dmax
   self.gamma = gamma
   self.gradientscale = gradientscale
   self.gvf_iterations = gvf_iterations
   self.iterations = iterations
   self.kappa = kappa
   IF N_Elements(max_v) EQ 0 THEN self.max_v = Max(image) ELSE self.max_v = max_v
   IF N_Elements(min_v) EQ 0 THEN self.min_v = Min(image) ELSE self.min_v = min_v
   self.mu = mu
   self.original = Ptr_New(image)
   self.sigma = sigma
   self.spatial_scale = spatial_scale
   self.xrange = [0,s[0]]
   dims = Size(image, /Dimensions)
   self.xsize = dims[0]
   self.xsnake_f = xsnake_f
   self.yrange = [0,s[1]]
   self.ysize = dims[1]
   self.ysnake_f = ysnake_f
   
   ; Scale the original image.
   IF Size(image, /TNAME) NE 'BYTE' THEN BEGIN
       self.image = Ptr_New(Float(Scale_Vector(*self.original, MIN=self.min_v, MAX=self.max_v, /NAN, 0, 255 )), /No_Copy)
   ENDIF ELSE BEGIN
       IF (N_Elements(max_v) NE 0) && (N_Elements(min_v) NE 0) THEN BEGIN
           self.image = Ptr_New(Float(Scale_Vector(*self.original, MIN=min_v, MAX=max_v, /NAN, 0, 255)), /No_Copy)
       ENDIF ELSE self.image = Ptr_New(Float(*self.original))
   ENDELSE
   
   ; Finished.
   RETURN, 1

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
PRO GVF_Snake__Define, class

   class = { GVF_Snake, $
    
             INHERITS IDL_Object, $   ; A stub, unless in IDL 8.x.
             
             ; Object parameters.
             alpha: 0.0, $            ; Elasticity parameter (controls the snake's tension).
             beta: 0.0, $             ; Rigidity parameter (controls the snake's rigidity.
             blur: 0L, $              ; A flag that indicates the image should be Gaussian blurred.
             dmin: 0.0, $             ; The minimum distance between points on the snake.
             dmax: 0.0, $             ; The maximum distance between points on the snake.
             edgemap: Ptr_New(), $    ; The image edgemap.
             fx: Ptr_New(), $         ; The X gradient of the edgemap.
             fy: Ptr_New(), $         ; The Y gradient of the edgemap.
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
             npts: 0L, $              ; The number of points in the snake.
             original: Ptr_New(), $   ; The original image, so we can always restore.
             roi: Obj_New(), $        ; The final contour, stored as an IDLanROI object.
             sigma: 0.0, $            ; The sigma factor for Gaussian blurring.
             spatial_scale: DblArr(2), $ ; The spatial scale factors for calculating ROI perimeter and area.
             u: Ptr_New(), $          ; The GVF solution X gradient.
             v: Ptr_New(), $          ; The GVF solution Y gradient.
             xrange: FltArr(2), $     ; The X range of the image. [0,s[0]+1].
             xsize: 0L, $             ; The X size of the image window.
             xsnake: Ptr_New(), $     ; The initial X coordinates of the snake.
             xsnake_f: Ptr_New(), $   ; The final X coordinates of the snake.
             yrange: FltArr(2), $     ; The Y range of the image, [0,s[1]+1].
             ysize: 0L, $             ; The Y size of the image window.
             ysnake: Ptr_New(), $     ; The initial Y coordinates of the snake.
             ysnake_f: Ptr_New() $   ; The final Y coordinates of the snake.
           }

END
