; docformat = 'rst'
;
; NAME:
;  ActiveContour
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
;  `Coyote Store <http://www.idlcoyote.com/coyotestore/index.php?main_page=product_info&cPath=2&products_id=184>`.
;
; :Categories:
;    Graphics, Analysis
;    
; :Params:
;     image: in, optional
;        The image for which the active contour (snake) will be applied.
;        This argument must be 2D. The user will be asked to select an
;        image file if this argument is not provided.
;     x: in, optional, type=float
;        The initial X points of the active contour or snake. Optional.
;        Must be used with Y.
;     y: in, optional, type=float
;        The initial Y points of the active contour or snake. Optional.
;        Must be used with X.
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
;     sigma: in, optional, type=float, default=1.0
;        The standard deviation or sigma of the Gaussian function used in Gaussian
;        blurring. 
;     spatial_scale: in, optional, type=double
;        Set this keyword to a two-element array that specifies the pixel scale in
;        the X and Y directions ([xscale, yscale]). The scale factors are applied
;        when the perimeter and area calculations for the final contour is made.
;        Default is [1.0D, 1.0D].
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
;        
; :Copyright:
;    Copyright (c) 2003-2011, Fanning Software Consulting, Inc.
;---------------------------------------------------------------------------
PRO ActiveContour, image, x, y, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   COLOR=color, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DRAWID=drawid, $
   NOTIFY_EVENT=notify_event, $
   GAMMA=gamma, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_value, $
   MIN_VALUE=min_value, $
   MU=mu, $
   PALETTE=palette, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale, $
   ITERATIONS=iterations

snake = Obj_New('ActiveContour', image, x, y, $
   ALPHA=alpha, $
   BETA=beta, $
   BLUR=blur, $
   COLOR=color, $
   DELTA_MAX=dmax, $
   DELTA_MIN=dmin, $
   DRAWID=drawid, $
   NOTIFY_EVENT=notify_event, $
   GAMMA=gamma, $
   KAPPA=kappa, $
   GRADIENTSCALE=gradientscale, $
   GVF_ITERATIONS=gvf_iterations, $
   MAX_VALUE=max_value, $
   MIN_VALUE=min_value, $
   MU=mu, $
   PALETTE=palette, $
   SIGMA=sigma, $
   SPATIAL_SCALE=spatial_scale, $
   ITERATIONS=iterations)

IF Obj_Valid(snake) THEN snake -> SetDisplay
IF Obj_Valid(snake) THEN snake -> Controls

END