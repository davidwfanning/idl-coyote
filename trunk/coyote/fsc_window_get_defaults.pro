; docformat = 'rst'
;
; NAME:
;   FSC_Window_Get_Defauls
;
; PURPOSE:
;   Allows the user to get the global defaults for resizeable FSC_Window programs.
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
; :Description:
;   Allows the user to get the global defaults for resizeable FSC_Window programs.
;
; :Categories:
;    Graphics
;    
; :Params:
;    None
;       
; :Keywords:
;     background: out, optional, type=string
;         The background color of the window.
;     delay: out, optional, type=float
;          The amount of delay between command execution.
;     eraseit: out, optional, type=boolean
;         The Erase status of the window.
;     im_density: out, optional, type=integer
;         The sampling density.
;         file from PostScript outout.
;     im_options: out, optional, type=string
;         Current ImageMagick convert options.
;     im_resize: out, optional, type=integer
;         The amount PostScript output is resized.
;     im_transparent: out, optional, type=boolean
;         The transparent background setting.
;     multi: out, optional, type=Intarr(5)
;         The !P.MULTI setting for the window.
;     palette: out, optional, type=BytArr(N,3)
;         The window color palette.
;     ps_delete: out, optional, type=boolean
;         The delete PostScript file status of the window.
;     ps_encapsulated: out, optional, type=boolean
;          The PostScript encapsulated status of the window.
;     ps_metric: out, optional, type=boolean
;          The metric status of the window.
;     title: out, optional, type=boolean
;         The window title. 
;     xomargin: out, optional, type=intarr(2)
;         The !X.OMargin value for multiplots.
;     xpos: out, optional, type=integer
;         The X offset of the window from the upper-left corner of the display.
;     xsize: out, optional, type=integer
;         The starting X size of the window.
;     yomargin: out, optional, type=intarr(2)
;         The !Y.OMargin value for multiplots.
;     ypos: out, optional, type=integer
;         The Y offset of the window from the upper-left corner of the display.
;     ysize: out, optional, type=integer
;         The starting Y size of the window.
;          
; :Examples:
;    Used to get FSC_Window global properties::
;       IDL> FSC_Window_Get_Defaults, PALETTE=palette, PS_ENCAPSULATED=encap, PS_METRIC=metric
;       
; :Author:
;       FANNING SOFTWARE CONSULTING::
;           David W. Fanning 
;           1645 Sheely Drive
;           Fort Collins, CO 80526 USA
;           Phone: 970-221-0438
;           E-mail: davidf@dfanning.com
;           Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; :History:
;     Change History::
;        Written, 29 January 2011. DWF.
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
PRO FSC_Window_Get_Defaults, $
   Background = background, $                      ; The background color. 
   Delay = delay, $                                ; The amount of delay between command execution.
   EraseIt = eraseit, $                            ; Set this keyword to erase the display before executing the commands.
   Multi = multi, $                                ; Set this in the same way !P.Multi is used.   
   XOMargin = xomargin, $                          ; Set the !X.OMargin. A two element array.
   YOMargin = yomargin, $                          ; Set the !Y.OMargin. A two element array
   XSize = xsize, $                                ; The X size of the FSC_Window graphics window.
   YSize = ysize, $                                ; The Y size of the FSC_Window graphics window.
   Title = title, $                                ; The window title.
   XPos = xpos, $                                  ; The X offset of the window on the display.
   YPos = ypos, $                                  ; The Y offset of the window on the display. 
   Palette = palette, $                            ; The color table palette to use for the window.
   
   ; ImageMagick Properties.
   IM_Transparent = im_transparent, $  ; Sets the "alpha" keyword on ImageMagick convert command.
   IM_Density = im_density, $                      ; Sets the density parameter on ImageMagick convert command.
   IM_Resize = im_resize, $                        ; Sets the resize parameter on ImageMagick convert command.
   IM_Options = im_options, $                      ; Sets extra ImageMagick options on the ImageMagick convert command.
   
   ; PostScript properties.
   PS_Delete = ps_delete, $                        ; Delete the PostScript file when making IM files.
   PS_Metric = ps_metric, $                        ; Select metric measurements in PostScript output.
   PS_Encapsulated = ps_encapsulated, $            ; Create Encapsulated PostScript output.
   PS_FONT=ps_font, $                              ; Select the font for PostScript output.
   PS_CHARSIZE=ps_charsize, $                      ; Select the character size for PostScript output.
   PS_SCALE_FACTOR=ps_scale_factor, $              ; Select the scale factor for PostScript output.
   PS_TT_FONT=ps_tt_font                           ; Select the true-type font to use for PostScript output.
   
   Compile_Opt idl2
   
   ; Return to caller on error.
   ON_Error, 2
   
   ; Does the defaults structure exist? If not, create the defaults.
   DefSysV, '!FSC_WINDOW_DEFAULTS', EXISTS=exists
   IF ~exists THEN FSC_Window_Set_Defaults
   
   ; If the user asked for the default, give it to them.
   IF Arg_Present(background) THEN background = !FSC_WINDOW_DEFAULTS.background
   IF Arg_Present(delay) THEN delay = !FSC_WINDOW_DEFAULTS.delay
   IF Arg_Present(eraseit) THEN eraseit = !FSC_WINDOW_DEFAULTS.eraseit
   IF Arg_Present(multi) THEN multi = !FSC_WINDOW_DEFAULTS.multi
   IF Arg_Present(xomargin) THEN xomargin = !FSC_WINDOW_DEFAULTS.xomargin
   IF Arg_Present(yomargin) THEN yomargin = !FSC_WINDOW_DEFAULTS.yomargin
   IF Arg_Present(xsize) THEN xsize = !FSC_WINDOW_DEFAULTS.xsize
   IF Arg_Present(ysize) THEN ysize = !FSC_WINDOW_DEFAULTS.ysize
   IF Arg_Present(title) THEN title = !FSC_WINDOW_DEFAULTS.title
   IF Arg_Present(xpos) THEN xpos = !FSC_WINDOW_DEFAULTS.xpos
   IF Arg_Present(ypos) THEN ypos = !FSC_WINDOW_DEFAULTS.ypos
   IF Arg_Present(palette) THEN palette = !FSC_WINDOW_DEFAULTS.palette
   IF Arg_Present(im_transparent) THEN im_transparent = !FSC_WINDOW_DEFAULTS.im_transparent
   IF Arg_Present(im_density) THEN im_density = !FSC_WINDOW_DEFAULTS.im_density
   IF Arg_Present(im_resize) THEN im_resize = !FSC_WINDOW_DEFAULTS.im_resize
   IF Arg_Present(im_options) THEN im_options = !FSC_WINDOW_DEFAULTS.im_options
   IF Arg_Present(ps_delete) THEN ps_delete = !FSC_WINDOW_DEFAULTS.ps_delete
   IF Arg_Present(ps_metric) THEN ps_metric = !FSC_WINDOW_DEFAULTS.ps_metric
   IF Arg_Present(ps_encapsulated) THEN ps_encapsulated =!FSC_WINDOW_DEFAULTS.ps_encapsulated
   IF Arg_Present(ps_charsize) THEN ps_charsize =!FSC_WINDOW_DEFAULTS.ps_charsize
   IF Arg_Present(ps_font) THEN ps_font =!FSC_WINDOW_DEFAULTS.ps_font
   IF Arg_Present(ps_scale_factor) THEN ps_scale_factor =!FSC_WINDOW_DEFAULTS.ps_scale_factor
   IF Arg_Present(ps_tt_font) THEN ps_tt_font =!FSC_WINDOW_DEFAULTS.ps_tt_font
   
END
