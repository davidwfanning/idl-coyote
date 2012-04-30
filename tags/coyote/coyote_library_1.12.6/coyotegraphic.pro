; docformat = 'rst'
;
; NAME:
;   CoyoteGraphic
;
; PURPOSE:
;   This simple just identifies a routine as a Coyote Graphic routine. It is written
;   primarily so I can identify such routines before I assign a background color to
;   a graphics window.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;   This simple just identifies a routine as a Coyote Graphic routine. It is written
;   primarily so I can identify such routines before I assign a background color to
;   a graphics window.
;
; :Categories:
;    Graphics
;    
; :Examples:
;    Used in graphics programs::
;       IDL> IF CoyoteGraphic() THEN background = 'white'
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
;        Written, 18 January 2011. DWF.      
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION CoyoteGraphic, routine

    list = [ $
      'CGARROW', $
      'CGAXIS', $
      'CGCOLOR', $
      'CGBARPLOT', $
      'CGBOXPLOT', $
      'CGCOLORBAR', $
      'CGCOLORFILL', $
      'CGCONTOUR', $
      'CGDCBAR', $
      'CGDISPLAY', $
      'CGERASE', $
      'CGHISTOPLOT', $
      'CGIMAGE', $
      'CGLOADCT', $
      'CGPLOT', $
      'CGPLOTS', $
      'CGSCATTER2D', $
      'CGSURF', $
      'CGSURFACE', $
      'CGSYMBOL', $
      'CGTEXT', $
      'CGCONTROL', $
      'CGDELETE', $
      'CGWINDOW', $
      'CGSET', $  
      'CTLOAD', $
      'DCBAR', $
      'FSC_COLOR', $
      'FSC_COLORBAR', $
      'HISTOPLOT', $
      'TVIMAGE', $
      'TVSCALE', $
      'XCOLORS' $
      ]
      
   ; Can you find the routine in the list.
   index = Where(list EQ StrUpCase(routine), count)
   RETURN, count
   
END
      
