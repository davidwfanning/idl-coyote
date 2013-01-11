; docformat = 'rst'
;+
; This program ...
;
; :Categories:
;    Utilities
;    
; :Examples:
;    Here is how to use this program::
;       IDL> tic
;       IDL> p = Plot(cgDemodata(1))
;       IDL> toc
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
;     Change History::
;        Written, 10 January 2013 by David W. Fanning.
;
; :Copyright:
;     Copyright (c) 2013, Fanning Software Consulting, Inc.
;-
PRO Toc, ELAPSED_TIME=elapsed_time

   COMMON _fsc_clock$time, start_time
   
   elapsed_time = Systime(1) - start_time
   Print, 'Elapsed Time: ', elapsed_time, Format='(A15, x, F0.6)'

END

PRO Tic

   COMMON _fsc_clock$time, start_time

   start_time = Systime(1)
 
END
   
   