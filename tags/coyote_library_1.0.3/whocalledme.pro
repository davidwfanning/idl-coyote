; docformat = 'rst'
;
; NAME:
;   WhoCalledMe
;
; PURPOSE:
;   This is a function that will identify the caller of the program module
;   that contains this funciton.
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
;   This is a function that will identify the caller of the program module
;   that contains this funciton.
;
; :Categories:
;    Utilities
;    
; :Params:
;    none
;       
; :Keywords:
;     none
;     
; : Return Value:
;     caller:
;         A string in uppercase letters identifying the caller of the program
;         module from which this program was called.
;         
; :Examples:
;    Used to determine which module called this module containing WhoCalledMe::
;       IDL> Print, WhoCalledMe()
;       
;       ; Compile and run the following main level program.
;       ;***************************
;       PRO junker
;          Print, WhoCalledMe()
;       END
;       
;       PRO junk
;          Print, WhoCalledMe()
;       END
;       
;       junk
;       END
;       ;**************************

;       IDL> .go
;            $MAIN$
;            JUNK
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
;        Written, 16 January 2011. DWF. 
;
; :Copyright:
;     Copyright (c) 2011, Fanning Software Consulting, Inc.
;-
FUNCTION WhoCalledMe

   Compile_Opt idl2

   ; Return to caller on error.
   On_Error, 2

   ; Get the call stack and the calling routine's name.
   callStack = Scope_Traceback()
   
   ; Find where I am in the call stack. The calling program is up
   ; two levels from there. Unless, of course, I am close to $MAIN$.
   index = Where(StrMid(callstack, 0, 11) EQ 'WHOCALLEDME', count)
   IF count GE 1 THEN index = (Reverse(index))[0] 
   callingRoutine = (StrSplit(StrCompress(callStack[(index-1) > 0])," ", /Extract))[0]
   
   ; Return the answer.
   RETURN, callingRoutine
   
END
