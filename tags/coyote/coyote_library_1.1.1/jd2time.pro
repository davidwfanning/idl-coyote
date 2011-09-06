;+
; NAME:
;    JD2TIME
;
; PURPOSE:
;
;    The purpose of this function is to convert a Julian day number into
;    a time string of the form "16 Mar 2009".
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;    Utility.
;
; CALLING SEQUENCE:
;
;    result = JD2TIME(jdnumber, jdyear)
;
; INPUTS:
;
;    jdnumber:   A Julian day number or array of Julian day numbers. If absent,
;                today's current Julian day number.
;                
;    jdyear:     The year for which the Julian day number applies. If absent, the current year.
;    
; OUTPUTS:
;
;    result:     A scalar or vector of time strings of the form "16 Mar 2009 15:34:26".
;
; KEYWORDS:
;
;    None.
;    
; DEPENDENCIES:
; 
;    Requires THEMONTHS from the Coyote Library.
;    
;         http://www.dfanning.com/programs/themonths.pro
;
; MODIFICATION HISTORY:
;
;    Written by: David W. Fanning, 25 June 2009.
;-
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
FUNCTION JD2Time, jdnumber, jdyear

    ; Return to caller, on error
    On_Error, 2
    
    ; Get today's date, if argument is not present.
    IF N_Elements(jdnumber) EQ 0 THEN BEGIN
        jnumber = Systime(/JULIAN)
        CalDat, jnumber, month, day, year
        RETURN, String(day, FORMAT='(I2.2)') + ' ' + $
                theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
                String(year, FORMAT='(I4)')
    ENDIF
    
    ; Make sure this is a Julian Day Number.
    num = N_Elements(jdnumber)
    error = BytArr(num)
    FOR j=0,num-1 DO BEGIN
        IF jdnumber[j] LT 1 OR jdnumber[j] GT 366 THEN error[j] = 1
    ENDFOR
    IF Total(error) NE 0 THEN Message, 'Julian Day Numbers must be in the range 1-366.'
    
    ; Make sure you have a year and that the number matches the number of days.
    IF N_Elements(jdyear) EQ 0 THEN jdyear = Replicate(Fix(StrMid(Systime(), 19)), num)
    IF N_Elements(jdyear) NE num THEN jdyear = Replicate(jdyear, num)
    
    ; Calculate the date.
    CalDat, Julday(1, jdnumber, jdyear), month, day
    timeString = String(day, FORMAT='(I2.2)') + ' ' + $
         theMonths(month, /ABBREVIATION, /FIRSTLETTER) + ' ' + $
         String(jdyear, FORMAT='(I4)')   
     
   ; Make sure you return a scalar, if needed.
   IF num EQ 1 THEN Return, timeString[0] ELSE RETURN, timeString
   
END