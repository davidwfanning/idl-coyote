;+
; NAME:
;   PARSELINE
;
; PURPOSE:
;   This function accepts a string as an argument and returns an
;   array which contains the numerical values in that string.  It can
;   handle space or comma delimited numerical values.  It returns
;   a value of -1 if non-numerical stuff is found in the line.
;
; CALLING SEQUENCE:
;   array_out = PARSELINE(string_in, desired_data)
;
; OPTIONAL INPUTS:
;
;   DESIRED_DATA -- An array which specifies the desired columns of data
;   from the string.  It is zero based.  Column numbers can be repeated in
;   the array.  If this is not specified the default is to return an array
;   containing each value in the string once in the order they appear in
;   the string.
;
; KEYWORD PARAMETERS:
;
;   DOUBLE -- The default behavior.  The returned array will be a double
;   precision floating point array.
;
;   FLOAT -- The returned array will be a single precision floating point
;   array.
;
;   INTEGER -- The returned array will be an array of short integers.
;
; RETURN VALUE:
;
;   array_out = dblarr(n_data)  N_DATA is either the number of columns in
;   the string or the number of elements in the DESIRED_DATA array.
;
; USAGE:
;
; The calling procedure will look something like:
;
;    IDL> text=''
;    IDL> openr,1,'input.dat'
;    IDL> readf,1,text
;    IDL> data=parseline(text,[0,1,5,4,6,7])
;    IDL> print,data
;    0.10000000  3.4000000  2.4000000e-06  5.0000000  2.0080000  0.0000000
;
; MAJOR FUNCTIONS and PROCEDURES:
;
;   None.
;
; MODIFICATION HISTORY:
;
;   Written by: Alan Munter, munter@uiuc.edu, May 1997.
;
;-

FUNCTION parseline, string_in, data_columns, double=double, float=float, $
   integer=integer

   on_ioerror, error

; The maximum number of values in the line is the string length divided by 2

   case n_params() of
      0: message, 'Not enough arguments to parseline'
      1: data_in_line=dblarr(fix(strlen(string_in)/2))
      2: data_in_line=dblarr(max(data_columns)+1)
   endcase

; substitute spaces for commas

   while strpos(string_in,',') ne -1 do $
      strput,string_in,' ',strpos(string_in,',')

; take out all of the spaces and add one onto the end

   string_in=strtrim(strcompress(string_in),2) + ' '

; get the all of the numerical values in the line

   i=0
   beginspace=0
   space=0
   while space ne -1 do begin
      space=strpos(string_in,' ',beginspace) ; space at end of number
      if space ne -1 then $
         data_in_line(i)=double(strmid(string_in,beginspace,space-beginspace))
      beginspace = space+1
      i=i+1
   endwhile

; define the array that will be returned

   if n_elements(data_columns) gt 0 then begin
      array_out=data_in_line(data_columns)
   endif else begin
      array_out=data_in_line(0:i-2)
   endelse

; convert the output to the correct format

   if keyword_set(float) then array_out=float(array_out)
   if keyword_set(integer) then array_out=fix(array_out)

   return, array_out
   error: return, -1
END
