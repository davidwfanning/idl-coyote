FUNCTION SINC,X
;+
; FUNCTION SINC,X
; Computes the standard SINC function for any list of values represented
; by the vector x. See Gaskill, Page 47.
;
; USAGE:
; x=FINDGEN(1001)/20-25 CREATE A VECTOR ON THE INTERVAL [-25,25]
; y=SINC((x-8)/5) Construct a function that is shifted and stretched
;
; HISTORY
; Written by Harvey Rhody, September 1997
;-

X=FLOAT(X)
i0=WHERE(X EQ 0.)
Y=0.*X
IF MIN(i0) GE 0 THEN Y[i0]=1.0
i1=WHERE(X NE 0.)
Y(i1)=SIN(!PI*X(i1))/(!PI*X(i1))
RETURN,Y
END
