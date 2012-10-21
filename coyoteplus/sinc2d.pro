FUNCTION SINC2D,X,Y
;+
; Z=SINC2D(X,Y)
; Computes the 2D SINC function for the arrays X and Y,
;
; X and Y must be arrays that are of the same size.
;
; USAGE
; Compute X and Y arrays using MESHDOM
; s=FINDGEN(101)/10 -5
; t=FINDGEN(101)/10 -5
; MESHDOM,s,t,X,Y
; Z=TRI2D(X/2,Y/3)
; SURFACE,Z,X/2,Y/3
;
; HISTORY
; Written by Harvey Rhody, September 1997
;-

RETURN,sinc(X)*sinc(Y)
END