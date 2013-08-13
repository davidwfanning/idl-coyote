;+
; NAME:
;  UTM_ZONE
;
; PURPOSE:
;  The function returns the UTM zone number for a given longitude.
;
; CALLING SEQUENCE:
;  result = UTM_ZONE(CM)
;
; ARGUMENTS:
;  The longitude for the desired zone.   Must be in decimal degrees
;    and must be -180. > Lon < 180. where longitude west is negative.
;   This argument may be a vector.
;
; KEYWORDS:
;  CM   Set this keyword equal to a named variable to return the central meridian for each
;      value in the input argument.
;   DOUBLE  Set this keyword to force calculations in double precision.
;
; EXAMPLE:
;
;  To find the UTM zone for -67 degrees (67 West)...
;  IDL> Print, UTM_ZONE(-67)
;             19
;
; REFERENCE:
;    J.P. Snyder, "Map projections - A working manual',1987,
;       U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;       U.S. Govt Printing Office,Washington, DC 20402.
;
;     see pages 57-58 and Figure 11
;
; MODIFICATION HISTORY:
;   Written by Ben Tupper, 13JUL2000.
;   pemaquidriver@tidewater.net
;   27 JAN 2003
;       Added DOUBLE.  BT
;       Patched up problem with lon = -180 =>  Zone =  0
;           now it forces the zone to be between 1 and 60 inclusive
;       Removed loop in zone calculation.
; 2008-05-20 BT (IDL 6.3) Changed zone calulation to use VALUE_LOCATE
;   and prtected the input argument (what was I thinking!).
;-

FUNCTION UTM_ZONE, iLon, CM = CM, Double = Double

On_Error, 2

Dbl = Keyword_set(Double)
Machine = Machar(Double = Dbl)

Lon = iLon

   ;be sure that the longitude is a bit smaller than extremes
CM = Dbl ? $
    (-180.d + Machine.eps)  > Lon < (180.d - Machine.eps) : $
    (-180. + Machine.eps)  > Lon < (180. - Machine.eps)

  ;make an array of bounding meridians for each zone
Bounds = Dbl ? $
    Dindgen(61)*6.d - 180.d : $
    Findgen(61)*6. - 180.

N = N_elements(CM)
;Zone =  1 > ((Where ( Bounds GE CM) )[0] )  < 60
Zone = 1 > (VALUE_LOCATE(bounds, cm) + 1) < 60
  ;if requested transform CM into its proper value
If Arg_Present(CM) Then CM = Bounds[Zone-1L]+3.

Return, Zone
END