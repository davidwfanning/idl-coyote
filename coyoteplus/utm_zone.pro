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
;  Lon   Set this keyword equal to a named variable to return the central meridian for each
;      value in the input argument.
;
; EXAMPLE:
;
;  To find the UTM zone for -67 degrees (67 West)...
;  IDL> Print, UTM_ZONE(-67)
;             19
;
; REFERENCE:
;    J.P. Snyder, "Map projections - A working manual',1987,
;		U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;		U.S. Govt Printing Office,Washington, DC 20402.
;
;     see pages 57-58 and Figure 11
;
; MODIFICATION HISTORY:
;	Written by Ben Tupper, 13JUL2000.
;   pemaquidriver@tidewater.net
;-

FUNCTION UTM_ZONE, Lon, CM = CM

On_Error, 2

   ;be sure that the lonitude is a bit smaller than extremes
CM = -179. > FLoat(Lon) < 179.
  ;make an array of bounding meridians for each zone
Bounds = Findgen(61)*6. - 180.
N = N_elements(CM)
Zone = LonArr(N)
For i = 0L, N-1L Do Zone[i]=   (Where ( Bounds GE CM[i]) )[0]
  ;if requested transform CM into its proper value
If Arg_Present(CM) Then CM = Bounds[Zone-1L]+3.

Return, Zone

END