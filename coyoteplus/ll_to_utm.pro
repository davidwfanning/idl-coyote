;+
; NAME:
;   LL_TO_UTM
;
; PURPOSE:
;	This function converts longitude/latitude measurements to UTM coordinates.
;	Many different map data are available for estimating the shape of the
;	ellipsoid.
;
; REFERENCE:
;	J.P. Snyder, "Map projections - A working manual',1987,
;		U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;		U.S. Govt Printing Office,Washington, DC 20402.
;
; CATEGORY:
;	Mapping.
;
; CALLING SEQUENCE:
;	Result = ll_to_utm(lon,lat,[DatumName])
;
; INPUTS:
;	LON, LAT   Identically sized vectors containing the decimal degrees of latitude and longitude.
;	Degrees west and south are negative.
;
; OPTIONAL INPUTS:
;  DatumName   Set this equal to a scalar string of a map datum to use.
;  Possible choices are:
;             NAD27
;		 	  WGS84
;			  NAD83
;      	      GRS80
;      	      WGS82
;      	      AUS1965
;      	      KRAS1940
;      	      INT1924
;      	      HAY1909
;      	      CLARKE1880
;      	      CLARKE1866
;      	      AIRY1830
;      	      BESSEL1841
;      	      EVEREST1830
;
;  If this is not set and the Datum keyword is not set then NAD27/CLARKE1866 is used.
;
; KEYWORD PARAMETERS:
;	DATUM	Set this keyword to one of the following:
;		1 NAD27 (Default)
;		2 WGS84
;		3 International 1924 (aka Euro50 and Hay1909)  (NOT TESTED AS OF 11JUL2000 BT)
;	   Other values are not supported since the DatumName argument is easier to use.
;	NOFALSE Set this keyword to prevent the return of False Easting. Default is to
;		return a False Easting (NoFalse = 0).
;   P Set this keyword to a named variable to return the map datum parameters.
;   K Set this keyword equal to anamed variable to return the scaling factor for
;	   each lon/lat pair.
;   Zone Set this keyword equal to a named variable to return the UTM zone to
;	  which each lon/lat pair belongs.  NOTE:  Only the numbered (1-60) longitudinal
;     zone values are returned.  The lettered (A-Z latitudinal zones are not
;	  returned.
;   CM Set this keyword equal to a named variable to retrun the central meridian
;	  for each lon/lat pair's zone.
;
; OUTPUTS:
;	A 2,n elements double precision array where [0,*] = (Flase) Easting
;    and [1,*] = False Northing.
;
; OPTIONAL OUTPUTS:
;	None.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None known.
;
; RESTRICTIONS:
;	Calls Map_Datum and UTM_ZONE functions.
;
; EXAMPLE:
;	IDL> print, ll_to_utm(-73.5, 40.5, 'CLARKE1866',CM = CM, K = K, P =P, Zone = Zone), format = '(2(F10.2, 1x))'
; 		627106.31 4484119.00
;	IDL> print, CM, K, Zone
;     -75.0000
;      0.99979888
;          18
; IDL> help, P,/str
;** Structure <1055df8>, 5 tags, length=24, refs=1:
;   NAME            STRING    'CLARKE1866'
;   A               FLOAT      6.37821e+006
;   B               FLOAT      6.35658e+006
;   F               FLOAT        0.00339006
;   E               FLOAT         0.0822717
;
; COMMENTS:
;  Using the worked example from Snyder (p269) reveals a slight difference between
;  the worked example and the results shown below
;   SNYDERS RESULTS
;	  LON = -73.5  -> UTM Easting = 627,106.5m
;     LAT = 40.5 -> UTM Northing = 4,484,124.4m
;
;  I suspect the difference lies in the calculation of Eccentricity (P.E).
;   Snyder gives the formula E = SQRT(2*F - F^2)...
;   Thus for The 'CLARKE1866' map datum E = 0.0822717.  Surprisingly,
;   the worked example uses E = 0.0822719 without explanation.
; For the CLARKE1866 map datum I could substitute the Snyder's value for
;   the value resulting from the formula, but I'm not inclined to do so until
;   I get confirmation from the USGS that one or the other is 'wrong'.  BT 14JUL2000
;
;
; MODIFICATION HISTORY:
; 	Written by:	Ben Tupper		Spring 1998
;	Pemaquid River Company
;	email: pemaquidriver@tidewater.net
;	tel:  (207) 563 - 1048
;   248 Lower Round Pond Road
;	POB 106
;	Bristol, ME 04539-0106
;
;  SEP 1999	Cleaned up documentation and added NoFalse keyword.
;  DEC 2, 1999  Reversed order of input and output arguments to fit familiar x,y ordered pair sense.
;  4 FEB 2000 Documented change on 2 DEC 1999.
; 11JUL2000 Added International1924 (Euro50) Map Datum.   BT
; 13JUL2000 Added DatumName argument and all available Map Data from Snyder. BT
;	Added ZONE, CM, P and K keywords.
; 14JUL2000 Luis Alonso found errors in result.  Formulas retyped in a neater form.
;   Values still agrees (closely) with Snyder's worked example but Luis' example
;   is stiil about 1km off.  I suspect this has to do with the difference between
;   eccentricity values... I'll contact USGS/Snyder.
;   New functions UTM_ZONE and MAP_DATUM created.
;
;-

FUNCTION LL_TO_UTM, LonV, LatV, DatumName,$
	Datum=Datum, Zone = Zone, CM = CM, K = K, P = P, $
	NoFalse = NoFalse

If N_Params() LT 2 Then Begin
   Message, 'Must supply at least two arguments'
   Return, -1
EndIf

Num = N_elements(LonV)
If N_elements(LatV) NE Num Then Begin
   Message, 'Input arguments must have same number of elements'
   Return, -1
EndIf

Utm = FltArr(2,N_Elements(LonV))
Lon=Float(LonV)			&	Lat=Float(LatV)

If N_Elements(DatumName) EQ 0 Then Begin
  If N_elements(Datum) NE 0 Then Begin
      DatumChoices = ['Blank','NAD27','WGS84', 'NAD83',$
      	      'GRS80', 'WGS82', 'AUS1965', 'KRAS1940','INT1924', 'HAY1909', 'CLARKE1880',$
      	      'CLARKE1866', 'AIRY1830', 'BESSEL1841', 'EVEREST1830']
      DatumName = DatumChoices[Datum[0]]
  EndIf Else DatumName = 'NAD27'

EndIf

	;get the map datum info
P = Map_Datum(DatumName)
If Size(P,/Type)  NE 8 Then Begin
   Message, 'Map Datum not found'
   Return, -1
EndIf

    ;make Ecc_sq_prime
E2p = P.E^2*(1.0 - P.E^2)

	;get the UTM zone ID and the central meridian
Zone = UTM_Zone(Lon, CM = CM)

	;determine lon origin (cm) in radians
Lon0 = CM*!DtoR

	;	convert to radians
Lon = Lon*!DtoR    &    Lat = Lat*!DtoR

N = P.A/SQRT(1. - P.E^2*(sin(lat))^2 )
T = ( Tan(Lat) )^2
C = E2p * ( cos(lat) )^2
A = Cos(Lat ) * (Lon - Lon0)

  ;for the UTM projections, the scaling factor K is taken to be a constant, K0,
  ; at the central meridian
K0 = 0.9996d
	; k is unused for UTM... default to k0
	; make it available any way
If Arg_Present(K) Then $
  k = k0*(1.+ (1.+C)*(A^2)/2. + $
	(5. - 4.*T + 42.*C + 13.*C^2 -28.*E2p)*(A^4)/24. + $
	(61. - 148.*T + 16.*T^2)*(A^6)/720.)

South =Where(LatV LT 0.0, CountSouth)
    ; M is M(Lat Origin, the equator)
M0 = FltArr(Num)
If CountSouth GT 0 Then M0[South] = 10000000.

   ;M is the true distance along the CM from the equator
M = P.A*((1.0 - P.E^2/4. - 3.*P.E^4/64. - 5.*P.E^6/256. )* Lat - $
	(3.*P.E^2/8. + 3.*P.E^4/32.+45.*P.E^6/1024.)*sin(2.*lat) + $
	(15.*P.E^4^2/256.+45.*P.E^6/1024.)* sin(4.*lat) - $
	(35.*P.E^6/3072.)*sin(6.*lat) )

	;  Easting or X in Snyder
Lon =  k0*N*(A + (1. - T + C)*(A^3)/6. $
	+ (5. - 18.*T + T^2 + 72.*C - 58.*E2p)*(A^5)/120.)

	;Northing or Y in Snyder
Lat =   K0*(M-M0 + N*tan(Temporary(lat))*((A^2) / 2. + ( 5. - T + 9.*C + 4.*C^2 ) * (A^4)/24. + $
	              (61. - 58.*T + T^2 + 600.*C - 330.*E2p)*(A^6) / 720.))

	; make False Easting if desired (default)
If Not (KeyWord_Set(NoFalse))  Then Lon = Lon + 500000.0d

Return,Transpose([[Lon],[Lat]])

END