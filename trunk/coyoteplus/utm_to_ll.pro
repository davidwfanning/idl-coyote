;+
; NAME:
;	UTM_TO_LL
;
; PURPOSE:
;	This function converts UTM coordinates to decimal degrees of lon/lat.
;	Many map data are available.
;
; REFERENCE:
;	 J.P. Snyder, "Map projections - A working manual',1987,
;		U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;		U.S. Govt Printing Office,Washington, DC 20402.
;
; CATEGORY:
;	Mapping.
;
; CALLING SEQUENCE:
;	Result = UTM_TO_LL(East,North, [DatumName])
;
; INPUTS:
;	East/North   Identically sized vectors containing the UTM coordinates
;	in meters for false Easting and Northing. 
;
; OPTIONAL INPUTS:
;  DatumName   Set this equal to a scalar string of a map datum to use.
;  Possible choices are:
;          NAD27
;		 	  WGS84
;			  NAD83
;      	  GRS80
;      	  WGS82
;      	  AUS1965
;      	  KRAS1940
;      	  INT1924
;      	  HAY1909
;      	  CLARKE1880
;      	  CLARKE1866
;      	  AIRY1830
;      	  BESSEL1841
;      	  EVEREST1830
;
;  If this is not set and the Datum keyword is not set then NAD27/CLARKE1866 is used.
;
; KEYWORD PARAMETERS:
;	Zone 	Set this keyword to the Zone number for the UTM coordinate grid.  If not known,
;     please consult the reference above.  The default is 31.
;     As a quick reference, zones are 6 degrees of longitude wide starting at Zone 1
;     from -180 to -174 degrees.   Zone 60 extends from 174 to 180 degrees.
;   NoFalse  Set this keyword to indicate that Easting is not in False Easting units.
;   CM Set this keyword equal to a named variable to retreive the central meridian
;     in degrees for each lon/lat pair.
;   P Set this keyword to a named variable to retrieve the map datum parameters.
;
; OUTPUTS:
;	A 2,n elements floating point array where [0,*] = Latitude and [1,*] = Longitude in
;	decimal degrees.
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
;	Calls MAP_DATUM  function.
;
; EXAMPLE:
;  The following example is from the worked solutions in the Snyder reference.
;  Snyder's answers are Lon = -73.5 (West) and Lat = 40.5 (North)
;
;IDL> Print, UTM_TO_LL(East,North, 'CLARKE1866', Zone = 18,CM = CM)
;      -73.499995       40.499994
;
; COMMENTS:
;  I suspect the difference between the solution in the procedure and that in
;  Snyder's worked example lies in the calculation of Eccentricity (P.E).
;   Snyder gives the formula E = SQRT(2*F - F^2)...
;   Thus for The 'CLARKE1866' map datum E = 0.0822717.  Surprisingly,
;   the worked example uses E = 0.0822719 without explanation.
; For the CLARKE1866 map datum I could substitute the Snyder's value for
;   the value resulting from the formula, but I'm not inclined to do so until
;   I get confirmation from the USGS that one or the other is 'wrong'.  BT 14JUL2000
;
; MODIFICATION HISTORY:
; 	Written by:	Ben Tupper		Spring 1998
;	Pemaqid River Company
;	email: pemaquidriver@tidewater.net
;	tel:  (207) 563 - 1048
;   248 Lower Round Pond Road
;	POB 106
;	Bristol, ME 04539-0106
;
;  SEP 1999	Cleaned up documentation.
;  14JULY2000  Added Zone keyword default to 31. BT
;		Added MAP_DATUM fucntion to support all map data listed
;	   on table 1 (Ch 3) in Snyder Reference.
;      Added CM keyword.
;      Added NoFalse Keyword.
;      Cleaned up the formatting.
;      Added P keyword.
;	   Now returns single precision.
;	   Datum keyword no longer supported.
;
;  14 JAN 2008 Calculations in double precision and changed
;  default DATUM to 'WGS84'. DWF
;-


FUNCTION UTM_TO_LL, East, North, DatumName,$
	Zone = Zone, CM=CM, P=P, NoFalse = NoFalse

   On_Error, 2
   
   If N_Params() LT 2 Then Begin
       Message, 'Two inputs arguments are required'
       Return, -1
   EndIf
   
   Num = N_elements(East)
   If N_elements(North) NE Num Then Begin
      Message,'Input vectors must be same length'
      Return, -1
   EndIf
   
   X=Double(East)
   Y=Double(North)
   If Not KeyWord_Set(NoFalse) Then X = X - 500000.0d
   
   If N_elements(DatumName) EQ 0 Then $
       P = Map_Datum('WGS84') Else $
       P = Map_Datum(StrTrim(DatumName[0],2))
   
   E2p = P.E^2*(1.-P.E^2)
   E1 = (1. - SQRT(1. - P.E^2))/(1. + SQRT(1. - P.E^2))
   K0 = 0.9996D
   
   
   South = Where(North GT 10000000.d, SouthCount)
   M0 = FltArr(Num)
   If SouthCount GT 0 Then M0[South] = 10000000.d
   M = M0 + Y/K0
   u  = M/(P.A*(1.0-P.E^2/4.0 - 3.0*P.E^4/64.0- 5.0*P.E^6/256.0))	;rectifying latitude
   
   If N_elements(Zone) EQ 0 Then Zone = Replicate(31,Num)
   CM = -180.0 + 3.0d*(2.*(1>Zone<60) - 1.)
   
      ;Footprint latitude in degrees
   Phi1 = u + (3.0*E1/2.0 - 27.0*E1^3/32.0)*sin(2.0*u) + $
   	(21.0*E1^2/16.0 - 55.0*E1^4/32.0)*sin(4.0*u) + $
   	(151.0*E1^3/96.0)*sin(6.0*u) + $
   	(1097.0*E1^4/512.0)*sin(8.0*u)
   
   C1 = E2p*(cos(Phi1))^2
   T1 = (tan(Phi1))^2
   N1 = P.A/SQRT(1.0 -P.E^2*(sin(Phi1))^2 )
   R1 = P.A*(1.0-P.E^2)/(1.0-P.E^2*(sin(Phi1))^2)^1.5
   D = X/(N1*K0)
   
   Y = Phi1 - $
   	(N1*tan(Phi1)/R1)*(D^2/2.0 - (5.0 + 3.0*T1 + 10.0*C1 -4.0*C1^2 - 9.0*E2p)*D^4/24 +$
   	(61.0 + 90.0*T1 + 298.0*C1 + 45.0*T1^2 + 252.0*E2p - 3.0*C1^2)*D^6/720.0)
   
   X = CM*!DtoR + $
   	(D - (1.0 + 2.0*T1 + C1)*D^3/6.0 + $
   	(5.0 - 2.0*C1 + 28.0*T1 - 3.0*C1^2 + 8.0*E2p + 24.0*T1^2)*(D^5)/120.0)/cos(Phi1)
   
   RETURN, Transpose([[X],[Y]])*!RaDeg

end