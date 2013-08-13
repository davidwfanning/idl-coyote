;+
;	NAME:
;		Map_Datum
;
;   PURPOSE:
;   This function returns a structure of mapping parameters based on the Map Datum selected.
;
;   CALLING SEQUENCE:
;     result = MAP_DATUM(DatumName)
;
;   INPUT ARGUMENTS:
;     DatumName    A scalar string of one of the following values....
;		      NAD27
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
;  KEYWORDS:
;	MAP_SET   Set this keyword to return an array suitable for the Map_SET
;		command.
;
;   RETURNED STRUCTURE:
;     The returned structure contains the following fields
;	     NAME: DatumName (Str)
;        A: Equatorial Radius (m, float)
;        B: Polar Radius (m, float)
;        F: Flattening (dbl)
;        E:  Eccentricity  SQRT((2F - F^2))    (float)
;
;  All of the values are  from the following reference:
;    J.P. Snyder, "Map projections - A working manual',1987,
;		U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;		U.S. Govt Prinitng Office,Washington, DC 20402.
;
; MODIFICATION HISTORY:
;   Written by Ben Tupper, JULY 14 2000
;   pemaquidriver@tidewater.net
;   28JULY2000 Added Map_Set keyword
;-

FUNCTION MAP_DATUM, DatumName, Map_Set = Map_Set

On_Error,2

Case StrUpCase(DatumName) of
   'WGS84':BEGIN			;same as GRS80
   		a = 6378137.
   		b = 6356752.3
   		f = 1/298.257
   		END
   'NAD27':BEGIN			;same as Clarke1866
   		a = 6378206.4
   		b = 6356583.8
   		f = 1/294.98
   		END
   'NAD83':BEGIN			;same as GRS80
   		a = 6378137.
   		b = 6356752.3
   		f = 1/298.257
   		END
   'GRS80':BEGIN
   		a = 6378137.
   		b = 6356752.3
   		f = 1/298.257
   		END
   'WGS72':BEGIN
   		a = 6378135.
   		b = 6356750.5
   		f = 1/298.26
   		END
   'AUS1965':BEGIN
   		a = 6378160.
   		b = 6356774.7
   		f = 1/298.25
   		END
   'KRAS1940':BEGIN
   		a = 6378245.
   		b = 6356863.
   		f = 1/298.3
   		END
   'INT1924':BEGIN
   		a = 6378388.
   		b = 6356911.9
   		f = 1/297.
   		END
   'HAY1909':BEGIN		;same as INT1924
   		a = 6378388.
   		b = 6356911.9
   		f = 1/297.
   		END
   'EURO50':BEGIN	    ;same as INT1924
   		a = 6378388.
   		b = 6356911.9
   		f = 1/297.
   		END
   'CLARKE1880':BEGIN
   		a = 6378249.1
   		b = 6356514.9
   		f = 1/293.46
   		END
   'CLARKE1866':BEGIN
   		a = 6378206.4
   		b = 6356583.8
   		f = 1/294.98
   		END
   'AIRY1830':BEGIN
   		a = 6377563.4
   		b = 6356256.9
   		f = 1/299.32
   		END
   'BESSEL1841':BEGIN
   		a = 6377397.2
   		b = 6356079.0
   		f = 1/299.15
   		END
   'EVEREST1830':BEGIN
   		a = 6377276.3
   		b = 6356075.4
   		f = 1/303.80
   		END
   ELSE:BEGIN
   		MESSAGE, 'DatumName: '+DatumName+' is not currently supported'
   		Return,-1
   		END
EndCase

If KeyWord_SET(Map_Set) Then $
	Return, [a, ( 2.*f - f^2) ,  0.9996] Else $
	Return, {Name: DatumName, A:a, B:b, F:f, E:SQRT(2*f-f^2) }

END