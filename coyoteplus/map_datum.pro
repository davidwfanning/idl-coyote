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
;		     NAD27
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
;   RETURNED STRUCTURE:
;     The returned structure contains the following fields
;	     NAME: DatumName (Str)
;        A: Equatorial Radius (m, double)
;        B: Polar Radius (m, double)
;        F: Flattening (double)
;        E:  Eccentricity  SQRT((2F - F^2))    (double)
;
;  All of the values are  from the following reference:
;    J.P. Snyder, "Map projections - A working manual',1987,
;		U.S.G.S. Professional Paper 1395, Supt. of Docs No: I 19.16:1395,
;		U.S. Govt Prinitng Office,Washington, DC 20402.
;
; MODIFICATION HISTORY:
;   Written by Ben Tupper, JULY 14 2000
;   pemaquidriver@tidewater.net
;   Changed all values to doubles. DWF 14 January 2008.
;-

FUNCTION MAP_DATUM, DatumName

   On_Error,2
   
   Case StrUpCase(DatumName) of
      'WGS84':BEGIN			;same as GRS80
      		a = 6378137.D
      		b = 6356752.3D
      		f = 1/298.257D
      		END
      'NAD27':BEGIN			;same as Clarke1866
      		a = 6378206.4D
      		b = 6356583.8D
      		f = 1/294.98D
      		END
      'NAD83':BEGIN			;same as GRS80
      		a = 6378137.D
      		b = 6356752.3D
      		f = 1/298.257D
      		END
      'GRS80':BEGIN
      		a = 6378137.D
      		b = 6356752.3D
      		f = 1/298.257D
      		END
      'WGS72':BEGIN
      		a = 6378135.D
      		b = 6356750.5D
      		f = 1/298.26D
      		END
      'AUS1965':BEGIN
      		a = 6378160.D
      		b = 6356774.7D
      		f = 1/298.25D
      		END
      'KRAS1940':BEGIN
      		a = 6378245.D
      		b = 6356863.D
      		f = 1/298.3D
      		END
      'INT1924':BEGIN
      		a = 6378388.D
      		b = 6356911.9D
      		f = 1/297.D
      		END
      'HAY1909':BEGIN		;same as INT1924
      		a = 6378388.D
      		b = 6356911.9D
      		f = 1/297.D
      		END
      'EURO50':BEGIN	    ;same as INT1924
      		a = 6378388.D
      		b = 6356911.9D
      		f = 1/297.D
      		END
      'CLARKE1880':BEGIN
      		a = 6378249.1D
      		b = 6356514.9D
      		f = 1/293.46D
      		END
      'CLARKE1866':BEGIN
      		a = 6378206.4D
      		b = 6356583.8D
      		f = 1/294.98D
      		END
      'AIRY1830':BEGIN
      		a = 6377563.4D
      		b = 6356256.9D
      		f = 1/299.32D
      		END
      'BESSEL1841':BEGIN
      		a = 6377397.2D
      		b = 6356079.0D
      		f = 1/299.15D
      		END
      'EVEREST1830':BEGIN
      		a = 6377276.3D
      		b = 6356075.4D
      		f = 1/303.80D
      		END
      ELSE:BEGIN
      		MESSAGE, 'DatumName: '+DatumName+' is not currently supported'
      		Return,-1
      		END
   EndCase
   
   Return, {Name: DatumName, A:a, B:b, F:f, E:SQRT(2*f-f^2) }

END