;+
;	NAME:	GREATCIRCLE
;
; PURPOSE:
;	This function  returns the azimuth and range (great circle distance) between consecutive
;	points of longitude, latitude.
;
; CATEGORY:
;	Mapping
;
; CALLING SEQUENCE:
;
;	Result = GreatCircle(Lon,Lat)
;
; INPUT
;	LON   A vector of longitude values in decimal degrees (-180. to 180.)
;	LAT    A vector of latitude values in decimal degrees (-90. to 90.)
;
; KEYWORDS
;	RADIANS
;	Set this keyword to indicate that the input values are in radians.  Setting this value
;	cause the output azimuth values to be in radians.
;
;	RADIUS
;	Set this keyword to the desired raduis of the sphere.   The default value is
;	is the  equatorial radius for the GRS 80 model for the Earth ellipsoid.
;	The WGS84 (NAD 83) map datum uses the same radius.
;	Default value (GRS 80) =  6378137. meters (Source = Snyder, Chapter 3, Table 1)
;
;	CLARKE
;	Set this keyword to a non-zero value to use the equatorial radius
;	for the Clarke 1866 model for the ellipsoid.
;	NAD27 mapdatum uses the same value.
;	Value (Clarke 1866) =  6378206.4 meters (source = Snyder, Chapter 3, Table 1)
;	If this keyword is not set, the GRS 80 value is used.   This keyword has no effect if
;	the radius keyword is set.
;
; OUTPUT
;	A  (2,n) array of great circle distance and azimuth between consecutive
;	points in the LON/LAT input arguments.   The first column contains the
;	great circle distance in units of the radius. The second column contains the azimuth in decimal degrees.
;	The last row of the returned array will always be equal to [0.0,0.0].
;
; DISCUSSION
;	Snyder describes (Chapter 5) calculations for great circle distance,
;	azimuth and range between pairs of locations on the surface of a sphere.
;	Extension of these calculations to the ellipsoid "is much more involved technically
;	and requires approximation.  General discussion of this is omitted here."
;
;	In the example, points A (LonA, LatA) and B (LonB, LatB)  are separated $
;	by an arbitrary distance on the surface of a sphere. A third point, C  is placed at the pole
;	(North Pole in example.)  The angle subtended at C is the anglular separation between
;	the meridians extending to A and B.
;
;	Connecting these three points, along the surface of the sphere,
;	creates a sphereical triangle with sides a, b and c (each located opposite the points
;	of the same name.  Side c is the arc whose length is the great circle distance between A and B.
;
;	Snyder shows two calculations for the great circle distance; the first is lifted directly from the law
;	of cosines and the second is is a modified form used because the first is " not accurate in
;	practicle computation for values of c very close to 0."  The second is " exact, and is very accurate
;	in practice for values of c from 0 to nearly 180 degrees."
;
;
;	(5.3)	 Cos(c) = sin(LatA)*sin(LatB) + cos(LatA)*cos(LatB)*cos(LonB-LonA)
;
;	(5.3a)  Sin(c/2.) = $
;					SQRT( (sin((LatB-LatA)/2.)^2 + $
;					cos(LatA)* cos(LatB)*sin( (LonB-LonA)/2.)^2 )
;
;	Each of the above is solved for c and the multiplied by RADIUS to determine the
;	the great circle distance separating A and B.
;
;	The angle subtended at A is "the Azimuth (AZ) east of north which point B bears to point A."
;	Snyder shows three equations for calculation Azimuth.
;
;	(5-4) Sin(Az) =  sin(LonB-LonA) * cos(LatB) / sin(c)
;
;	(5-4a) cos(Az) = ( cos(LatA)*sin(LatB) - sin(LatA)*cos(LatB)*cos(LonB-LonA) ) / sin(c)
;
;	(5-4b) tan(Az) = cos(latB)*sin(LonB-LonA) /    $
;				(cos(LatA)*sin(LatB) - sin(LatA)*cos(LatB)*cos(LonB-LonA))
;
;	"Equation (5-4b) is usually preferred since it avoids the inaccuracies of finding
;	an arcsin near 90 degrees or an arccos near 0 degrees."
;
; REFERENCE
;  Snyder, John P. (1987), "Map Projections-A Working Manual",
;	 U.S. Geological Survey Professional Paper 1395,
;	 U.S.Government Printing Office, Washington, D.C.
;
; EXAMPLE:
;IDL> Lon = [-70,-68,-40]
;IDL> Lat = [45,52,10]
;IDL> print,greatcircle(lon,lat)
;       792994.72       9.9775669
;       5317158.2       141.35805
;      0.00000000      0.00000000
;IDL> print,greatcircle(lon,lat,/Clarke)
;       793003.36       9.9775669
;       5317216.1       141.35805
;      0.00000000      0.00000000
;
;
;
; MODIFICATION HISTORY:
;	This routine was inspired by Great_Circle (Liam Gumley, 1999) and Compass (Paul Ricchiazzi, 1993).
; 	Written by:	Ben Tupper  Dec 11, 1999
;	Pemaquid River Company
;	email pemaquidriver@tidewater.net
;	tel:  (207) 563 - 1048
;   248 Lower Round Pond Road
;	POB 106
;	Bristol, ME 04539-0106
;
;	9SEP2000 Changed output type to match input type, BT
;-




FUNCTION GreatCircle, LonVector, LatVector, $
	Radius = Radius,$
	Clarke = Clarke,$
	Radians = Radians

On_Error, 2

	;check that Lon and Lat have same number of elements
N = N_Elements(LonVector)
If N_elements(LatVector) NE N Then Message, 'Input Arguments must have same number of elements.'

Type = Size(LonVector, /Type)

Case Type of 
	5: BEGIN
				DTOR = !DPi/180.d
				Pi = !DPi
				One = 1.0d
			END
	ELSE:  BEGIN
				DTOR = !DtoR
				Pi = !Pi
				One = 1.0
						END
EndCase

	; convert the longitude/latitude values into radians unless otherwise told
If N_elements(Radians) EQ 0 Then Begin

	Lon = LonVector*DtoR
	Lat = LatVector*DtoR

EndIf Else Begin

	Lon = LonVector
	Lat = LatVector

EndElse

	; check that the lon/lat values are within quadrant bounds
MinLon = Min(Lon, Max = MaxLon)
MinLat = Min(Lat, Max = MaxLat)
If (MinLon LT (0.0 - Pi)) OR (MaxLon GT Pi) Then Message, 'Longitude must fall within -180.0 to 180.0'
If (MinLat LT (0.0-Pi/2.)) OR (MaxLat GT Pi/2.) Then Message, 'Latitude must fall within -90.0 to 90.0'

	;set the radius of the sphere
If n_elements(Radius) EQ 0 Then $
	If N_Elements(Clarke) EQ 0 Then $
		Radius =  6378137. * One  Else Radius = 6378206.4*One

RangeAz = Make_Array(2,N, /NoZero, Type = Type)

For i =0L, N-2L Do Begin

;	Sin(c/2.) = $
;					SQRT( (sin((LatB-LatA)/2.)^2 + $
;					cos(LatA)* cos(LatB)*sin( (LonB-LonA)/2.)^2 )

	SinX = $
		SQRT( (sin((Lat[i+1]-Lat[i])/2.)^2 + $
					cos(Lat[i])* cos(Lat[i+1])*sin( (Lon[i+1]-Lon[i])/2.)^2 ) )


	RangeAz[0,i]  = Radius * (2.0*One) *  asin(SinX)


;  tan(Az) = cos(latB)*sin(LonB-LonA) /    $
;				(cos(LatA)*sin(LatB) - sin(LatA)*cos(LatB)*cos(LonB-LonA))

	RangeAz[1,i] = atan(  cos(lat[i+1])*sin(Lon[i+1]-Lon[i]) /    $
			( cos(Lat[i])*sin(Lat[i+1]) - sin(Lat[i])*cos(Lat[i+1])*cos(Lon[i+1]-Lon[i]) ) )

EndFor

Index = Where(RangeAz[1,*] LT 0.0, Count)
If Count GT 0 Then RangeAz[1,Index] = Pi + RangeAz[1,Index]
	;convert back to degrees unless told otherwise
If N_elements(Radians) EQ 0 Then  RangeAz[1,*] = RangeAz[1,*] * (One/DTOR)



Return, RangeAz

END
