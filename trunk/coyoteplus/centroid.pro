FUNCTION Centroid, array

ndim = Size(array, /N_Dimensions)
IF ndim NE 2 THEN BEGIN
   Message, 'Array must be two-dimensional. Returning...', /Informational
   RETURN, -1
ENDIF

s = Size(array, /Dimensions)
totalMass = Total(array)

xcm = Total( Total(array, 2) * Indgen(s[0]) ) / totalMass
ycm = Total( Total(array, 1) * Indgen(s[1]) ) / totalMass

RETURN, [xcm, ycm]
END
