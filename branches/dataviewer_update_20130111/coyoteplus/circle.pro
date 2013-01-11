FUNCTION CIRCLE, xcenter, ycenter, radius
points = (2 * !PI / 99.0) * FINDGEN(100)
x = xcenter + radius * COS(points )
y = ycenter + radius * SIN(points )
RETURN, TRANSPOSE([[x],[y]])
END

