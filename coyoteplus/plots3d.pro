Pro PlotS3D

   ; Create some data.
   
time = Findgen(200)

x = Sin(time/2.5) / Exp(time/100) ; Damped Sine Curve
y = Sin(time/2.5) / Exp(time/100) ; Damped Sine Curve
z = Sin(time/5.0 * !DtoR) ; Function of the time
x = Reverse(x) ; Reverse the x vector
minx = Min(x, Max=maxx)
miny = Min(y, Max=maxy)

   ; Open a window. Load a yellow color for the plot.
   
Device, Get_Decomposed=theState, Decomposed=0
Window, /Free, XSize=450, YSize=450
TVLCT, 255, 255, 0, 1

   ; Set up the 3D space. Draw axes for the plot.
   ; Save the 3D transformation in !P.T.
   
Surface, Dist(5), XRange=[minx, maxx], YRange=[miny, maxy], $
   ZRange=[0,1], XStyle=1, YStyle=1, ZStyle=1, /NoData, /Save

   ; Plot the data in 3D space (use T3D keyword).
   
PlotS, x, y, z, /T3D, Color=1

   ; Create an animation of the data over time.
   ; Let's do 50 frames.
   
XInterAnimate, Set=[250,250,50], /Showload
FOR j=1,49 DO BEGIN
   Surface, Dist(5), XRange=[minx, maxx], YRange=[miny, maxy], $
      ZRange=[0,1], XStyle=1, YStyle=1, ZStyle=1, /NoData, /Save
   PlotS, x((j-1)*4:j*4), y((j-1)*4:j*4), z((j-1)*4:j*4), /T3D, Color = 1
   XInterAnimate, Frame=j, Window=!D.Window
ENDFOR
XInterAnimate
Device, Decomposed=theState
END
