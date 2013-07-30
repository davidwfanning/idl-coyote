PRO PolarPlot, radius, angle, _Extra=extra

   ; Fake data if needed.

IF N_Params() EQ 0 THEN BEGIN
   angle = ((Randomu(seed, 360)*360) - 180) * !DtoR
   radius = Randomu(seed, 360) * 100
ENDIF

   ; Load plot colors.

TVLCT, [100, 255, 0], [100, 255, 255], [100, 0, 0], 1
Device, Decomposed=0, Get_Decomposed=theState

   ; Establish plot coordinates.

Plot, radius, angle, /Polar, XStyle=5, YStyle=5, $
   /NoData, Background=1, Position=Aspect(1.0)

   ; Draw axis through center.

Axis, /XAxis, 0, 0, Color=2
Axis, /YAxis, 0, 0, Color=2

   ; Plot data.

OPlot, radius, angle, PSym=2, /Polar, Color=3

   ; Draw 25 and 75 percent circles.

dataMax = Max(radius)
percent25 = Circle(0, 0, 0.25*dataMax)
percent75 = Circle(0, 0, 0.75*dataMax)
PlotS, percent25, Color=2
PlotS, percent75, Color=2
Device, Decomposed=theState
END