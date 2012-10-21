PRO TwoSurf

   ; Create the data.

peak = Shift(Dist(20, 16), 10, 8)
peak = Exp( - (peak / 5) ^ 2)
saddle = Shift(peak, 6, 0) + Shift(peak, -6, 0) / 2B

   ; Load program colors.

Device, Get_Decomposed=state, Decomposed=0
colors = (!D.N_Colors < 256)/2
LoadCT, 1, NColors=colors
LoadCT, 3, NColors=colors, Bottom=colors-1

   ; Open a window and display first data set.

Window, 1, XSize=300, YSize=300
Set_Shading, Values=[0,colors-1]
Shade_Surf, peak, ZRange=[0.0, 1.2]

   ; Open a window and display second data set.

Window, 2, XSize=300, YSize=300
Set_Shading, Values=[colors, 2*colors-1]
Shade_Surf, saddle, ZRange=[0.0, 1.2]

   ; Go into Z-buffer.

thisDevice = !D.Name
Set_Plot, 'Z', /Copy

   ; Configure Z device.

Device, Set_Colors=2*colors, Set_Resolution=[300,300]

   ; Load objects in Z-buffer.

Set_Shading, Values=[0,colors-1]
Shade_Surf, peak, ZRange=[0.0, 1.2]
Set_Shading, Values=[colors, 2*colors-1]
Shade_Surf, saddle, ZRange=[0.0, 1.2], /NoErase

   ; Take a snap-shot of Z-buffer display plane.

picture = TVRD()

   ; Display the results.

Set_Plot, thisDevice
Window, 3, XSize=300, YSize=300
TV, picture
Device, Decomposed=state
END