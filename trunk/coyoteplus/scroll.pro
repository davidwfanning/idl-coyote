PRO Scroll

Window, XSIZE=360, YSIZE=360
LoadCT, 4
TVImage, LoadData(7)

  ; Open a pixmap window 4 columns wide.

Window, 1, /Pixmap, XSize=4, YSize=360
FOR j=0,360/4 DO BEGIN

   ; Copy four columns on right of display into pixmap.

Device, Copy=[356, 0, 4, 360, 0, 0, 0]

   ; Make the display window the active window.

WSet, 0

   ; Move window contents over 4 columns.

Device, Copy=[0, 0, 356, 360, 4, 0, 0]

   ; Copy pixmap contents into display window on left.

Device, Copy=[0, 0, 4, 360, 0, 0, 1]
Wait, 0.125
ENDFOR
END
