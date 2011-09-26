;+
; NAME:
;       AdjustPosition
;
; PURPOSE:
;
;       This is a program for interactively adjusting the plot position
;       coordinates. The result of the function is a four-element floating
;       point array of normalized coordinates, suitable for passing to the
;       POSITION keyword of most IDL graphics commands.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       position = AdjustPosition(startingPosition)
;
; OPTIONAL INPUTS:
;
;       startingPosition - A four-element array of normalized coordinates
;            of the form [x0, y0, x1, y1].
;
; OUTPUTS:
;
;       position - The adjusted plot position. A four-element array of normalized coordinates.
;
; INPUT KEYWORDS:
;
;       GROUP_LEADER - The group leader of this program. This keyword
;            is required to ensure modal operation when calling from
;            another widget program.
;
;       TITLE - The title of the window. "Adjust Plot Position in Window..." by default.
;
;       XOFFSET - The X offset of the program on the display. Calculated from the
;            upper left-hand corner of the display.
;
;       YOFFSET - The Y offset of the program on the display. Calculated from the
;            upper left-hand corner of the display.
;
; OUTPUT KEYWORDS:
;
;       CANCEL - Returns a 1 if the user selects the Cancel button. Returns 0 otherwise.
;            Note that if the use cancels, the "position" parameter is set to the value of
;            the "startingPosition" parameter.
;
; DEPENDENCIES:
;
;       Reqires FSC_FIELD and FSC_PLOTWINDOW from the Coyote Library:
;
;                     http://www.dfanning.com/programs/fsc_field.pro
;                     http://www.dfanning.com/programs/fsc_plotwindow.pro
;
; MODIFICATION HISTORY:
;
;       Written by David Fanning, March 2001.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO AdjustPosition_CenterTLB, tlb

; Center the top-level base widget.

screenSize = Get_Screen_Size()
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;------------------------------------------------------------------------------------------



PRO AdjustPosition_PlotEvents, event

; This event handler responds to events generated by manipulating
; the plot position interactively with the mouse.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the current position from the window.

position = info.plotWindow->GetPosition()

   ; Update the text widgets.

info.p0->Set_Value, position[0]
info.p1->Set_Value, position[1]
info.p2->Set_Value, position[2]
info.p3->Set_Value, position[3]

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------------------



PRO AdjustPosition_FieldEvents, event

; The event handler responds to events from the text widgets.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the values from the text widgets.

x0 = info.p0->Get_Value()
y0 = info.p1->Get_Value()
x1 = info.p2->Get_Value()
y1 = info.p3->Get_Value()

   ; Make sure the values are ordered properly. There must
   ; be at least 0.05 unit between values.

IF x0 GT x1 THEN BEGIN
   temp = x0
   x0 = x1
   x1 = temp
ENDIF
IF x1-x0 LT 0.05 THEN x0 = 0 > (x1 - 0.05)
info.p0->Set_Value, x0
info.p2->Set_Value, x1

IF y0 GT y1 THEN BEGIN
   temp = y0
   y0 = y1
   y1 = temp
ENDIF
IF y1-y0 LT 0.05 THEN y0 = 0 > (y1 - 0.05)
info.p1->Set_Value, y0
info.p3->Set_Value, y1

   ; Set the position in the display.

position = [x0, y0, x1, y1]
info.plotWindow->SetWindowSize, position

   ; Update the form info pointer.

*(info.ptr) = {cancel:0, position:position}

Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------------------



PRO AdjustPosition_ButtonEvents, event

; This event handler responds to Cancel and Accept buttons.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get the latest plot position.

position = info.plotWindow->GetPosition()

   ; Which button is it? Load the pointer with the correct
   ; information.

Widget_Control, event.id, Get_Value=buttonValue
CASE buttonValue OF
   'Cancel': *(info.ptr) = {cancel:1, position:position}
   'Accept': *(info.ptr) = {cancel:0, position:position}
ENDCASE

Widget_Control, event.top, /Destroy
END ;------------------------------------------------------------------------------------------



FUNCTION AdjustPosition, currentPosition, Group_Leader=group_leader, $
   Title=title, Cancel=cancelled, XOffset=xoffset, YOffset=yoffset

   ; Check keywords.

IF N_Elements(currentPosition) EQ 0 THEN currentPosition = [0.15, 0.15, 0.9, 0.9]
IF N_Elements(title) EQ 0 THEN title = "Adjust Plot Position in Window..."

   ; Create TLB as modal, if possible. Otherwise, will have to
   ; rely on command line blocking.

IF N_Elements(group_leader) NE 0 THEN BEGIN
   tlb = Widget_Base(Title=title, Modal=1, Group_Leader=group_leader, $
      Column=1, Base_Align_Center=1)
ENDIF ELSE BEGIN
   tlb = Widget_Base(Title=title, Column=1, Base_Align_Center=1)
ENDELSE

   ; Create the plot window widget.

plotwindow = FSC_PlotWindow(tlb, PageSize='DISPLAY', WindowSize=currentPosition, $
   Event_Pro='AdjustPosition_PlotEvents', /Color)

   ; Other widgets.

label = Widget_Label(tlb, Value='Adjust plot position with MOUSE. Use middle button to CENTER.')
rowbaseID = Widget_Base(tlb, Row=2)
f = FSC_Field(rowbaseID, Title='x0: ', Value=currentPosition[0], Decimal=3, Object=p0, $
   /CR_Only, Event_Pro='AdjustPosition_FieldEvents')
f = FSC_Field(rowbaseID, Title='y0: ', Value=currentPosition[1], Decimal=3, Object=p1, $
   /CR_Only, Event_Pro='AdjustPosition_FieldEvents')
f = FSC_Field(rowbaseID, Title='x1: ', Value=currentPosition[2], Decimal=3, Object=p2, $
   /CR_Only, Event_Pro='AdjustPosition_FieldEvents')
f = FSC_Field(rowbaseID, Title='y1: ', Value=currentPosition[3], Decimal=3, Object=p3, $
   /CR_Only, Event_Pro='AdjustPosition_FieldEvents')

buttonBaseID = Widget_Base(tlb, Row=1, Event_Pro='AdjustPosition_ButtonEvents')
b = Widget_Button(buttonBaseID, Value='Cancel')
b = Widget_Button(buttonBaseID, Value='Accept')

   ; Create a pointer to collect position information. Set for CANCEL.

ptr = Ptr_New({Cancel:1})

   ; Position the widget on the display.

IF (N_Elements(xoffset) EQ 0) AND (N_Elements(yoffset) EQ 0) THEN AdjustPosition_CenterTLB, tlb ELSE BEGIN
   IF N_Elements(xoffset) EQ 0 THEN xoffset = 100
   IF N_Elements(yoffset) EQ 0 THEN yoffset = 100
   Widget_Control, tlb, XOffset=xoffset, YOffset=yoffset
ENDELSE

Widget_Control, tlb, /Realize

   ; Create and store info structure.

info = {p1:p1, p2:p2, p3:p3, p0:p0, ptr:ptr, plotWindow:plotWindow}
Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Block here until the widget is destroyed.

XManager, 'adjustposition', tlb

   ; Set the cancel flag. Return starting position.

cancelled = (*ptr).cancel
IF cancelled THEN BEGIN
   Ptr_Free, ptr
   RETURN, currentPosition
ENDIF

   ; Free pointer and return the new position.

thisPosition = (*ptr).position
Ptr_Free, ptr

RETURN, thisPosition
END