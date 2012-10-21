PRO Dynamic_Menus_List_Events, event

; This event handler responds to animal selections from
; the list widget. The selected item is added to the
; droplist and pull-down menu widgets.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; How many animals currently on the list?

Widget_Control, event.id, Get_UValue=animals
numAnimals = N_Elements(animals)

   ; Get the selected animal.

selectedAnimal = animals[event.index]

   ; Special case if only one animal is left.

IF numAnimals EQ 1 AND event.index EQ 0 THEN BEGIN
   newAnimals = ['None']
   Widget_Control, info.listID, Set_Value=newAnimals, $
      Set_UValue=newAnimals
   Widget_Control, event.top, Set_UValue=info, /No_Copy
   RETURN
ENDIF

   ; All other cases handled here.

CASE event.index OF
   0: newAnimals = animals[1:numAnimals-1]
   numAnimals-1: newAnimals = animals[0:numAnimals-2]
   ELSE: newAnimals = [animals[0:event.index-1], animals[event.index+1:*]]
ENDCASE

   ; Update the animals in the list widget.

Widget_Control, info.listID, Set_Value=newAnimals, $
   Set_UValue=newAnimals

   ; Add the selected animal to the droplist widget by first destroying
   ; the old droplist widget and re-creating a new one.

Widget_Control, info.dropID, /Destroy, Get_UValue=currentAnimals
info.dropID = Widget_Droplist(info.dropbase, Title='Current Animal Menu', $
      Value=[currentAnimals, animals[event.index]], $
      UValue=[currentAnimals, animals[event.index]], $
      Event_Pro='Dynamic_Menus_Droplist_Events')
Widget_Control, info.dropID, Set_Droplist_Select=N_Elements(currentAnimals)

   ; Add the selected animal to the pull-down menu widget.
   ; The procedure here is to add a new button to the existing ones.

Widget_Control, info.menuID, Get_UValue=currentAnimals

   ; Build a new menu button.

currentAnimals = [currentAnimals, animals[event.index]]
lastIndex = N_Elements(currentAnimals) - 1
*info.buttons = [*info.buttons, Widget_Button(info.menuID, $
   Value=currentAnimals[lastIndex])]

   ; Update the animals in the menu list.

Widget_Control, info.menuID, Set_UValue=currentAnimals

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;--------------------------------------------------------



PRO Dynamic_Menus_Droplist_Events, event

; Handles events from the droplist widget.
; Which animal was selected? Print it.

Widget_Control, event.id, Get_UValue=animals
Print, 'Current DropList Selection: ', animals[event.index]
END
;--------------------------------------------------------



PRO Dynamic_Menus_DropMenu_Events, event

; Handles events from the pull-down menu widget.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which animal was selected? Print it.

Widget_Control, event.id, Get_Value=thisAnimal
Print, 'Pull-Down Menu Selection: ', thisAnimal

   ; Update the current selection in the droplist widget.

Widget_Control, info.dropID, Get_UValue=animals
thisAnimalIndex = Where(animals EQ thisAnimal)
Widget_Control, info.dropID, Set_Droplist_Select=thisAnimalIndex[0]

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;--------------------------------------------------------


PRO Dynamic_Menus_Cleanup, tlb
Widget_Control, tlb, Get_UValue=info
Ptr_Free, info.buttons
END
;--------------------------------------------------------



PRO Dynamic_Menus

; This program shows how to dynamically add to and
; delete items from list widgets, pull-down menu
; button, and droplist widgets.


   ; Create the widgets for the program.

tlb = Widget_Base(Column=1, /Base_Align_Center, $
   MBar=menuID, Title='Dynamic Menus')

   ; Create the list widget.

animals = ['Cat', 'Dog', 'Cow', 'Eagle', 'Fox']
listbase = Widget_Base(tlb, Column=1)
label = Widget_Label(listbase, Value='Add These Animals to Menus')
listID = Widget_List(listbase, YSize = 5, Value=animals, $
   UValue=animals, Event_Pro='Dynamic_Menus_List_Events')

   ; Create the droplist widget.

dropbase = Widget_Base(tlb, Column=1)
dropValues = 'Coyote'
dropID = Widget_Droplist(dropbase, Title='Current Animal Menu', $
   Value=dropValues, UValue=dropValues, $
   Event_Pro='Dynamic_Menus_Droplist_Events')

   ; Create the pull-down menu widget.

menubase = Widget_Base(tlb, /Base_Align_Center)
menuID = Widget_Button(menubase, Value='Select Animal Menu', $
   /Menu, UValue=['Coyote'], $
   Event_Pro='Dynamic_Menus_DropMenu_Events')
buttons = IntArr(1)
buttons[0] = Widget_Button(menuID, Value='Coyote')

   ; Center the top-level base.

Device, Get_Screen_Size=screenSize
xCenter = screenSize(0) / 2
yCenter = screenSize(1) / 2
geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2
Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

   ; Realize the widgets.

Widget_Control, tlb, /Realize

   ; Create the info structure to hold program information.

info={ dropID:dropID, $             ; The identifier of the droplist widget.
       dropbase:dropbase, $         ; The droplist base widget.
       menuID:menuID, $             ; The identifier of the pull-down menu widget.
       menubase:menubase, $         ; The pull-down menu base widget.
       listID:listID, $             ; The identifier of the list widget.
       listbase:listbase, $         ; The list base widget.
       buttons:Ptr_New(buttons) $   ; The IDs of the buttons in the pull-down menu.
     }
Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Run the program as a non-blocking widget.

XManager, 'dynamic_menus', tlb, /No_Block, Cleanup='Dynamic_Menus_Cleanup'
END
;--------------------------------------------------------
