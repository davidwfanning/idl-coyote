PRO buttontypes

tlb = Widget_Base(Title='Push-Buttons', Column=1, Scr_XSize=200)
button1 = Widget_Button(tlb, Value='Coyote')
button2 = Widget_Button(tlb, Value='Cow')
button3 = Widget_Button(tlb, Value='Dog')
button4 = Widget_Button(tlb, Value='Pig')
button5 = Widget_Button(tlb, Value='Sheep')

Widget_Control, tlb, /Realize

tlb = Widget_Base(Title='Checkbox Buttons', Column=1, Scr_XSize=200, /NonExclusive)
button1 = Widget_Button(tlb, Value='Coyote')
button2 = Widget_Button(tlb, Value='Cow')
button3 = Widget_Button(tlb, Value='Dog')
button4 = Widget_Button(tlb, Value='Pig')
button5 = Widget_Button(tlb, Value='Sheep')

Widget_Control, tlb, /Realize

tlb = Widget_Base(Title=' Radio Buttons', Column=1, Scr_XSize=200, /Exclusive)
button1 = Widget_Button(tlb, Value='Coyote')
button2 = Widget_Button(tlb, Value='Cow')
button3 = Widget_Button(tlb, Value='Dog')
button4 = Widget_Button(tlb, Value='Pig')
button5 = Widget_Button(tlb, Value='Sheep')

Widget_Control, tlb, /Realize
END