FUNCTION WidgetFont, DEFAULT=default

   ; Build a small widget to determine the current 
   ; and default widget fonts.
   base = Widget_Base(MAP=0)
   button = Widget_Button(base, Value='TEST')
   
   ; Checking before realization gives default font.
   defaultFont = Widget_Info(button, /FONTNAME)
   
   ; Checking after realization gives current font.
   Widget_Control, base, /REALIZE
   currentFont = Widget_Info(button, /FONTNAME)
   
   ; Clean up.
   Widget_Control, base, /DESTROY

   IF Keyword_Set(default) THEN $
        RETURN, defaultFont ELSE $
        RETURN, currentFont
    
END