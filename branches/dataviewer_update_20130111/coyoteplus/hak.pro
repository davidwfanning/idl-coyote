    PRO HAK, NoPrompt=noprompt

    ; Clear typeahead buffer before input
    WHILE Get_KBRD(0) do junk=1

    ; Get input
    IF Keyword_Set(noprompt) THEN BEGIN
        junk = Get_KBRD(1)
    ENDIF ELSE BEGIN
        Print, 'Hit any key to continue...'
        junk = Get_KBRD(1)
        IF StrUpCase(junk) EQ 'Q' THEN RETURN
        Print, 'Continuing...
    ENDELSE

    ; Clear typeahead buffer after input
    WHILE Get_KBRD(0) do junk=1

    END

