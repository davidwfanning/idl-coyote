FUNCTION GetDataType, inputType, EXTERNAL=external

      ; Note: Because this is to be used primarily with

      ON_ERROR, 2 ; Return to caller
      
      ; Check input parameter.
      IF N_Elements(inputType) EQ 0 THEN Message, 'INPUTTYPE argument is required.'
      IF Size(inputType, /TNAME) NE 'STRING' THEN Message, 'INPUTTYPE argument must be a string.'

      ; Possible synonyms.
      type = StrCompress(StrUpCase(inputType), /ALL)
      IF type EQ 'SHORT' THEN type = 'INT'
      IF type EQ 'INTEGER' THEN IF Keyword_Set(external) THEN type = 'LONG' ELSE type = 'INT'
      IF type EQ 'LONGINTEGEER' THEN type = 'LONG'
      IF type EQ 'UNSIGNEDINTEGER' THEN IF Keyword_Set(external) THEN type = 'ULONG' ELSE type = 'UINT'
      IF type EQ 'UNSIGNEDLONG' THEN type = 'ULONG'
      IF type EQ 'BINARY' THEN type = 'BYTE'
      
      CASE type OF
         'BYTE': type = 1
         'INT': type = 2
         'LONG': type = 3
         'FLOAT': type = 4
         'DOUBLE': type = 5
         'COMPLEX': type = 6
         'STRING': type = 7
         'DCOMPLEX': type = 9
         'UINT': type = 12
         'ULONG': type = 13
         'LONG64': type = 14
         'ULONG64': type = 15
         ELSE: Message, 'Unable to convert input to type: ' + StrUpCase(type)
      ENDCASE

    RETURN, type
    
END