Function All_Tags, structure, rootname

; This is a function that recursively searches through
; a structure tree, finding ALL of the structure's field names.
; It returns a pointer to an array of pointers, each pointing
; to the names of structure fields.

IF N_Elements(rootname) EQ 0 THEN rootname = '.' ELSE $
   rootname = StrUpCase(rootname) + '.'
names = Tag_Names(structure)
retValue = Ptr_New(rootname + names)

   ; If any of the fields are structures, report them too.

FOR j=0,N_Elements(names)-1 DO BEGIN
   ok = Execute('s = Size(structure.' + names[j] + ')')
   IF s[s[0]+1] EQ 8 THEN BEGIN
      newrootname = rootname + names[j]
      theseNames = Call_Function('All_Tags', $
         structure.(j), newrootname)
      retValue = [[retValue],[theseNames]]
   ENDIF
ENDFOR

RETURN, retValue
END
;-------------------------------------------------------------------



FUNCTION Get_Tags, structure, rootname

; This function returns the names of all structure fields
; in the structure as a string array. The names are given
; as valid structure names from the root structure name,
; which can be passed in along with the structure itself.

On_Error, 1

   ; Check parameters.

CASE N_Params() OF

   0: BEGIN
      Message, 'Structure argument is required.'
      ENDCASE
      
   1: BEGIN
      rootname = ''
      s = Size(structure)
      IF s[s[0]+1] NE 8 THEN $
         Message, 'Structure argument is required.'
      ENDCASE
      
   2: BEGIN
      s = Size(structure)
      IF s[s[0]+1] NE 8 THEN $
         Message, 'Structure argument is required.'
      s = Size(rootname)
      IF s[s[0]+1] NE 7 THEN $
         Message, 'Root Name parameter must be a STRING'
      ENDCASE
      
ENDCASE

tags = All_Tags(structure, rootname)

   ; Extract and free the first pointer.

retval = [*tags[0,0]]
Ptr_Free, tags[0,0]

   ; Extract and free the the rest of the pointers.
   
s = Size(tags)
FOR j=1,s[2]-1 DO BEGIN
   retval = [retval, *tags[0,j]]
   Ptr_Free, tags[0,j]
ENDFOR
Ptr_Free, tags

   ; Return the structure names.
   
RETURN, retval
END


   ; Main-level program to exercise Get_Tags.

d = {dog:'spot', cat:'fuzzy'}
c = {spots:4, animals:d}
b = {fast:c, slow:-1}
a = {cars:b, pipeds:c, others:'man'}
tags = Get_Tags(a)
s = Size(tags)
For j=0,s[1]-1 Do Print, tags[j]
END

