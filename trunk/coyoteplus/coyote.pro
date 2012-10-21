FUNCTION COYOTE, target_dir, ALL=all

   ; The purpose of this function is to find the "coyote" directory
   ; and return its path. The "coyote" directory can be named whatever
   ; you like. If no directory is found that contains the target directory
   ; string, the function returns a null string.

   ; If the ALL keyword is used, all directories containing the target
   ; directory are returned. Otherwise, only the first directory containing
   ; the target directory string is returned.

ON_ERROR, 1
IF N_PARAMS() EQ 0 THEN target_dir = 'COYOTE'
type = SIZE(target_dir)
IF type(type(0) + 1) NE 7 THEN $
   MESSAGE, 'Function argument must be of type STRING.'
all = KEYWORD_SET(all)

   ; Check current directory first.

CD, Current=thisDir
IF STRPOS(STRUPCASE(thisDir), STRUPCASE(target_dir)) GT 0 THEN BEGIN
   IF NOT all THEN RETURN, thisDir
ENDIF

   ; Look in !Path directories.

pathDir = EXPAND_PATH(!Path, /Array)
s = SIZE(pathDir)

   ; Nothing?

IF s(1) LT 1 THEN BEGIN
   MESSAGE, 'No directories meeting specification "' +  $
      target_dir + '" found...', /Informational
   RETURN, ''
ENDIF

   ; Check entire path for target directory.

check = STRPOS(STRUPCASE(pathDir(*)), STRUPCASE(target_dir))
found = WHERE(check NE -1)
IF found(0) GE 0 THEN BEGIN

   ; Return all the matches or just the first?

   IF all THEN RETURN, pathDir(WHERE(check NE -1)) ELSE $
      RETURN, (pathDir(WHERE(check NE -1)))(0)
ENDIF

   ; No matches.

MESSAGE, 'No directories meeting specification "' +  $
   target_dir + '" found...', /Informational
RETURN, ''
END
