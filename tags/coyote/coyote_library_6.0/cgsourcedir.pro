; docformat = 'rst'
;
; NAME:
;   cgSourceDir
;
; PURPOSE:
;   The purpose of this function is to provide a portable way of finding
;   the source directory of a program distribution. The directory that is returned
;   is the directory in which the source file using cgSourceDir() resides.
;   The program is useful for distributing applications that have a large number
;   of files in specific program directories (e.g., source, resources, data, etc.).
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2014, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
;
;+
; The purpose of this function is to provide a portable way of finding
; the source directory of a program distribution. The directory that is returned
; is the directory in which the source file using cgSourceDir() resides.
; The program is useful for distributing applications that have a large number
; of files in specific program directories (e.g., source, resources, data, etc.).
;
; :Categories:
;    Utilities
;    
; :Keywords:
;    nomark: in, optional, type=boolean, default=0
;      Normally, the source directory name is returned with a final path
;      separator added to the end of the directory name. Setting this keyword
;      will suppress this final path separator.
;    oneup: in, optional, type=boolean, default=0
;       Setting this keyword will return the directory just above the source 
;       directory in the file hierarchy.
;    twoup: in, optional, type=boolean, default=0
;        Setting this keyword will return the directory two directories above the source
;        directory in the file hierarchy. 
;         
; :Examples:
;     Assume that your application files (and source programs) reside in this root directory::
;
;           ../app
;
;     You have placed a DATA directory immediately under the APP directiory, and a RESOURCES
;     directory immedately under the DATA directory. Your directory structure looks like this::
;
;           ../app                    ; Contains your application and source (*.pro) files.
;           ../app/data               ; Contains your application data files.
;           ...app/data/resources     ; Contains your application resource files.
;
;     The end user can install the APP directory wherever he or she likes. In your
;     program, you will identify the DATA and RESOURCES directory like this::
;
;            ; Get icon image in resources directory.
;            filename = Filepath(Root_Dir=cgSourceDir(), Subdirectory=['data','resources'], 'myicon.tif')
;
;            ; Get default image in data directory.
;            filename = Filepath(Root_Dir=cgSourceDir(), Subdirectory='data', 'ctscan.tif')
;
;     Alternatively, you might set up an application directory structure like this::
;
;           ../app                    ; Contains your application files.
;           ../app/source             ; Contains your application source (*.pro) files.
;           ../app/data               ; Contains your application data files.
;           ...app/data/resources     ; Contains your application resource files.
;
;     In this case, you would use the ONEUP keyword to find your data and resource files, like this::
;
;            ; Get icon image in resources directory.
;            filename = Filepath(Root_Dir=cgSourceDir(/ONEUP), Subdirectory=['data','resources'], 'myicon.tif')
;
;            ; Get default image in data directory.
;            filename = Filepath(Root_Dir=cgSourceDir(/ONEUP), Subdirectory='data', 'ctscan.tif')
;       
; :Author:
;    FANNING SOFTWARE CONSULTING::
;       David W. Fanning 
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: david@idlcoyote.com
;       Coyote's Guide to IDL Programming: http://www.idlcoyote.com
;
; :History:
;     Change History::
;       Written by: David W. Fanning, 23 November 2003. Based on program SOURCEROOT, written by
;         Jim Pendleton at RSI (http://www.rsinc.com/codebank/search.asp?FID=35).
;       Added ONEUP keyword. 10 December 2003. DWF.
;       Added TWOUP keyword. 8 June 2007. DWF.
;       Added NOMARK keyword. 8 June 2007. DWF.
;       Renamed from ProgramRootDir and updated to cgSourceDir. 27 Jan 2014. DWF.
;
; :Copyright:
;     Copyright (c) 2003-2014, Fanning Software Consulting, Inc.
;-
FUNCTION cgSourceDir, $
    NoMark=nomark, $
    OneUp=oneup, $
    TwoUp=twoup

   ; Return to caller on an error.
   On_Error, 2

   ; Get the current call stack.
   Help, Calls=callStack

   ; Get the name of the calling routine.
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]

   ; We don't know if the calling routine is a procedure or a function,
   ; and we don't have a way to get information without knowing this. So,
   ; we are going to try first to see if it is a procedure. If not, we
   ; will try it as a function. Unfortunately, if it is *not* a procedure,
   ; we will cause an error. We have to catch that and handle it silently.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      Message, /Reset
      thisRoutine = Routine_Info(callingRoutine, /Functions, /Source)
   ENDIF

   IF N_Elements(thisRoutine) EQ 0 THEN $
      thisRoutine = Routine_Info(callingRoutine, /Source)

   ; If there are no path separators, you are here.
   IF ( StrPos(thisRoutine.Path, Path_Sep()) ) EQ -1 THEN BEGIN
      CD, Current=thisDir
      sourcePath = FilePath(thisRoutine.Path, Root_Dir=thisDir)
   ENDIF ELSE BEGIN
      sourcePath = thisRoutine.Path
   ENDELSE

   ; Strip the root directory off the source path.
   root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)

   ; If ONEUP is set, then climb up the source directory by one directory. This will
   ; be the *second* path separator, since the root directory has a path separator
   ; as its end.
   IF Keyword_Set(oneup) THEN BEGIN
      i = Where( Byte(root) EQ (Byte(Path_Sep()))[0], count)

      IF count GE 2 THEN BEGIN
         sourcePath = StrMid(root, 0, StrLen(root)-1)
         root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)
      ENDIF
   ENDIF

   ; If TWOUP is set, then climb up the source directory by two directories. This will
   ; be the *third* path separator, since the root directory has a path separator
   ; as its end.
   IF Keyword_Set(twoup) THEN BEGIN
      i = Where( Byte(root) EQ (Byte(Path_Sep()))[0], count)

      IF count GE 3 THEN BEGIN
         sourcePath = StrMid(root, 0, StrLen(root)-1)
         root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)
         sourcePath = StrMid(root, 0, StrLen(root)-1)
         root = StrMid(sourcePath, 0, StrPos(sourcePath, Path_Sep(), /Reverse_Search) + 1)
      ENDIF
   ENDIF

   ; Remove last path separation mark, if requested.
   IF Keyword_Set(nomark) THEN RETURN, StrMid(root, 0, StrLen(root)-1) ELSE RETURN, root
END

