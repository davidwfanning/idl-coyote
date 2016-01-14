; docformat = 'rst'
;
; NAME:
;   cgSymSize
;
; PURPOSE:
;   This function returns a normalization factor that acts to create symbols that appear to
;   be the same size by normalizing their filled area. This factor should be multiplied by
;   the symbol size to create the proper size for the symbol.
;
;******************************************************************************************;
;                                                                                          ;
;  Copyright (c) 2015, by Fanning Software Consulting, Inc. All rights reserved.           ;
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
; This function returns a normalization factor that acts to create symbols that appear to
; be the same size by normalizing their filled area. This factor should be multiplied by
; the symbol size to create the proper size for the symbol. The function works in association
; with cgSymCat for selecting plotting symbols. The normaization factors were derived originally
; by Mats Löfdahl using the equation: factor = 1/Sqrt(area).
;
; :Categories:
;    Graphics
;    
; :Params:
;    theSymbolIndex: in, required, type=integer
;       The number of the symbol you wish to use. Possible values are::
;          0  : No symbol.                         NONE
;          1  : Plus sign.                         PLUSSIGN
;          2  : Asterisk.                          ASTERISK
;          3  : Dot (period).                      DOT
;          4  : Open diamond.                      OPENDIAMOND
;          5  : Open upward triangle.              OPENUPTRIANGLE
;          6  : Open square.                       OPENSQUARE
;          7  : X.                                 X
;          8  : Defined by the user with USERSYM.  USERSYM
;          9  : Open circle.                       OPENCIRCLE
;         10  : Histogram style plot.              HISTOGRAM
;         11  : Open downward triangle.            OPENDOWNTRIANGLE
;         12  : Open rightfacing triangle.         OPENRIGHTTRIANGLE
;         13  : Open leftfacing triangle.          OPENLEFTTRIANGLE
;         14  : Filled diamond.                    FILLEDDIAMOND
;         15  : Filled square.                     FILLEDSQUARE
;         16  : Filled circle.                     FILLEDCIRCLE
;         17  : Filled upward triangle.            FILLEDUPTRIANGLE
;         18  : Filled downward triangle.          FILLEDDOWNTRIANGLE
;         19  : Filled rightfacing triangle.       FILLEDRIGHTTRIANGLE
;         20  : Filled leftfacing triangle.        FILLEDLEFTTRIANGLE
;         21  : Hourglass.                         HOURGLASS
;         22  : Filled Hourglass.                  FILLEDHOURGLASS
;         23  : Bowtie.                            BOWTIE
;         24  : Filled bowtie.                     FILLEDBOWTIE
;         25  : Standing Bar.                      STANDINGBAR
;         26  : Filled Standing Bar.               FILLEDSTANDINGBAR
;         27  : Laying Bar.                        LAYINGBAR
;         28  : Filled Laying Bar.                 FILLEDLAYINGBAR
;         29  : Hat up.                            HATUP
;         30  : Hat down.                          HATDOWN
;         31  : Hat right.                         HATRIGHT
;         32  : Hat down.                          HATDOWN
;         33  : Big cross.                         BIGCROSS
;         34  : Filled big cross.                  FILLEDBIGCROSS
;         35  : Circle with plus.                  CIRCLEWITHPLUS
;         36  : Circle with X.                     CIRCLEWITHX
;         37  : Upper half circle.                 UPPERHALFCIRCLE
;         38  : Filled upper half circle.          FILLEDUPPERHALFCIRCLE
;         39  : Lower half circle.                 LOWERHALFCIRCLE
;         40  : Filled lower half circle.          FILLEDLOWERHALFCIRCLE
;         41  : Left half circle.                  LEFTHALFCIRCLE
;         42  : Filled left half circle.           FILLEDLEFTHALFCIRCLE
;         43  : Right half circle.                 RIGHTHALFCIRCLE
;         44  : Filled right half circle.          FILLEDRIGHTHALFCIRCLE
;         45  : Star.                              STAR
;         46  : Filled star.                       FILLEDSTAR
;         
;    defaultSymSize: in, optional, type=float
;        The default symbol size. Uses !P.SymSize by default, if not defined.
;
; :Examples:
;
;    To see difference in symbol size::
;        !P.SymSize = 2.5
;        cgDisplay, WID=0, Title = 'Usual Symbol Sizes'
;        cgPlot, [1], /NoData
;        cgPlotS, [0.2,0.5, 0.8], [0.5, 0.5, 0.5], PSym=-17        ; Usual filled triangle size
;        cgPlotS, [0.2,0.5, 0.8], [0.25, 0.25, 0.25], PSym=-16     ; Usual filled circle size
;        cgPlotS, [0.2,0.5, 0.8], [0.75, 0.75, 0.75], PSym=-15     ; Usual filled box size
;        
;        cgDisplay, WID=1, Title = 'Normalized Symbol Sizes'
;        cgPlot, [1], /NoData
;        cgPlotS, [0.2,0.5, 0.8], [0.5, 0.5, 0.5], PSym=-17, SymSize=cgSymSize(17)        ; Normalized filled triangle size
;        cgPlotS, [0.2,0.5, 0.8], [0.25, 0.25, 0.25], PSym=-16, SymSize=cgSymSize(16)     ; Normalized filled circle size
;        cgPlotS, [0.2,0.5, 0.8], [0.75, 0.75, 0.75], PSym=-15, SymSize=cgSymSize(15)     ; Normalized filled box size
;        !P.SymSize = 1.0
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
;    Change History::
;       Written by David W. Fanning, 17 April 2015, from an IDL newsgroup suggestion from Mats Löfdahl.
;
; :Copyright:
;     Copyright (c) 2015, Fanning Software Consulting, Inc.
;-
FUNCTION cgSymSize, theSymbolIndex, defaultSymSize

   Compile_Opt idl2

   ; Error handling.
   On_Error, 2 ; Return to caller
   
   ; Must have a symbol index, or just return.
   IF N_Elements(theSymbolIndex) EQ 0 THEN RETURN, 1.00
   theSymbolIndex = Long(theSymbolIndex)

   ; Set a default size, if you need one.
   SetDefaultValue, defaultSymSize, !P.SymSize
   
   ; Find the normalization factor.
   CASE theSymbolIndex OF
       0: normalizationFactor = 1.00000
       1: normalizationFactor = 1.25002
       2: normalizationFactor = 0.884060
       3: normalizationFactor = 1.00000
       4: normalizationFactor = 1.25002
       5: normalizationFactor = 1.24985
       6: normalizationFactor = 0.884060
       7: normalizationFactor = 0.884060
       8: normalizationFactor = 1.00000
       9: normalizationFactor = 1.00000
      10: normalizationFactor = 1.00000
      11: normalizationFactor = 1.25013
      12: normalizationFactor = 1.24987
      13: normalizationFactor = 1.24985
      14: normalizationFactor = 1.25002
      15: normalizationFactor = 0.884060
      16: normalizationFactor = 1.00000
      17: normalizationFactor = 1.24985
      18: normalizationFactor = 1.25013
      19: normalizationFactor = 1.24987
      20: normalizationFactor = 1.24985
      21: normalizationFactor = 1.24961
      22: normalizationFactor = 1.24961
      23: normalizationFactor = 1.24947
      24: normalizationFactor = 1.24947
      25: normalizationFactor = 1.24988
      26: normalizationFactor = 1.24988
      27: normalizationFactor = 1.24991
      28: normalizationFactor = 1.24991
      29: normalizationFactor = 1.25002
      30: normalizationFactor = 1.25002
      31: normalizationFactor = 1.25002
      32: normalizationFactor = 1.25002
      33: normalizationFactor = 1.23751
      34: normalizationFactor = 1.23751
      35: normalizationFactor = 1.00000
      36: normalizationFactor = 1.00000
      37: normalizationFactor = 1.00000
      38: normalizationFactor = 1.00000
      39: normalizationFactor = 1.00000
      40: normalizationFactor = 1.00000
      41: normalizationFactor = 1.00000
      42: normalizationFactor = 1.00000
      43: normalizationFactor = 1.00000
      44: normalizationFactor = 1.00000
      45: normalizationFactor = 1.53805
      46: normalizationFactor = 1.53805
      ELSE: Message, 'No symbol with this symbol index found.'
    ENDCASE

    ; Return the symbol size.
    RETURN, normalizationFactor * defaultSymSize
    
END