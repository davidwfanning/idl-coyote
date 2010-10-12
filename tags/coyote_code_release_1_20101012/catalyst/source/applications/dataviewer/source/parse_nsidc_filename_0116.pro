;*****************************************************************************************************
;+
; NAME:
;       PARSE_NSIDC_FILENAME_0116
;
; PURPOSE:
;
;       The purpose of this routine is to read and obtain information about
;       NSIDC daily ice motion vectors (nsidc-0116).
;       
; AUTHOR:
;
;       David W. Fanning, Ph.D
;       National Snow and Ice Data Center (NSIDC)
;       NSIDC/CIRES University of Colorado
;       Boulder, CO 80309
;       E-Mail: fanning@nsidc.org
;
; CATEGORY:
;
;       File Reading.
;
; SYNTAX:
;
;       image = Parse_NSIDC_Filename_0116(filename)
;       
; RETURN_VALUE:
; 
;       image:       The gridded image data in the file.
;       
; ARGUMENTS:
; 
;       filename:    The name of an NSIDC-0116 image file. 
;                    
; OUTPUT_KEYWORDS:
; 
;        INFO:       An output structure containing information about the image. See the
;                    documentation for PARSE_NSIDC_FILENAME for an example.
; 
;        SUCCESS:    Set to 1 if the file was successfully read, otherwise to 0.
;       
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 10 June 2010.
;-
;******************************************************************************************;
;  Copyright (c) 2010, Regents of the University of Colorado. All rights reserved.         ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      1. Redistributions of source code must retain the above copyright                   ;
;         notice, this list of conditions and the following disclaimer.                    ;
;      2. Redistributions in binary form must reproduce the above copyright                ;
;         notice, this list of conditions and the following disclaimer in the              ;
;         documentation and/or other materials provided with the distribution.             ;
;      3. Neither the name of the Regents of the University of Colorado nor the names      ;
;         of its contributors may be used to endorse or promote products derived from      ;
;          this software without specific prior written permission.                        ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERISTY OF COLORADO ''AS IS'' AND    ;
;  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED           ;
;  WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.     ;
;  IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF COLORADO BE LIABLE FOR ANY DIRECT,   ;
;  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT      ;
;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR      ;         
;  PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,        ;
;  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      ;
;  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE              ;
;  POSSIBILITY OF SUCH DAMAGE.                                                             ;
;******************************************************************************************;
FUNCTION Parse_NSIDC_Filename_0116, filename, INFO=info, SUCCESS=success

   COMPILE_OPT idl2

   ; Simple error handling. Return to caller.
   ON_ERROR, 2

   ; Check parameters.
   success = 0 ; Assume no success
   IF N_Elements(filename) EQ 0 THEN Message, "A filename is a required input parameter."

   ; Parse the root file name to determine the parameters that need to be set appropriately.
   ; If this is not a compressed file, with extension .gz, then we will have to add the extension
   ; back to the filename and set the extension to a null string.
   root_name = FSC_Base_Filename(filename, DIRECTORY=theDirectory, EXTENSION=theExtension)
   IF StrUpCase(theExtension) NE 'GZ' THEN BEGIN
        root_name = root_name + '.' + theExtension
        theExtension = ''
   ENDIF
   
   ; What kind of file are we dealing with here?
   parts = StrSplit(root_name, '.', /EXTRACT)
   CASE StrUpCase(parts[1]) OF
        'VECT': BEGIN
            dataType = 'VECTOR'
            IF StrUpCase(parts[2]) NE 'GRID' THEN BEGIN
                Message, 'DataViewer cannot deal with raw ice motion vector files at this time.'
            ENDIF
            region = StrUpCase(parts[4])
            vectorLength = 12500.0
            xlabelOffset = -5800000.0
;            vectorType = StrUpCase(parts[2])
;            region = StrUpCase(parts[4])
;            rows = File_Lines(filename)
;            data = Fltarr(5, rows-2)
;            header = StrArr(2)
;            OpenR, lun, filename, /GET_LUN
;            ReadF, lun, header
;            testValue = 0.0
;            ReadS, header[1], testValue
;            IF Abs(rows - testValue) GT 2 THEN BEGIN
;                header = ""
;                data = FltArr(5, rows-1)
;            ENDIF
;            Point_Lun, lun, 0
;            ReadF, lun, header, data
;            Free_Lun, lun
            END
            
        'MEAN': BEGIN
            dataType = 'MEAN'
            CASE StrUpCase(parts[2]) OF
                'WEEK': meanType = "WEEK"
                'JAN-DEC': meanType = "YEAR"
                ELSE: meanType = "MONTH'
            ENDCASE
            IF meanType EQ 'WEEK' THEN region = StrUpCase(parts[5]) ELSE region = StrUpCase(parts[4])
            vectorLength = 37500.0
            xLabelOffset = -5500000.0
            END
   ENDCASE
   
   ; Gridded data size depends on region.
   CASE StrUpCase(region) OF
            
      'N': data = IntArr(3, 361, 361)
      'S': data = IntArr(3, 321, 321)
                
   ENDCASE
            
   ; Read the vector data.
   OpenR, lun, filename, /GET_LUN, /SWAP_IF_BIG_ENDIAN
   ReadU, lun, data
   Free_Lun, lun
   u = Reform(data[0,*,*] / 10.0)
   v = Reform(data[1,*,*] / 10.0)
   value = Reform(data[2,*,*] / 10.0)
   
   ; Create X and Y arrays to go with these values.
   s = Size(data, /DIMENSIONS)
   x = Scale_Vector(Dindgen(s[1]), -4524688.5, 4524688.5)
   y = Scale_Vector(Dindgen(s[2]), -4524688.5, 4524688.5)
   lon = Rebin(x, s[1], s[2])
   lat = Rebin(Reform(Reverse(y), 1, s[2]), s[1], s[2])

   ; No color change allowed.
   colorChangeAllowed = 0
   colorChangeNColors = 256 ; Must be > 1 for subsequent processing.
   
   ; Create map projection information.  Polar views are Lambert Equal Area Azimuthal.
   CASE region OF
        'N': BEGIN
               mapCoord = Obj_New('MAPCOORD', 111, SPHERE_RADIUS=6371228L, CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=90.0, LIMIT=[30, -180., 90.0, 180.0])
               lats = Indgen(6)*10+30
               lons = Findgen(11) * 36 
            END
        'S': BEGIN
               mapCoord = Obj_New('MAPCOORD', 111, SPHERE_RADIUS=6371228L, CENTER_LONGITUDE=0.0, $
                    CENTER_LATITUDE=-90.0, LIMIT=[-90.,-180., -30, 180.0])
               lats = -Reverse(Indgen(6)*10+30)
               lons = Findgen(11) * 36 
            END
   ENDCASE
   
   ; Define the grid and outlines for this image.
   grid_color = CatGetDefault('DATAVIEWER_GRID_COLOR')
   vector_color = CatGetDefault('DATAVIEWER_VECTOR_COLOR')
   outline_color = CatGetDefault('DATAVIEWER_OUTLINE_COLOR')
   landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
   outline = Obj_New('Map_Outline', MAP_OBJECT=mapCoord, LAND_COLOR=landmask_color, $
        /FILL, COLOR=outline_color)
   grid = Obj_New('Map_Grid', MAP_OBJECT=mapCoord, COLOR=grid_color, LATS=lats, LONS=lons)
   mapCoord -> SetProperty, GRID_OBJECT=grid, OUTLINE_OBJECT=outline
   
   ; Plotting every vector is excessive. Let's plot 5000 randomly located vectors.
   reduce = Long(Randomu(seed, 5000) * s[1]*s[2])
   lon = lon[reduce]
   lat = lat[reduce]
   u  = u[reduce]
   v = v[reduce]
   value = value[reduce]
   i = where(value GT 0, count)
   IF count GT 0 THEN BEGIN
       lon = lon[i]
       lat = lat[i]
       u = u[i]
       v = v[i]
       value = value[i]
   ENDIF
   
   ; Create a vector object for this data set.
   vectorObj = Obj_New('Map_Vector', lon, lat, u, v, mapCoord, /UVCOORDS, $
        LENGTH=vectorLength, COLOR=vector_color)
   mapCoord -> SetOverlay, vectorObj, 2

   ; Create a legend for the plot.
   vectLegend = Obj_New('Map_Vector', -6000000.0, -6000000.0, 10, 0, mapCoord, /UVCOORDS, $
        LENGTH=vectorLength, COLOR=vector_color)
   textLegend = Obj_New('TextLine', X=xLabelOffset, Y=-6050000.0, '10 cm/sec', $
        COORD_OBJECT=mapCoord, COLOR=vector_color, Charsize=0.75, FONT=-1, ALIGNMENT=0, $
        /UVCOORDS)
   mapCoord -> SetOverlay, vectLegend, 3
   mapCoord -> SetOverlay, textLegend, 4
 
   ; Create an info structure about the image
   info = {directory :   theDirectory, $
           filename:     filename, $
           extension:    theExtension, $
           nsidc_tag:    'nsidc_0116', $
           colorChangeAllowed: colorChangeAllowed, $
           colorChangeNColors: colorChangeNColors, $
           vectorObj: vectorObj, $
           xsize:        s[1], $
           ysize:        s[2], $
           sclmin:       0, $
           sclmax:       249, $
           mapcoord:     mapcoord }

   ; Return the image, scaled properly and reversed for proper display in IDL.
   success = 1
   RETURN, BytArr(s[1],s[2])

END
