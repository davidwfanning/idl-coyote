; $Id: orb__define.pro,v 1.6 2001/01/15 22:26:48 scottm Exp $
;
; Copyright (c) 1997-2001, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;+
; NAME:
;   ORB
;
; PURPOSE:
;   This object serves as a graphical representation of an orb,
;   (or sphere), which subclasses from the IDLgrModel class.
;
; CATEGORY:
;   Object graphics.
;
; CALLING SEQUENCE:
;   To initially create:
;           oOrb = OBJ_NEW('orb')
;
;   To retrieve a property value:
;       oOrb->GetProperty
;
;   To set a property value:
;       oOrb->SetProperty
;
;   To print to the standard output stream the current properties of
;   the orb:
;       oOrb->Print
;
;   To destroy:
;       OBJ_DESTROY, oOrb
;
; KEYWORD PARAMETERS:
;   ORB::INIT:
;   <Note that keywords accepted by IDLgrModel::Init and/or
;    IDLgrPolygon::Init are also accepted here.>
;   POS:    A three-element vector, [x,y,z], specifying the position
;               of the center of the orb, measured in data units .
;       Defaults to [0,0,0].
;   RADIUS: A floating point number representing the radius of the
;               orb (measured in data units).  The default is 1.0.
;   DENSITY: A floating point number representing the density at which
;               the vertices should be generated along the surface of the
;               orb.  The default is 1.0.
;   TEX_COORDS: Set this keyword to a nonzero value if texture map
;               coordinates are to be generated for the orb.
;
;   ORB::GETPROPERTY:
;   POS:    Set this keyword to a named variable that upon return will
;       contain a three-element vector, [x,y,z], specifying the
;       position of the center of the orb, measured in data units .
;   RADIUS: Set this keyword to a named variable that upon return will
;       contain a floating point number representing the radius of the
;               orb (measured in data units).
;   DENSITY: Set this keyword to a named variable that upon return will
;       contain a floating point number representing the density at
;       which the vertices are generated along the surface of the
;               orb.
;
;   ORB::SETPROPERTY:
;   <Note that keywords accepted by IDLgrModel::SetProperty and/or
;    IDLgrPolygon::SetProperty are also accepted here.>
;   POS:    A three-element vector, [x,y,z], specifying the position
;               of the center of the orb. Defaults to [0,0,0].
;   RADIUS: A floating point number representing the radius of the
;               orb (measured in data units).  The default is 1.0.
;   DENSITY: A floating point number representing the density at which
;               the vertices should be generated along the surface of the
;               orb.  The default is 1.0.
;
; EXAMPLE:
;   Create an orb centered at the origin with a radius of 0.5:
;       oOrb = OBJ_NEW('Orb', POS=[0,0,0], RADIUS=0.5)
;
; MODIFICATION HISTORY:
;   Written by: RF, September 1996.
;   Modified SetProperty Method: RHT, August, 2001
;-

;----------------------------------------------------------------------------
; ORB::INIT
;
; Purpose:
;  Initializes an orb object.
;
;  This function returns a 1 if initialization is successful, or 0 otherwise.
;
FUNCTION Orb::Init, POS=pos, RADIUS=radius, DENSITY=density, $
                       TEX_COORDS=tex_coords, _EXTRA=e

    IF (self->IDLgrModel::Init(_EXTRA=e) NE 1) THEN RETURN, 0

    self.pos = [0.0,0.0,0.0]
    self.radius = 1.0
    self.density = 1.0

    IF (N_ELEMENTS(pos) EQ 3) THEN $
        self.pos = pos

    IF (N_ELEMENTS(radius) EQ 1) THEN $
        self.radius = radius

    IF (N_ELEMENTS(density) EQ 1) THEN $
        self.density = density

    IF (N_ELEMENTS(tex_coords) EQ 1) THEN $
        self.texture = tex_coords

    ; Initialize the polygon object that will be used to represent
    ; the orb.
    self.oPoly = OBJ_NEW('IDLgrPolygon', SHADING=1, /REJECT, _EXTRA=e)

    self->Add,self.oPoly

    ; Build the polygon vertices and connectivity based on property settings.
    self->BuildPoly

    RETURN, 1
END

;----------------------------------------------------------------------------
; ORB::CLEANUP
;
; Purpose:
;  Cleans up all memory associated with the orb.
;
PRO orb::Cleanup

    ; Cleanup the polygon object used to represent the orb.
    OBJ_DESTROY, self.oPoly

    ; Cleanup the superclass.
    self->IDLgrModel::Cleanup
END

;----------------------------------------------------------------------------
; ORB::SETPROPERTY
;
; Purpose:
;  Sets the value of properties associated with the orb object.
;
PRO orb::SetProperty, POS=pos, RADIUS=radius, DENSITY=density, _EXTRA=e

    rebuild = 0B

    ; Pass along extraneous keywords to the superclass and/or to the
    ; polygon used to represent the orb.
    self->IDLgrModel::SetProperty, _EXTRA=e
    self.oPoly->SetProperty, _EXTRA=e

    IF (N_ELEMENTS(pos) EQ 3) THEN BEGIN
        self.pos = pos
        rebuild = 1B
    ENDIF

    IF (N_ELEMENTS(radius) EQ 1) THEN BEGIN
        self.radius = radius
        rebuild = 1B
    ENDIF

    IF (N_ELEMENTS(density) EQ 1) THEN BEGIN
        self.density = density
        rebuild = 1B
    ENDIF

    ; Rebuild the polygon according to keyword settings.
    IF (rebuild) THEN self->BuildPoly
END

;----------------------------------------------------------------------------
; ORB::GETPROPERTY
;
; Purpose:
;  Retrieves the value of properties associated with the orb object.
;
PRO orb::GetProperty, POS=pos, RADIUS=radius, DENSITY=density,$
                         POBJ=pobj, _REF_EXTRA=re

    self->IDLgrModel::GetProperty, _EXTRA=re
    self.oPoly->GetProperty, _EXTRA=re

    pos = self.pos
    radius = self.radius
    density = self.density
    pobj = self.oPoly
END

PRO orb::Print
    PRINT, self.pos
    PRINT, self.radius
    PRINT, self.density
END

;----------------------------------------------------------------------------
; ORB::BUILDPOLY
;
; Purpose:
;  Sets the vertex and connectivity arrays for the polygon used to
;  represent the orb.
;
PRO orb::BuildPoly
    ; Build the orb.

    ; Number of rows and columns of vertices is based upon the density
    ; property.
    nrows = LONG(20.0*self.density)
    ncols = LONG(20.0*self.density)
    IF (nrows LT 2) THEN nrows = 2
    IF (ncols LT 2) THEN ncols = 2

    ; Create the vertex list and the connectivity array.
    nverts = nrows*ncols + 2
    nconn = (ncols*(nrows-1)*5) + (2*ncols*4)
    conn = LONARR(ncols*(nrows-1)*5 + 2*ncols*4)
    verts = FLTARR(3, nverts)
    IF (self.texture NE 0) THEN $
        tex = FLTARR(2,nverts)

    ; Fill in the vertices.
    i = 0L
    j = 0L
    k = 0L
    tzinc = (!PI)/FLOAT(nrows+1)
    tz = (!PI/2.0) - tzinc
    FOR k=0,(nrows-1) DO BEGIN
        z = SIN(tz)*self.radius
        r = COS(tz)*self.radius
        t = 0
        IF (self.texture NE 0) THEN BEGIN
            tinc = (2.*!PI)/FLOAT(ncols-1)
        ENDIF ELSE BEGIN
            tinc = (2.*!PI)/FLOAT(ncols)
        ENDELSE
        FOR j=0,(ncols-1) DO BEGIN
            verts[0,i] = r*COS(t) + self.pos[0]
            verts[1,i] = r*SIN(t) + self.pos[1]
            verts[2,i] = z + self.pos[2]

            IF (self.texture NE 0) THEN BEGIN
                tex[0,i] = t/(2.*!PI)
                tex[1,i] = (tz+(!PI/2.0))/(!PI)
            ENDIF

            t = t + tinc
            i = i + 1L
        ENDFOR
        tz = tz - tzinc
    ENDFOR
    top = i
    verts[0,i] = self.pos[0]
    verts[1,i] = self.pos[1]
    verts[2,i] = self.radius*1.0 + self.pos[2]
    i = i + 1L
    bot = i
    verts[0,i] = self.pos[0]
    verts[1,i] = self.pos[1]
    verts[2,i] = self.radius*(-1.0) + self.pos[2]

    IF (self.texture NE 0) THEN BEGIN
        tex[0,i] = 0.5
        tex[1,i] = 0.0
        tex[0,i-1] = 0.5
        tex[1,i-1] = 1.0
    ENDIF

    ; Fill in the connectivity array.
    i = 0
    FOR k=0,(nrows-2) DO BEGIN
        FOR j=0,(ncols-1) DO BEGIN
            conn[i] = 4

            conn[i+4] = k*ncols + j

            w = k*ncols + j + 1L
            IF (j EQ (ncols-1)) THEN w = k*ncols
            conn[i+3] = w

            w = k*ncols + j + 1L + ncols
            IF (j EQ (ncols-1)) THEN w = k*ncols + ncols
            conn[i+2] = w

            conn[i+1] = k*ncols + j + ncols

            i = i + 5L
            IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
                i = i - 5L
        ENDFOR
    ENDFOR
    FOR j=0,(ncols-1) DO BEGIN
        conn[i] = 3
        conn[i+3] = top
        conn[i+2] = j+1L
        IF (j EQ (ncols-1)) THEN conn[i+2] = 0
        conn[i+1] = j
        i = i + 4L
        IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
            i = i - 4L
    ENDFOR
    FOR j=0,(ncols-1) DO BEGIN
        conn[i] = 3
        conn[i+3] = bot
        conn[i+2] = j+(nrows-1L)*ncols
        conn[i+1] = j+(nrows-1L)*ncols+1L
        IF (j EQ (ncols-1)) THEN conn[i+1] = (nrows-1L)*ncols
        i = i + 4L
        IF ((self.texture NE 0) AND (j EQ (ncols-1))) THEN $
            i = i - 4L
    ENDFOR

    self.oPoly->SetProperty, DATA=verts, POLYGONS=conn

    IF (self.texture NE 0) THEN $
        self.oPoly->SetProperty, TEXTURE_COORD=tex
END

;----------------------------------------------------------------------------
; ORB__DEFINE
;
; Purpose:
;  Defines the object structure for an orb object.
;
PRO orb__define
    struct = { orb, $
               INHERITS IDLgrModel, $
               pos: [0.0,0.0,0.0], $
               radius: 1.0, $
               density: 1.0, $
               texture: 0, $
               oPoly: OBJ_NEW() $
             }
END







