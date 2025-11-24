; -------------------------------------------------------------------------
; --- Agon/Console 8 MOS assembly routines --------------------------------------------
; --- v3.0 required for some file routines --------------------------------------------
; -------------------------------------------------------------------------

; Calls a MOS API function.
;
; In:           hl      MOS API function number
;               de      address of register record
;               need to then push the actual reg values in/out of the registers struct
; Out:          hl      MOS error number, 0 is OK
;

mos_call:
;    push    ix
    ld      a,l     ;mos API routine to call
    push    de      ;address of register struct
    pop     ix
    ; AF is not used in these calls so not included in the registers.
    ;Consider macros for these if we use them again.
    ;ld bc,(IX+0) ->  LD BC,(IX+d) 0/1 5/6 DD, 07, dd
    db      0ddh, 007h, 00
    ;ld de,(IX+2) ->  LD DE,(IX+d) 0/1 5/6 DD, 17, dd
    db      0ddh, 017h, 02
    ;ld hl,(IX+4) ->  LD HL,(IX+d) 0/1 5/6 DD, 27, dd
    db      0ddh, 027h, 04


    rst     08h ;MOS API call
;    push    af
;    or      64
;    rst     10h
;    pop     af
    ;ld (IX+0),bc -> LD (IX+d),BC 0/1 5/6 DD, 0F, dd
    db      0ddh, 00Fh, 00
    ;ld (IX+2),de -> LD (IX+d),DE 0/1 5/6 DD, 1F, dd
    db      0ddh, 01Fh, 02
    ;ld (IX+4),hl -> LD (IX+d),HL 0/1 5/6 DD, 2F, dd
    db      0ddh, 02Fh, 04
    ld      l,a ;Return A into HL.
    ld      h,0
;    pop     ix
    ret

mos_call_seek:
; 0x1C: mos_flseek
; Parameters:
;C: File handle
;HLU: Least significant 3 bytes of the offset from the start of the file
;E: Most significant byte of the offset (set to 0 for files < 16MB)
;Preserves: HL(U), BC(U), DE(U)
;
;Returns:
;A: Status code
    ld      a,01ch     ;mos API routine to call
    push    hl      ;address of register struct - only 1 param
    pop     ix
    ld      de,0
    ;ld bc,(IX+0) ->  LD BC,(IX+d) 0/1 5/6 DD, 07, dd
    db      0ddh, 007h, 00
    ;ld hl,(IX+4) ->  LD HL,(IX+d) 0/1 5/6 DD, 27, dd
    db      0ddh, 027h, 04
    ld      e,0

    ld      b,7
mos_call_seek_x7:
    mklil
    add     hl,hl
    djnz    mos_call_seek_x7

    ld      hl,0

    rst     08h ;MOS API call

    ld      l,a ;Return A into HL.
    ld      h,0
    ret


mos_file_length:
;0x19: mos_getfil
;Get a pointer to a FIL structure in MOS (Requires MOS 1.03 or above)
;This call is useful if you wish to use the FatFS API directly, but need to pass a FIL structure to a FatFS API call.
;Parameters:
;C: File handle
;Preserves: BC(U)
;Returns:
;HLU: 24-bit pointer to a FIL structure (in MOS RAM)
    ld      a,019h     ;mos API routine to call
    push    hl      ;address of register struct - only 1 param
    pop     ix
    ld      de,0
    ;ld bc,(IX+0) ->  LD BC,(IX+d) 0/1 5/6 DD, 07, dd
    db      0ddh, 007h, 00
    rst     08h ;MOS API call

;this will be in a different page than the code, so everything needs to be mklil until you get the result
    mklil
    ld     de,13   ; 11 is offset to lower 3bytes in FSIZE_t, part of the FFOBJD struct that HL points to
    defb    0       ;need the extra byte
    mklil
    add hl,de
    mklil
    ld     a,(hl)   ;top byte
    mklil
    dec     hl
    mklil
    dec     hl
    mklil
;    ld      hl,(hl) ;lower 2 bytes
    ldhl_hl_
    ld      b,7
mos_file_len_divlp:
    rrca
    rrc     h
    rrc     l
    djnz    mos_file_len_divlp
    ret


; Return Paramter Count
__getargc:
    ld  hl,(__parmcount)    ;done during load
    ret

; return a character indexed from the parameter number
; No bounds checking.
; Entry: HL has Parameter Number (1 index = the first parameter), DE has Byte Number (1 index)
__getargvchar:
    dec     de
    push    de
    ld      de,hl
    add     hl,hl   ;* 2
    add     hl,de   ;* 3

    ld      de,argv_ptrs
;    add     hl,de
    push    hl
    pop     ix
    add     ix,de
    ldamb   ;LD     A, MB           ; Segment base
    call    _set_aix24
;    ld.lil      hl,(ix)
    ldhlix  0
    pop     de
    mklil
    add     hl,de
    mklil
    ld      a,(hl)
;test:    jp  test

    ld      l,a
    ld      h,0
    ret
;
; Print character to screen
;
; Entry:  A (ASCII code)
; Exit:   -
; Uses:   C,E,IY
;
__putc:
                rst     10h
                ret

;
; New line
;
; Entry:  -
; Exit:   -
; Uses:   -
;
__newline:
                ld      a,13
                rst     10h
                ld      a,10
                rst     10h
                ret

; Blocking, and do not echo character.
; Out:  HL=result
__readkey:
                push    ix
            ld  a, 8        ;0x08: mos_sysvars
            rst 08h         ;IX(U) now has sysvars
            ld  h,0
__readkey_1:
            mklil
            ld  a,(ix+18h)  ;key down?
            or  a
            jr  z,__readkey_1
            mklil
            ld  a,(ix+5)    ;valid key?
            or  a
            jr  z,__readkey_1            
            ld  l,a
            mklil
            ld  (ix+18h),h  ;zero
;__readkey_1:
                pop     ix
;                rst     10h    ;not sure we need to re-display this?
                ret

__keypressed:
; sysvar_keyascii:        EQU 05h ; 1: ASCII keycode, or 0 if no key is pressed
; sysvar_vkeydown         EQU 18h ; 1: Virtual key state from FabGL (0=up, 1=down)
; should also check the key is 1) down and 2) meaningful

; not sure why we are preserving IX
            push    ix
            ld  a, 8        ;0x08: mos_sysvars
            rst 08h         ;IX(U) now has sysvars
            mklil
            ld  a,(ix+18h)  ;Is a key down?
            or  a
            jr  z,__keypressed_1    ;return no key down
            mklil
            ld  a,(ix+05h)  ;Is key down returning a character?
            or  a
            jr  z,__keypressed_1    ;if not, return no key down
            ld  a,1         ;Else return key down.
__keypressed_1
            ld  l,a
            ld  h,0
            pop     ix
            ret

; Reads a whole line of input from the keyboard into the central buffer, so
; that other routines can consume it.
;
; Entry:  -
; Exit:   HL points to buffer
; Uses:   AF,BC,DE
;
; TODO Separate input from string-to-integer functionality (??)
;
;0x09: mos_editline
;Parameters:
;HL(U): Address of the buffer
;BC(U): Buffer length
;E: Flags to control editor behaviour
;Preserves: HL(U), BC(U), DE(U)

__getline:
                ld      hl,__linebuf
                ld      bc,127
                ld      e,9     ;clear buffer, disable history
                ld      a,9     ;mos_editline
                rst     08h
                ld      (__lineptr),hl
                ex      de,hl   ;de now has the line buffer
                ld      hl,__linelen
                ld      (hl),255    ;"-1" so we can inc below
                ld      b,127   ;maximum line length in case for safety
__getline_lp:
                inc     (hl)    ;increment counter for characters
                ld      a,(de)
                cp      32      ;is it any sort of control character
                jr      c,__getline_2   ;yes, exit
                inc     de      ;next character
                djnz    __getline_lp
__getline_2:
                xor     a
                ld      (de),a      ;make it 0 terminated for consistency.
                call    __newline
                ret

; Agon Screen controls

__textfg:       ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,l        ; l has text colour 0-7.
                rst     10h
                ret


__textbg:       ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,l         ; l has background colour 0-7.
                or      128         ; set top bit for background
                rst     10h
                ret

;
; Currentlty assuming that we are in 16 or 64 colour mode.
; might enforce this at the beginning during the startup??
;
; Also assuming that these are used when we aren't using colour,
; so no attempt to track current colour is made as yet. This
; could be a future improvement.
;
__texthigh:     ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,128+15        ; solid white background
;                ld      a,128        ; solid black background
                rst     10h
                ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                xor     a        ;black text
;                ld      a,10        ;bright green text
                rst     10h
                ret

__textnorm:     ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,128        ; solid black background
                rst     10h
                ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,15        ;bright white text
                rst     10h
                ret

__textlow:      ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,128        ; solid black background
                rst     10h
                ld      a,17    ;VDU 17, colour: Set text colour (COLOUR)
                rst     10h
                ld      a,7        ;dim white text
                rst     10h
                ret


__gotoxy:     ld a,31   ;VDU 31, x, y: Move text cursor to x, y text position.
                        ;note top left is (1,1) so we need to adjust.
              rst 10h
              ld a,l    ;x position
              dec a
              rst 10h
              ld a,e    ;y position
              dec a
              rst 10h
              ret

__clrscr:     ld    a,12    ;VDU 12: Clear text area (CLS)
              rst   10h
              ret

; Clear to End of Line
; There is no code to do this VDP side so we do manually, assuming scroll protection is on.
; sysvar_cursorX:         EQU 07h ; 1: Cursor X position
; sysvar_scrCols:         EQU 13h ; 1: Screen columns in characters
; VDU 23, 0, &82: Request text cursor position
; Requires scroll protection set on
; TODO: add code to request cursor location update and wait for result.
__clreol:
; not sure why we are preserving IX
; update cursor position first.
            ld      a,23
            rst     10h
            xor     a
            rst     10h
            ld      a,82h
            rst     10h
; now try to calculate the required data
            push    ix
            push    bc
            ld      a, 8        ;0x08: mos_sysvars
            rst     08h         ;IX(U) now has sysvars
            mklil
            ld      a,(IX+13h)
            mklil
            sub     (IX+7h)
            ld      b,a
            ld      c,a
            ld      a,' '       ;spaces
__clreol_sp:
            rst     10h
            djnz    __clreol_sp
            ld      b,c
            ld      a,8         ;backspaces
__clreol_bs:
            rst     10h
            djnz    __clreol_bs
            pop     bc
            pop     ix
            ret

__cursor_on:  ld hl,__cur_on_str
              call  __puts
              ret
__cur_on_str: db 3,23,1,1   ;VDU 23, 1, n: Cursor control


__cursor_off: ld hl,__cur_off_str
              call  __puts
              ret
__cur_off_str: db 3,23,1,0   ;VDU 23, 1, n: Cursor control

__checkbreak:
; sysvar_keyascii:        EQU 05h ; 1: ASCII keycode, or 0 if no key is pressed
; not sure why we are preserving IX
            push    ix
            ld  a, 8        ;0x08: mos_sysvars
            rst 08h         ;IX(U) now has sysvars
            mklil
            ld  a,(ix+5)
            pop     ix
            cp      3
            ret     nz
            pop     hl  ;go up to the next level - TODO: Can this even work?
            jp      __done

; Delay routine
; This will need to be an estimate, subtracking 17ms for every 60Hz (or 75hz) tick...
; sysvar_time:            EQU 00h ; 4: Clock timer in centiseconds (incremented by 2 every VBLANK)
; Entry: HL = Milliseconds
; Exit: Uses A, HL
__delay:
            push    ix
            push    de
            push    bc
            ld  a, 8        ;0x08: mos_sysvars
            rst 08h         ;IX(U) now has sysvars
            ld  DE,17
            ld  a,h
            or  l
            jr  z,__delay_exit  ;trivial delay
__delay_outer:
            mklil
            ld  b,(ix+0)    ;lowest time tick counter, watching for change
__delay_inner:
            mklil
            ld  a,(ix+0)    ;lowest time tick counter, watching for change
            cp  b
            jr  z,__delay_inner ;no tick yet
            ld  b,a
            or  a   ;clear carry flag
            sbc hl,de
            jr  z,__delay_exit  ;right on zero edge case
            jr  nc,__delay_inner ;fetch the next tick

__delay_exit:
            pop bc
            pop hl
            pop ix
            ret
;
; Plots a point.
;
; In:   l=x, e=y
; Out:  -
;
; VDU 25, code, x; y;
; &40-&47   64-71   Point plot
; 4 (C) Move absolute
; 5 (D) Plot absolute in current foreground colour
al_plot:        ld      a,25
                rst     10h
                ld      a,045h
                rst     10h
                ld      a,l
                rst     10h
                ld      a,h
                rst     10h
                ld      a,e
                rst     10h
                ld      a,d
                rst     10h
                ret

;
; Draws a relative line starting at the most recent plot position.
;
; In:   hl= relative X, de= relative y
; Out:  -
;
; &00-&07   0-7 Solid line, includes both ends
; 1 (9) Plot relative in current foreground colour
al_draw:        ld      a,25
                rst     10h
                ld      a,001h
                rst     10h
                ld      a,l
                rst     10h
                ld      a,h
                rst     10h
                ld      a,e
                rst     10h
                ld      a,d
                rst     10h
                ret

;
; Draws a circle.
;
; In:   HL, DE = absolute X,Y, BC = radius (which is a relative x or y size with the other being 0)
; Out:  -
;
; VDU 25, code, x; y;
;
; To make this work, we are moving the cursor to the X,Y position of the centre
; then doing a relative circle plot of the radius.
;
; &40-&47   64-71   Point plot
; 4 (C) Move absolute
; &90-&97   144-151 Circle outline
; 1 (9) Plot relative in current foreground colour
al_circle:
                ld      a,25
                rst     10h
                ld      a,044h  ;move current cursor to X,Y
                rst     10h
                ld      a,l
                rst     10h
                ld      a,h
                rst     10h
                ld      a,e
                rst     10h
                ld      a,d
                rst     10h
                ld      a,25
                rst     10h
                ld      a,091h  ;Draw circle of radius BC
                rst     10h
                ld      a,c
                rst     10h
                ld      a,b
                rst     10h
                xor     a
                rst     10h
                rst     10h
                ret

; Returns the colour of a point
;
; In:   l=x, e=y
; Out:  HL = colour
; USES IX
;
; VDU 23, 0, &84, x; y;: Get colour of pixel at pixel position x, y
; This command will return the colour of the pixel at the given pixel position to MOS. The corresponding MOS sysvars will be updated to reflect the read pixel colour.
;

; sysvar_scrpixelIndex:   EQU 16h ; 1: Index of pixel data read from screen
;

al_point:
            ld  a, 8        ;0x08: mos_sysvars
            rst 08h         ;IX(U) now has sysvars
            ld      a,23
            rst     10h
            xor     a
            rst     10h
            ld      a,084h
            rst     10h
            ld      a,l
            rst     10h
            ld      a,h
            rst     10h
            ld      a,e
            rst     10h
            ld      a,d
            rst     10h
            mklil
            ld      l,(IX + 16h)    ;SYSVARS will not be in the same RAM page
            ld      h,0
            ret


; VDU 23, 0, &C0, n: Turn logical screen scaling on and off *
; Turns logical screen scaling on and off, where 1=on and 0=off.
; turn off logical scaling as soon as possible as we don't want this for pascal.
al_setcoords:
              ld hl,__scaling_off_str
              call  __puts
              ret
__scaling_off_str: db 8,23,0,0c0h,0,23,16,1,254   ;VDU 23, 1, n: Cursor control
                                                  ;VDU 23, 16, setting, mask: Define cursor movement behaviour

;
; Shutdown -> graceful successful exit. HL=0 for success.
;
; Entry: -
; Exit: -
; Uses: HL
;
__done:
            ld      hl,0 
            ret

; test cruft
    push    bc
    push    hl
    push    af
    push    de
    or      64
    rst     10h
    ld      bc,0
    xor     a
    rst     18h
;    ld      b,5
;mos_call_testlp:
;    ld      a,(hl)
;    inc     hl
;    rst     10h
;    djnz    mos_call_testlp
    pop     de
    pop     af
    pop     hl
    pop     bc