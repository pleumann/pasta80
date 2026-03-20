; -------------------------------------------------------------------------
; --- Overlay support for Agon --------------------------------------------
; -------------------------------------------------------------------------

; 24 bits ld helper macro for register pairs
;
    MACRO ld24 reg,val
        db      0x5b
        ld      reg,val & 0xffff
        db      (val >> 16) & 0xff
    ENDM

; Loads our overlay. The file name will be put into the assembly file by the
; compiler. It is usually the same as the binary name, with extension changed
; to ".ovr". The overlay is loaded to fixed address 0x50000, so just after the
; standard 64K address block for "classic" Z80 programs.
;
overload:
            mksil
            call    real_overload
            db      0x04
            ret

real_overload:
            ldamb
            push    af
            xor     a
            ldmba
            ld24    hl,0x40000 + ovl_name   ; Filename (in 0x040000 segment)
            ld24    de,0x50000              ; Load to fixed address 0x050000
            ld24    bc,0                    ; Accept any file size
            ld      a,0x01                  ; Call #1 "load file"
            mklil
            rst     0x08                    ; Invoke MOS
            ld      l,a                     ; fetch return code
            ld      h,0
            pop     af
            ldmba
    	    mklil
            ret

; Used by "farcall" to ensure the right page is visible at 0x[4]e000. We don't
; have memory paging, so we need to copy our overlay to the target address,
; which is still better than loading it from disk each time. The first two
; bytes of the overlay contain its actual size. All overlays start at 8K
; boundaries.
;
banksel:
            and     a
            ret     z

            push    hl
            push    de
            push    bc

            ld24    hl,0x40000
            ld24    de,0x02000
            ld      b,a
banksel1:
            mklil
            add     hl,de
            djnz    banksel1                ; Make UHL = A * 8192

            mklil
            db 0xed, 0x07

            ld24    de,0x4e000
            mklis
            ldir

            pop     bc
            pop     de
            pop     hl

            ret
