; -------------------------------------------------------------------------
; --- Next registers assembly routines ------------------------------------
; -------------------------------------------------------------------------

; Gets a Next register.
;
; In:   hl=register number
; Out:  hl=register value
;
nxt_getreg:     ld      bc,$243B
                out     (c),l
                inc     b
                in      l,(c)
                ld      h,0
                ret

; Sets a Next register.
;
; In:   hl=register number, de=register value
; Out:  -
;
nxt_setreg:     ld      bc,$243B
                out     (c),l
                inc     b
                out     (c),e
                ret

; Changes the current RAM page visible at $E000. Assumes interrupts are
; already disabled.
;
; In:   A (bank)
; Out:  -
;
banksel:
        ld      c,a             ; Save desired bank
        and     7
        out     ($fe),a
        ld      a,c
        nextreg $57,a
        ld      (curpage),a

        ret

; Changes the current RAM bank visible at $C000. This variant is for the
; Pascal RTL routine. It Uses the register calling convention and handles
; interrupts.
;
; In:   HL (bank)
; Out:  -
;
banksel_hl:
        ld      a,l
        di
        call    banksel
        ei
        ret
