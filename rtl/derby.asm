; =========================================================================
; === ZX Spectrum 128K run-time library ===================================
; =========================================================================

; Changes the current RAM bank visible at $C000. Assumes interrupts are
; already disabled.
;
; In:   A (bank)
; Out:  -
;
banksel:
        ld      c,a             ; Save desired bank

        ld      a,7
        sub     c
        out     ($fe),a

        ld      a,(0x5b5c)      ; Retrieve current config
        and     0xf8            ; Zero the RAM bank bits
        or      c               ; Add our desired bank
        ld      bc,0x7ffd
        ld      (0x5b5c),a      ; Save to system variable
        out     (c),a           ; Write to the port

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