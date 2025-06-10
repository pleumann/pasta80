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

; Performs a "far" call. Changes and restores the RAM bank at $C000 as
; needed.
;
; In:   A (bank), HL (callee address)
; Out:  -
;
farcall:
        ld      c,a
        ld      a,(0x5b5c)      ; See if we need to switch bank
        and     7
        cp      c
        jr      nz,farcall1
        jp      (hl)            ; Fast lane, use callee's ret
farcall1:
        di

        pop     de              ; Fetch our return address
        ld      (globalsp),sp   ; Save the global SP
        ld      sp,(localsp)    ; Activate our local stack
        push    de              ; Push return address to local stack
        push    af              ; Push old RAM bank to local stack
        ;inc     sp             ; Save a byte?
        ld      (localsp),sp    ; Save local SP
        ld      sp,(globalsp)   ; Activate global stack
        ld      a,c
        call    banksel         ; Change bank

        ei

        ld      de,farcall2     ; Perform the call, looks a bit ugly
        push    de
        jp      (hl)
farcall2:
        di

        ld      (globalsp),sp   ; Save the global SP
        ld      sp,(localsp)    ; Activate our local stack
        ;dec     sp             ; Save a byte?
        pop     af              ; Fetch the old bank
        pop     hl              ; Fetch our return address
        ld      (localsp),sp    ; Save local SP
        ld      sp,(globalsp)   ; Activate global stack
        call    banksel         ; Change bank

        ei

        jp      (hl)            ; Return to caller

globalsp:       dw      0
localsp:        dw      mystack+96
mystack:        ds      96