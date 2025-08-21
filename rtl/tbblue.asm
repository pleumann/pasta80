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

; Performs a "far" call. Changes and restores the RAM bank at $C000 as
; needed.
;
; In:   A (bank), HL (callee address)
; Out:  -
;
farcall:
        //add     32
        ld      c,a
        ld      a,(curpage)      ; See if we need to switch bank
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
curpage:        db      0
mystack:        ds      96