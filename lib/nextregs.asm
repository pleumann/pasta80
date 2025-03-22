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
