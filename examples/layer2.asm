; -------------------------------------------------------------------------
; --- Layer 2 graphics primitives -----------------------------------------
; -------------------------------------------------------------------------

; TODO: This should be integrated into the RTL properly.

__win_handle:   dw      0

; Initialize and activate layer 2
l2_enable:        
                ld      de, $01d5
                ld      a,1
                exx
                ld      bc,$0201
                exx
                ld      c,7
                rst     $08
                db      $94
                exx
                ld   (__win_handle),hl
                exx
                ret

;
; Set a pixel in LAYER 2,1
;
; Entry:    HL  x       (0..255)
;           DE  y       (0..191)
;           BC  color   (0..255)
;
l2_set_pixel:    di
                ld      a,e
                srl     a
                srl     a
                srl     a
                srl     a
                srl     a
                add     a,18

                db      $ed,$92,$57

                ld      a,e
                and     31
                ld      h,a

                ld      de,$e000
                add     hl,de
                ld      (hl),c

                db      $ed,$91,$57,$01

                ei
                ret

;
; Get a pixel in LAYER 2,1
;
; Entry:    HL  x       (0..255)
;           DE  y       (0..191)
;
l2_get_pixel:    di
                ld      a,e
                srl     a
                srl     a
                srl     a
                srl     a
                srl     a
                add     a,18

                db      $ed,$92,$57

                ld      a,e
                and     31
                ld      h,a

                ld      de,$e000
                add     hl,de
                ld      a,(hl)
                ld      h,0
                ld      l,a

                db      $ed,$91,$57,$01

                ei
                ret