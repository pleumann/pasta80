; Calls an esxDOS function.
;
; In:   hl=esxDOS function number
;       de=address of register record
; Out:  hl=esxDOS error number, 0 means OK.
;
esx_call:       ld      a,(hl)
                ld      (esx_call_num),a
                ex      de,hl

                push    hl

                di
                ld      (esx_saved_sp),sp
                ld      sp,hl
                pop     af
                pop     bc
                pop     de
                pop     ix                  ; HL when in dot command
                ld      sp,(esx_saved_sp)
                ei

                rst     $08
esx_call_num:   db      $00
                pop     hl
                jr      c,esx_call_err

                di
                ld      (esx_saved_sp),sp
                ld      sp,hl
                push    af
                push    bc
                push    de
                ; push    ix                ; IX/HL not used in responses
                ld      sp,(esx_saved_sp)
                ei

                ld      hl,0
                ret

esx_call_err:   ld      h,0
                ld      l,a
                ret

esx_saved_sp:   ds      2