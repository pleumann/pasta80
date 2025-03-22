; -------------------------------------------------------------------------
; --- esxDOS assembly routines --------------------------------------------
; -------------------------------------------------------------------------

; Calls an esxDOS function.
;
; In:           hl      esxDOS function number
;               de      address of register record
; Out:          hl      esxDOS error number, 0 is OK
;
esx_call:       ld      a,l
                ld      (esx_call_num),a
                ex      de,hl

                di
                ld      (esx_saved_sp),sp
                ld      sp,hl
                pop     af
                pop     bc
                pop     de
                pop     ix                  ; TODO HL when in dot command
                ld      (esx_call_args),sp
                ld      sp,(esx_saved_sp)
                ei

                rst     $08
esx_call_num:   db      $00

                jr      c,esx_call_err

                di
                ld      (esx_saved_sp),sp   ; This might be redundant
                ld      sp,(esx_call_args)
                push    ix                  ; Not actually used in results
                push    de
                push    bc
                push    af
                ld      sp,(esx_saved_sp)
                ei

                ld      hl,0
                ret

esx_call_err:   ld      h,0
                ld      l,a
                ret

esx_saved_sp:   ds      2
esx_call_args:  ds      2