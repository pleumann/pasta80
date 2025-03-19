esx_call:       di
                ld      (esx_saved_sp),sp
                ld      sp,hl
                pop     bc
                pop     de
                pop     hl
                pop     af
                ld      sp,(esx_saved_sp)
                ei
