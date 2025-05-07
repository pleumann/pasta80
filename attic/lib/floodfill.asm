; Old flood-fill implementation, droppen in favor of the one by John Metcalf
zx_fill:        ld      d,e
                ld      e,l

                ld      h,$FF
                push    hl
                push    de

zx_fillline:
                ;ld      l,1
                ;ld      e,1
                ;call    zx_gotoxy

                ;ld      hl,0
                ;add     hl,sp
                ;call    __putn

                ;ld      a,' '
                ;rst     $10

                pop     de

                ld      a,$FF
                cp      d
                ret     z

                call    zx_pixelad
                ld      b,a
                ld      a,(hl)
                and     b
                jp      nz,zx_fillline

zx_left_loop:   rlc     b
                jp      nc,zx_left_loop1
                dec     l
zx_left_loop1:  dec     e
                ld      a,255
                and     e
                jp      z,zx_left_done              ; Column 0 reached?

                ld      a,(hl)
                and     b
                jp      z,zx_left_loop

zx_left_done:   ld      c,e
                inc     c
zx_right_loop:  rrc     b
                jp      nc,zx_right_loop1
                inc     l

zx_right_loop1: inc     e
                jp      z,zx_right_done

                ld      a,(hl)
                or      b
                cp      (hl)
                jp      z,zx_right_done

                ld      (hl),a
                jp      zx_right_loop
zx_right_done:  ld      b,e
                ld      (zx_fill_save),bc

                inc     d
                call    zx_enqueue
                dec     d

                dec     d
                call    zx_enqueue

                jp      zx_fillline

zx_enqueue:     ld      a,175
                cp      d
                ret     c

                ld      bc,(zx_fill_save)
                ld      e,c

                ld      a,b
                sub     c
                ld      b,a
                call    zx_pixelad

                pop     ix

; use c trick in here.
                ld      c,0
zx_enqueue_lp:  sla     c
                and     (hl)
                jp      nz,zx_enqueue_1
                bit     1,c
                jp      nz,zx_enqueue_1
                inc     c
                push    de
zx_enqueue_1:   inc     e
                rrca
                jp      nc,zx_enqueue_2
                inc     l
zx_enqueue_2:   djnz    zx_enqueue_lp
                jp      (ix)

zx_fill_save:   ds      2
