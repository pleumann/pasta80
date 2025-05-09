;
; Print character to screen
;
; Entry:  A (ASCII code)
; Exit:   -
; Uses:   C,E,IY
;
__putc:
                ld      e,a
                ld      c,2
                call    5
                ret

;
; New line
;
; Entry:  -
; Exit:   -
; Uses:   -
;
__newline:
                ld      a,13
                call    __putc
                ld      a,10
                call    __putc
                ret

__readkey:
                ld      c,11
                call    5
                and     a
                jr      z,__readkey
                ld      c,6
                ld      e,255
                call    5
                ret

;
; Read number from keyboard
;
; Entry:  -
; Exit:   HL
; Uses:   AF,BC,DE
;
; TODO Separate input from string-to-integer functionality
;

__getline_old:
                ld      hl,127
                ld      (__linemax),hl
                ld      de,__linemax
                ld      c,10
                call    5
                ld      a,13
                call    __putc
                ld      a,10
                call    __putc
                ld      a,(__linelen)
                ld      d,0
                ld      e,a
                ld      hl,__linebuf
                add     hl,de
                ld      (hl),0
                and     a
                sbc     hl,de
                ld      (__lineptr),hl
                ret

__getline:
                ld      hl,__linebuf
                ld      (__lineptr),hl
                xor     a
                ld      (__linelen),a
__readline1:
                call    __readkey
                cp      ' '
                jr      c, __readline2
                cp      127
                jr      nc,__readline2
                ld      hl,(__lineptr)
                ld      (hl),a
                inc     hl
                ld      (__lineptr),hl
                ld      hl,__linelen
                inc     (hl)
                call    __putc
                jp      __readline1
__readline2:
                cp      127
                jr      nz, __readline3
                ld      hl,(__lineptr)
                dec     hl
                ld      (__lineptr),hl
                ld      hl,__linelen
                dec     (hl)
                ld      a,8
                call    __putc
                ld      a,' '
                call    __putc
                ld      a,8
                call    __putc
                jp      __readline1
__readline3:
                cp      13
                jr      nz,__readline1
                ld      hl,(__lineptr)
                ld      (hl),0
                ld      hl,__linebuf
                ld      (__lineptr),hl
                call    __newline
                ret

__textfg:     ld a,l
              add a,'0'
              ld  (__textfg_str+3),a
              ld hl,__textfg_str
              call  __puts
              ret
__textfg_str: db 3,27,'T',32


__textbg:     ld a,l
              add a,'0'
              ld  (__textbg_str+3),a
              ld hl,__textbg_str
              call  __puts
              ret
__textbg_str: db 3,27,'S',32

__gotoxy:     ld a,l
              add a,31
              ld  (__gotoxy_str+4),a
              ld a,e
              add a,31
              ld  (__gotoxy_str+3),a
              ld hl,__gotoxy_str
              call  __puts
              ret
__gotoxy_str: db 4,27,'Y',32,32

__clrscr:     ld hl,__clrscr_str
              call  __puts
              ret
__clrscr_str: db 4,27,'H',27,'J'

__cursor_on:  ld hl,__cur_on_str
              call  __puts
              ret
__cur_on_str: db 2,27,'e'


__cursor_off: ld hl,__cur_off_str
              call  __puts
              ret
__cur_off_str: db 2,27,'f'

__checkbreak:
                push    ix
                ld      c,11
                call    5
                pop     ix
                and     a
                ret     z
                push    ix
                ld      c,1
                call    5
                pop     ix
                cp      3
                ret     nz
                rst     0

;
; Startup (with some help from Melissa O'Neill for NXT case)
;
; Entry: -
; Exit: -
; Uses: *
;
__init:         ld      sp, ($0006)
                call    main

;
; Shutdown
;
; Entry: -
; Exit: -
; Uses: *
;
__done:         rst     0