                org     $8000

                jp      main

__saved_iy:     dw      0
__win_handle:   dw      0
__buffer:       equ     23698

;
; Print character to screen
;
; Entry:  A (ASCII code)
; Exit:   -
; Uses:   C,E,IY
;

__putc:
                rst     16
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
                ret

__readkey:
        halt
        bit    5, 1 (iy)
        jr     z, __readkey
        res    5, 1 (iy)
        ld     a, (23560)
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
                cp      12
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
                
__getn_old:
                ld      iy, (__saved_iy)
                ;    ld      a,1
                ;   rst $18
                ;   defw    $1601
                ld a,'>'
                call __putc
                ld      de,(23633)      ; save current channel
                push    de       
                ld      de,(__win_handle)        ; set current channel to magic window
                ld      (23633), de 
                ld      de,$01c3
                ld      c,7
                exx
                ld      hl,__linebuf
                ld      e,0
                ld      a,127
                exx
                rst     8
                db      $94
                pop     bc
                ld      (23633), bc 
                push    de
                pop de
                ld      a,e
                ld      hl,__linebuf
                call    __atoi
                ret

__textfg:     ld a,l
              ld  (__textfg_str+2),a
              ld hl,__textfg_str
              call  __puts
              ret
__textfg_str: db 2,16,0 ; No good in LAYER 2,1 - needs mapping

__textbg:     ld a,l
              ld  (__textbg_str+2),a
              ld hl,__textbg_str
              call  __puts
              ret
__textbg_str: db 2,17,0 ; No good in LAYER 2,1 - needs mapping

__gotoxy:     ld a,l
              dec   a
              ld  (__gotoxy_str+3),a
              ld a,e
              dec   a
              ld  (__gotoxy_str+2),a
              ld hl,__gotoxy_str
              call  __puts
              ret
__gotoxy_str: db 3,22,0,0

__clrscr:     ld hl,__clrscr_str
              call  __puts
              ret
__clrscr_str: db 1,14

;
; Startup (with some help from Melissa O'Neill for NXT case)
;
; Entry: -
; Exit: -
; Uses: *
;
__init:         ld      (__saved_iy),iy
                ld      de, $01d5
                ld      a,1
                exx
#if defined(LORES)
                ld      bc,$0100
#endif
#if defined(HIRES)
                ld      bc,$0201
#endif
                exx
                ld      c,7
                rst     $08
                db      $94
                exx
                ld   (__win_handle),hl
                exx
                ret

;
; Shutdown
;
; Entry: -
; Exit: -
; Uses: *
;

__done:         ld      iy,(__saved_iy)
                ret