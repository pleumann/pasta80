; =========================================================================
; === ZX Spectrum 48K run-time library ====================================
; =========================================================================

                ORG     $8000

; -------------------------------------------------------------------------
; --- Low-level routines expected by the compiler -------------------------
; -------------------------------------------------------------------------

; Starts and terminates the program.
;
; In:   -
; Out:  -
;
__init:         ld      (__saved_iy),iy ; Save sysvar pointer
                ld      (__saved_sp),sp ; Save original stack

                ld      sp,0            ; We use our own stack

                ld      a,2
                call    0x1601          ; Open channel #2

                call    main            ; Call user program

__done:         ld      sp,(__saved_sp) ; Restore original stack
                ld      iy,(__saved_iy) ; Restore sysvar pointer

                ret

__saved_sp:     dw 0
__saved_iy:     dw 0

; Prints a line break (fall-through intended).
;
; In:   -
; Out:  -
;
__newline:      ld      a,13

; Prints a single character to the screen.
;
; In:   l=ASCII code
; Out:  -
;
__putc:         rst     16
                ret

; Reads a whole line of input from the keyboard into the central buffer, so
; that other routines can consume it.
;
; In:   -
; Out:  -
;
__getline:      ld      hl,__linebuf    ; Setup everything
                ld      (__lineptr),hl
                xor     a
                ld      (__linelen),a
__readline1:    ld      a,143           ; Display cursor block
                rst     16
                ld      a,8
                rst     16

__readline2:    call    zx_readkey      ; Wait for a key to be pressed

                push    hl
                ld      d,0
                ld      e,(iy-1)
                ld      hl,$00c8
                call    $03b5           ; Make clicking noise
                pop     hl

                ld      a,l
                cp      ' '
                jp      c,__readline3   ; Handle control characters
                cp      127
                jp      nc,__readline2  ; Non-ASCII, read another key
                ld      hl,(__lineptr)
                ld      (hl),a          ; Store character
                inc     hl
                ld      (__lineptr),hl  ; Adjust pointer
                ld      hl,__linelen
                inc     (hl)            ; Increment length
                rst     16              ; Print character
                jp      __readline1     ; And repeat
__readline3:    cp      12
                jp      nz, __readline4
                ld      hl,(__lineptr)  ; Handle delete key
                dec     hl
                ld      (__lineptr),hl
                ld      hl,__linelen
                dec     (hl)
                ld      a,' '
                rst     16
                ld      a,8
                rst     16
                ld      a,8
                rst     16
                jp      __readline1
__readline4:    cp      13
                jp      nz,__readline2
                ld      hl,(__lineptr)  ; Handle enter key
                ld      (hl),0
                ld      hl,__linebuf
                ld      (__lineptr),hl
                ld      a,' '
                rst     16
                call    __newline
                ret

; Checks if Shift and Space keys (aka Break) are currently pressed.
; Terminates the program if this is the case. Works directly on the
; keyboard matrix and does not consume a pending key press.
;
; In:   -
; Out:  -
;
__checkbreak:   ld      a,$fe
                in      a,($fe)
                rra
                ret     c

                ld      a,$7f
                in      a,($fe)
                rra
                ret     c

                jp      __done

; -------------------------------------------------------------------------
; --- Routines that implement a Pascal procedure or function --------------
; -------------------------------------------------------------------------

; Checks if a key has been pressed.
;
; In:   -
; Out:  e=1 if key has been pressed, e=0 otherwise
;
zx_testkey:     ld      de,0
                bit     5, 1 (iy)
                ret     z
                inc     e
                ret

; Waits for a key press and returns the ASCII code resulting from it.
;
; In:   -
; Out:  e=ASCII code
;
zx_readkey:     halt
                bit     5, 1 (iy)
                jr      z,zx_readkey
                res     5, 1 (iy)
                ld      a, (#23560)
                ld      l,a
                ret

;
; Sets the foreground color (aka ink).
;
; In:   l=color (0..7)
; Out:  -
;
zx_color:       push    hl
                ld      a,16
                rst     16
                pop     hl
                ld      a,l
                rst     16
                ret

; Sets the background color (aka paper).
;
; In:   l=color (0..7)
; Out:  -
;
zx_background:  push    hl
                ld      a,17
                rst     16
                pop     hl
                ld      a,l
                rst     16
                ret

; Moves the cursor to a given location.
;
; In:   l=column (1..32), e=row (1..24)
; Out:  -
;
zx_gotoxy:      push    hl
                push    de

                ld      a,22
                rst     16

                pop     hl
                ld      a,l
                dec     a
                rst     16

                pop     hl
                ld      a,l
                dec     a
                rst     16

                ret

; Clears the screen and applies the attributes currently stored in the
; system variable at 23695. Sets the cursor position to the top left
; corner.
;
; In:   -
; Out:  -
;
zx_clrscr:      ld      hl, 16384
                ld      de, 6144
                ld      c, 0
                call    __fillchar      ; Clear pixels

                ld      hl, 22528
                ld      de, 768
                ld      a, (23695)
                ld      c, a
                call    __fillchar      ; Clear attributes

                ld      l, 1
                ld      e, 1
                call    zx_gotoxy       ; Set cursor

                ret

; Changes the border color using the appropriate ROM routine.
;
; In:   l=border color (0..7)
; Out:  -
;
zx_border:      ld      a,l
                and     $07
                call    $229b
                ret

; Delays the program by (roughly) a given number of milliseconds.
;
; In:   hl=milliseconds
; Out:  -
;
zx_delay:       ld      de,20
                call    __sdiv16
zx_delay_loop:  ld      a,h
                or      l
                ret     z
                dec     hl
                halt
                jr      zx_delay_loop