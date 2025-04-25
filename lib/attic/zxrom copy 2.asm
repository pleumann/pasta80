; =========================================================================
; === ZX Spectrum 48K run-time library ====================================
; =========================================================================

; -------------------------------------------------------------------------
; --- Low-level routines expected by the compiler -------------------------
; -------------------------------------------------------------------------

; Starts and terminates the program.
;
; In:   -
; Out:  -
;
__init:         di
                exx
                ld      (__saved_hl),hl ; Save alternate HL
                ld      (__saved_iy),iy ; Save sysvar pointer
                ld      (__saved_sp),sp ; Save original stack
                ld      sp,0            ; We use our own stack
                ei

                ld      a,2
                call    0x1601          ; Open channel #2

                call    main            ; Call user program

__done:         di
                ld      sp,(__saved_sp) ; Restore original stack
                ld      iy,(__saved_iy) ; Restore sysvar pointer
                ld      hl,(__saved_hl) ; Restore alternate HL
                exx
                ei

                ret

__saved_sp:     dw      0
__saved_iy:     dw      0
__saved_hl:     dw      0

; Prints a line break (fall-through intended).
;
; In:   -
; Out:  -
;
__newline:      ld      a,13

; Prints a single character to the screen.
;
; In:   a=ASCII code
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
zx_testkey:     ld      hl,0
                bit     5, (iy+1)
                ret     z
                inc     l
                ret

; Waits for a key press and returns the ASCII code resulting from it.
;
; In:   -
; Out:  e=ASCII code
;
zx_readkey:     halt
                bit     5, (iy+1)
                jr      z,zx_readkey
                res     5, (iy+1)
                ld      a, (23560)
                ld      l,a
                ret

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

;
; Changes the border color using the appropriate ROM routine.
;
; In:   l=border color (0..7)
; Out:  -
;
zx_border:      ld      a,l
                and     $07
                call    $229b
                ret

;
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

;
; Plots a point.
;
; In:   hl=coordinate
; Out:  -
;
zx_plot:        ld      bc,hl
                call    $22e5
                ret

zx_point:       ld      bc,hl
                call    $22aa
                ld      b,a
                inc     b
                ld      a,(hl)
zx_point_loop:  rlca
                djnz    zx_point_loop
                and     $01
                ld      h,0
                ld      l,a
                ret

;
; Plots a point.
;
; In:   hl=1st coordinate, de=2nd coordinate
; Out:  -
;
zx_draw:
                ld      bc,hl
                call    $24BA
                ret

zx_stacklow:    dw      2

zx_fill:        ld      d,e
                ld      e,l
                ld      hl,$FFFF
                push    hl
                ld      (zx_stacklow),hl
                push    de

zx_fillline:    ld      hl,0
                add     hl,sp
                ld      de,(zx_stacklow)
                xor a
                sbc hl,de
                add hl,de
                jr      nc,zx_fillline_go
                ld      (zx_stacklow),hl

zx_fillline_go: pop     de
                ld      a,e
                inc     a
                ld      hl,(zx_stacklow)
                ret     z

                ld      (zx_floodcoord),de

                call    zx_fillpixel
                jr      nc,zx_fillline

                ld      de,(zx_floodcoord)

zx_left_loop:   ld      a,e
                and     a
                jr      z,zx_right

                dec     e
                call    zx_fillpixel
                jr      c,zx_left_loop

zx_right:       ld      de,(zx_floodcoord)
zx_right_loop:  ld      a,e
                inc     a
                jr      z,zx_fillline

                inc     e
                call    zx_fillpixel
                jr      c,zx_right_loop
                jr      zx_fillline

zx_floodcoord:  ds      2

; Fills pixel at B (row) C (column). On return CF is set if pixel was filled.
zx_fillpixel:
            if 0
                db      $ed, $94        ; Get pixel address and bit number
                db      $ed, $95
            else
                ld      bc,de
                call    $22aa           ; Get pixel address and bit number

                ld      b,a
                inc     b
                xor     a
                scf
zx_fillpixel_1: rra                     ; Shift working bit into place
                djnz    zx_fillpixel_1
            endif


                ld      c,a
                and     (hl)            ; Check pixel on screen
                ret     nz              ; Do nothing if already set

                ld      a,c
                or      (hl)            ; Or working bit and screen
                ld      (hl),a          ; Update screen

                pop     hl              ; Save return address

                xor     a
                cp      d
                jr      z,zx_fillpixel_2  ; Line 0, don't go further down

                dec     d
                push    de              ; Put next pixel down on stack
                inc     d

zx_fillpixel_2: ld      a,175
                cp      d
                jr      z,zx_fillpixel_3 ; Line 176, don't go further up

                inc     d
                push    de              ; Put next pixel up on stack
                dec     d

zx_fillpixel_3: scf
                jp      (hl)            ; Return via jump


; scanline fill by John Metcalf
; call with d=x-coord, e=y-coord

; set end marker

zx_fill4:
  ld d,e
  ld e,l
fill:
  ld hl,$FFFF
  ld    (zx_stacklow),hl
  push hl

; calculate bit position of pixel

nextrun:        push    de
                ld      hl,0
                add     hl,sp
                ld      de,(zx_stacklow)
                xor a
                sbc hl,de
                add hl,de
                jr      nc,nextrun_1
                ld      (zx_stacklow),hl
nextrun_1:
                pop     de
  ld a,d
  and 7
  inc a
  ld b,a
  ld a,1
bitpos:
  rrca
  djnz bitpos
  ld c,b
  ld b,a

; move left until hitting a set pixel or the screen edge

seekleft:
  ld a,d
  or a
  jr z,goright
  dec d
  rlc b
  call scrpos
  jr nz,seekleft

; move right until hitting a set pixel or the screen edge,
; setting pixels as we go. Check rows above and below and
; save their coordinates to fill later if necessary

seekright:
  rrc b
  inc d
  jr z,rightedge
goright:
  call scrpos
  jr z,rightedge
  ld (hl),a
  inc e
  call checkadj
  dec e
  dec e
  call checkadj
  inc e
  jr seekright

; check to see if there's another row waiting to be filled

rightedge:
  pop de
  ld a,e
  inc a
  jr nz,nextrun
  ld    hl,(zx_stacklow)
  ret

; calculate the pixel address and whether or not it's set

scrpos:
  ld a,e
  and 248
  rra
  scf
  rra
  rra
  ld l,a
  xor e
  and 248
  xor e
  ld h,a
  ld a,l
  xor d
  and 7
  xor d
  rrca
  rrca
  rrca
  ld l,a
  ld a,b
  or (hl)
  cp (hl)
  ret

; check and save the coordinates of an adjacent row

checkadj:
  sla c
  ld a,e
  cp 192
  ret nc
  call scrpos+1
  ret z
  inc c
  bit 2,c
  ret nz
  pop hl
  push de
  jp (hl)