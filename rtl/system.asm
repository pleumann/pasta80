;
; PL0 built-in assembler functions
;

                LUA
                function SegInfo(Name, Start, End, Extra)
                    Len = End - Start
                    Str = string.format("%-9s: %5d bytes ($%4X-$%4X) %s", Name, Len, Start, End, Extra)
                    print(Str)
                end
                ENDLUA

                macro about name, addr, addr2
                    LUA
                        s = sj.calc("name")
                        a = sj.calc("addr")
                        z = sj.calc("addr2")
                        l = z - a
                        str = string.format("%-8s: %5d bytes (%4X-%4X)",s,l,a,z-1)
                        print(str)
                    ENDLUA
                endm
__cur_file      dw      0
__text_buf      dw      0

;
; Some stuff shared between the routines
;

__buffer        ds      32
__linemax:      ds      1
__linelen:      ds      1
__linebuf:      ds      128
;
; Boolean literals
;
__boolean1:     db      4,"True"
__boolean0:     db      5,"False"
__boolean_enum: dw __boolean0, __boolean1

;
; Signed 16 bit '=' check (with help from Ped7g)
;
; In:   HL (left), DE (right)
; Out:  A=0 if false, A=1 if true
;
__int16_eq0:    ld  de,0
__int16_eq:     xor a
                sbc hl,de
                ; add hl,de
                ret nz
                inc a
                ret

;
; Signed 16 bit '<>' check (with help from Ped7g)
;
; In:   HL (left), DE (right)
; Out:  A=0 if false, A=1 if true
;
__int16_neq0:   ld de,0
__int16_neq:    xor a
                sbc hl,de
                ; add hl,de
                ret z
                inc a
                ret

;
; Signed 16 bit 'lt' check (with help from Ped7g)
;
; In:   HL (left), DE (right)
; Out:  A=0 if false, A=1 if true
;
__int16_lt:     xor a
                sbc hl,de
                ; add hl,de
                jp pe,__int16_lt_of
                ret p
                inc a
                ret
__int16_lt_of:  ret m
                inc a
                ret

;
; Signed 16 bit '>=' check (with help from Ped7g)
;
; In:   HL (left), DE (right)
; Out:  A=0 if false, A=1 if true
;
__int16_geq:    xor a
                sbc hl,de
                ; add hl,de
                jp pe,__int16_geq_of
                ret m
                inc a
                ret
__int16_geq_of: ret p
                inc a
                ret

; Signed bc <= de <= hl check

__int16_case:   push    de
                call    __int16_geq
                pop     hl
                and     a
                ret     z
                push    hl
                ld      de,bc
                call    __int16_geq
                pop     de
                and     a
                ret     z
                inc     a
                ret

; Signed 16 bit left-shift
;
; Entry: HL (Number), E (number of Bits)
; Exit:  HL (Number)
__int_shl:      ld a,e
                and a
                ret z
                ld b,a
__int_shl1:
                sla l
                rl h
                djnz __int_shl1
                ret

; Signed 16 bit right-shift
;
; Entry: HL (Number), E (number of Bits)
; Exit:  HL (Number)
__int_shr:      ld a,e
                and a
                ret z
                ld b,a
__int_shr1:
                srl h
                rr l
                djnz __int_shr1
                ret

;
; Signed 16 bit multiplication (taken from Leventhal/Saville)
;
; Entry:  HL (multiplicand), DE (multiplier)
; Exit:   HL (product)
; Uses:   AF, BC
;
__mul16:        ld      c,l
                ld      b,h
                ld      hl,0
                ld      a,15
__mul16a:       sla     e
                rl      d
                jr      nc,__mul16b
                add     hl,bc
__mul16b:       add     hl,hl
                dec     a
                jr      nz,__mul16a
                or      d
                ret     p
                add     hl,bc
                ret

;
; Signed 16 bit division (taken from Leventhal/Saville)
;
; Entry:  HL (dividend), DE (divisior)
; Exit:   HL (quotient), DE (remainder), CF (div-by-zero)
; Uses:   AF, BC
;
__sdiv16:       ld      a,h
                ld      (__srem),a
                xor     d
                ld      (__squot),a
                ld      a,d
                or      a
                jp      p,__chkde
                sub     a
                sub     e
                ld      e,a
                sbc     a,a
                sub     d
                ld      d,a
__chkde:        ld      a,h
                or      a
                jp      p,__dodiv
                sub     a
                sub     l
                ld      l,a
                sbc     a,a
                sub     h
                ld      h,a
__dodiv:        call    __udiv16
                ret     c
                ld      a,(__squot)
                or      a
                jp      p,__dorem
                sub     a
                sub     l
                ld      l,a
                sbc     a,a
                sub     h
                ld      h,a
__dorem:        ld      a,(__srem)
                or      a
                ret     p
                sub     a
                sub     e
                ld      e,a
                sbc     a,a
                sub     d
                ld      d,a
                ret
__udiv16:       ld      a,e
                or      d
                jr      nz,__divide
                ld      hl,0
                ld      d,h
                ld      e,l
                scf
                ret
__divide:       ld      c,l
                ld      a,h
                ld      hl,0
                ld      b,16
                or      a
__dvloop:       rl      c
                rla
                rl      l
                rl      h
                push    hl
                sbc     hl,de
                ccf
                jr      c,__drop
                ex      (sp),hl
__drop:         inc     sp
                inc     sp
                djnz    __dvloop
                ex      de,hl
                rl      c
                ld      l,c
                rla
                ld      h,a
                or      a
                ret

__squot:        ds      1
__srem:         ds      1
__count:        ds      1

;
; Multiplication by 10
;
; Entry:  HL (multiplicand)
; Exit:   HL (product)
; Uses:   DE
;
__mul10:
                ex      hl,de
                add     hl,hl
                ld      de,hl
                add     hl,hl
                add     hl,hl
                add     hl,de
                ex      hl,de
                ret

;
; Division by 10 (taken from Z80 Heaven)
;
; Entry:  HL (dividend)
; Exit:   HL (quotient), A (remainder)
; Uses:   AF, BC
;
__div10:
                ld      bc,$0D0A
                xor     a
                add     hl,hl
                rla
                add     hl,hl
                rla
                add     hl,hl
                rla
__div10a:       add     hl,hl
                rla
                cp      c
                jr c,   __div10b
                sub c
                inc l
__div10b:       djnz __div10a
                ret

; Fallthrough intended
__abs16:
                bit     7,h
                ret     z

; Negation from Z80 Heaven
__neg16:
                xor     a
                sub     l
                ld      l,a
                sbc     a,a
                sub     h
                ld      h,a
                ret

;
;
;
;

__random:
    ex hl,de
    call __rand16
    res 7,h
    call __sdiv16
    ex hl,de
    ret

__random48:
    ex hl,de
    call __rand16
    push hl
    call __rand16
    push hl
    call __rand16
    pop de
    pop bc
    res 7,b
    ld   l,$81
    call FRAC
    ret

;#define smc    ;uncomment if you are using SMC
__rand16:
;;collaboration by Zeda with Runer112
;;160cc or 148cc if using SMC
;;26 bytes
;;cycle: 4,294,901,760 (almost 4.3 billion)
;#if defined(smc)
;seed1=$+1
;    ld hl,9999
;#else
    ld hl,(seed1)
;#endif
    ld b,h
    ld c,l
    add hl,hl
    add hl,hl
    inc l
    add hl,bc
    ld (seed1),hl
;#if defined(smc)
;seed2=$+1
;    ld hl,9999
;#else
    ld hl,(seed2)
;#endif
    add hl,hl
    sbc a,a
    and %00101101
    xor l
    ld l,a
    ld (seed2),hl
    add hl,bc
    ret
seed1:              dw 1234
seed2:              dw 5678

; String Compare Leventhal Saville P.290
;
; HL string 1
; DE string 2
; Result: Z=1, C=0 1<2
; Z =0, C=0 1>2
; Z=0, C=1 1<2


__strcmp:
        ld      a,(hl)
        ld      (__lens1),a
        ld      a,(de)
        ld      (__lens2),a
        cp      (hl)
        jr      c,__begcmp
        ld      a,(hl)
__begcmp:
        or      a
        jr      z,__cmplen
        ld      b,a
        ex      de,hl
__cmplp:
        inc     hl
        inc     de
        ld      a,(de)
        cp      (hl)
        ret     nz
        djnz    __cmplp
__cmplen:
        ld      a,(__lens1)
        ld      hl,__lens2
        cp      (hl)
        ret
__lens1:
        ds      1
__lens2:
        ds      1

; String concatenate
; HL = String 1, DE = String 2, B = max len, CF=1 if shortened
__strcat:
        ld      (__s1adr),hl
        push    bc
        ld      a,(hl)
        ld      (__s1len),a
        ld      c,a
        ld      b,0
        add     hl,bc
        inc     hl
        ld      a,(de)
        ld      (__s2len),A
        inc     de
        pop     bc
        ld      c,a
        ld      a,(__s1len)
        add     a,c
        jr      c,__toolng
        cp      b
        jr      z,__lenok
        jr      c,__lenok
__toolng:
        ld      a,255
        ld      (__strgov),a
        ld      a,(__s1len)
        ld      c,a
        ld      a,b
        sub     c
        ret     c
        ld      (__s2len),a
        ld      a,b
        ld      (__s1len),a
        jr      __docat
__lenok:
        ld      (__s1len),a
        sub     a
        ld      (__strgov),a
__docat:
        ld      a,(__s2len)
        or      a
        jr      z,__exit
        ld      c,a
        ld      b,0
        ex      de,hl
        ldir
__exit:
        ld      a,(__s1len)
        ld      hl,(__s1adr)
        ld      (hl),a
        ld      a,(__strgov)
        rra
        ret
__s1adr:
        ds      2
__s1len:
        ds      1
__s2len:
        ds      1
__strgov:
        ds      1

; String pos
__strpos:
        ld      (__string),hl
        ex      de,hl
        ld      a,(hl)
        or      a
        jr      z,__notfnd
        inc     hl
        ld      (__substg),hl
        ld      (__sublen),a
        ld      c,a
        ld      a,(de)
        or      a
        jr      z,__notfnd
        sub     c
        jr      c,__notfnd
        inc     a
        ld      b,a
        sub     a
        ld      (__index),a
__slp1:
        ld      hl,__index
        inc     (hl)
        ld      hl,__sublen
        ld      c,(hl)
        ld      hl,(__string)
        inc     hl
        ld      (__string),hl
        ld      de,(__substg)
__cmplp2:
        ld      a,(de)
        cp      (hl)
        jr      nz,__slp2
        dec     c
        jr      z,__found
        inc     hl
        inc     de
        jr      __cmplp2
__slp2:
        djnz    __slp1
        jr      __notfnd
__found:
        ld      a,(__index)
        ret
__notfnd:
        sub     a
        ret
__string:
        ds      2
__substg:
        ds      2
__slen:
        ds      1
__sublen:
        ds      1
__index:
        ds      1


; strcpy
__strcpy:
        ld      (__maxlen),a
        sub     a
        ld      (de),a
        ld      (__cpyerr),a
        or      b
        ret     z
        ld      a,(__maxlen)
        or      a
        jr      z,__erexit
        ld      a,c
        or      a
        jr      z,__erexit
        ld      a,(hl)
        cp      c
        ret     c
        ld      a,c
        add     a,b
        jr      c,__recalc
        dec     a
        cp      (hl)
        jr      c,__cnt1ok
        jr      z,__cnt1ok
__recalc:
        ld      a,255
        ld      (__cpyerr),a
        ld      a,(hl)
        sub     c
        inc     a
        ld      b,a
__cnt1ok:
        ld      a,(__maxlen)
        cp      b
        jr      nc,__cnt2ok
        ld      b,a
        ld      a,255
        ld      (__cpyerr),a
__cnt2ok:
        ld      a,b
        or      a
        jr      z,__erexit
        ld      b,0
        add     hl,bc
        ld      (de),a
        ld      c,a
        inc     de
        ldir
        ld      a,(__cpyerr)
__okexit:
        or      a
        ret     z
__erexit:
        scf
        ret
__maxlen:
        ds      1
__cpyerr:
        ds      1

;Insert
__strins:
        sub     a
        ld      (__inserr),a
        ld      a,(de)
        or      a
        ret     z
__idx0:
        ld      a,c
        or      a
        scf
        ret     z
__chklen:
        ld      a,(de)
        add     a,(hl)
        jr      c,__trunc
        cp      b
        ld      a,(de)
        jr      c,__idxlen
        jr      z,__idxlen
__trunc:
        ld      a,255
        ld      (__inserr),a
        ld      a,b
        sub     (hl)
        ret     c
        scf
        ret     z
__idxlen:
        ld      b,a
        ld      a,(hl)
        cp      c
        jr      nc,__lenok2
        ld      c,a
        add     a,b
        ld      (hl),a
        ex      de,hl
        ld      a,c
        inc     a
        add     a,e
        ld      e,a
        jr      nc,__idxl1
        inc     d
__idxl1:
        ld      a,255
        ld      (__inserr),a
        jr      __mvesub
__lenok2:
        push    bc
        push    de
        ld      e,a
        ld      d,0
        add     a,b
        ld      (hl),a
        ld      a,e
        sub     c
        inc     a
        add     hl,de
        ld      e,l
        ld      d,h
        ld      c,b
        ld      b,0
        add     hl,bc
        ex      de,hl
        ld      c,a
        lddr
        ex      de,hl
        inc     de
        pop     hl
        pop     bc
__mvesub:
        inc     hl
        ld      c,b
        ld      b,0
        ldir
        ld      a,(__inserr)
        rra
        ret
__inserr:
        ds      1

; Delete
__strdel:
        sub     a
        ld      (__delerr),a
        or      b
        ret     z
        ld      a,c
        or      a
        scf
        ret     z
        ld      a,(hl)
        cp      c
        ret     c
        ld      a,c
        add     a,b
        jr      c,__trunc2
        ld      e,a
        dec     a
        cp      (hl)
        jr      c,__cntok
        jr      z,__trunc2
        ld      a,255
        ld      (__delerr),a
__trunc2:
        ld      a,c
        dec     a
        ld      (hl),a
        ld      a,(__delerr)
        rra
        ret
__cntok:
        ld      a,(hl)
        ld      d,a
        sub     b
        ld      (hl),a
        ld      a,d
        sub     e
        inc     a
        push    hl
        ld      b,0
        add     hl,bc
        ex      (sp),hl
        ld      d,0
        add     hl,de
        pop     de
        ld      c,a
        ldir
__okexit2:
        or      a
        ret
__delerr:
        ds      1
;
; String "equal" check. Arguments and result on stack.
;
; Entry:    -
; Exit:     -
; Uses:     HL,DE,BC,AF
;
__streq:
        ld      hl,258
        add     hl,sp
        ld      de,hl
        dec     h
__streqnosp:
        ld      a,(de)
        cp      (hl)
        jr      nz,__streq0     ; Fast track if lengths differ
        call    __strcmp
        jr      z,__streq1
__streq0:
        ld      de,0

;        ld      (hl),de
        ret
__streq1:
        ld      de,1
;        ld      (hl),de
;        ld      a,1
        ret

;
; String "less than" check. Arguments and result on stack.
;
; Entry:    -
; Exit:     -
; Uses:     HL,DE,BC,AF
;
__strlt:
        ld      hl,258
        add     hl,sp
        ld      de,hl
        dec     d
        call    __strcmp
        jr      c,__strlt1
__strlt0:
;        ld      a,0
        ld      de,0
;        ld      (hl),de
        ret
__strlt1:
        ld      de,1
;        ld      (hl),de
;        ld      a,1
        ret

;
; String "less than or equal" check. Arguments and result on stack.
;
; Entry:    -
; Exit:     -
; Uses:     HL,DE,BC,AF
;
__strleq:
        ld      hl,258
        add     hl,sp
        ld      de,hl
        dec     d
        call    __strcmp
        jr      z,__strleq1
        jr      c,__strleq1
__strleq0:
        ld      de,0
;        ld      (hl),de
;        ld      a,0
        ret
__strleq1:
        ld      de,1
;        ld      (hl),de
;        ld      a,1
        ret

; Concat
__stradd:
        ld      hl,258
        add     hl,sp
        ld      de,hl
        dec     d
        ld      b,255
        call    __strcat
        ret

__pos:
        ld      hl,514
        add     hl,sp
        push    hl
        dec     h
        ld      de,hl
        dec     d
        ex      de,hl
        call    __strpos
        pop     hl
        ld      (hl),a
        inc     hl
        ld      (hl),0
        ret

__copy:
        ld      hl,2
        add     hl,sp
        ld      b,(hl)
        inc     hl
        inc     hl
        ld      c,(hl)
        inc     hl
        inc     hl
        ld      de,hl
        inc     d
        ld      a,255
        call    __strcpy
        ret

__insert:
        ld      hl,2
        add     hl,sp
        ld      c,(hl)
        inc     hl
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      de,hl
        ld      h,(hl)
        ld      l,a
        inc     de

        ld      b,255
        call    __strins
        ret

__delete:
        ld      hl,2
        add     hl,sp
        ld      b,(hl)
        inc     hl
        inc     hl
        ld      c,(hl)
        inc     hl
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        call    __strdel
        ret

__str_int:
        ld      hl,2
        add     hl,sp
        ld      bc,(hl)
        inc     hl
        inc     hl
        ld      de,(hl)
        ld      hl,bc
        push    hl
        inc     hl
        call    __itoa
        pop     hl
        ld      (hl),a
        ret

__val_int:
        ld      hl,6
        add     hl,sp
        ld      a,(hl)
        inc     hl
        call    __atoi
        ld      hl,4
        add     hl,sp
        ld      bc,(hl)
        ld      hl,bc
        ld      (hl),de
        pop     de
        ld      hl,260
        add     hl,sp
        ld      sp,hl
        push    de
        ret                     ; FIXME: Error reporting

__val_float:
        ld      hl,6
        add     hl,sp
;        ld      a,(hl)
;        inc     hl
        call    __atof
        exx
        pop     de
        pop     bc
        pop     hl
        call    __storefp
        exx
        ld      hl,256
        add     hl,sp
        ld      sp,hl
        push    de
        ret                     ; FIXME: Error reporting

; string on stack, de table, b size, a contains code if found, 255 if not
__val_enum:     ld      hl,6
                add     hl,sp
                call    __atoe
                ld      a,d
                and     a
                jr      nz,__val_enum1

                ld      hl,4            ; Result
                add     hl,sp
                ld      bc,(hl)
                ld      hl,bc
                ld      (hl),e
                inc     hl
                ld      (hl),a
                ld      e,0

__val_enum1:    ld      hl,2            ; Error
                add     hl,sp
                ld      bc,(hl)
                ld      hl,bc
                ld      (hl),e
                xor     a
                inc     hl
                ld      (hl),a

                pop     de
                ld      hl,260
                add     hl,sp
                ld      sp,hl
                push    de
                ret                     ; FIXME: Error reporting

__val_enum2:    ld      a,c
                ld      hl,4
                add     hl,sp
                ld      bc,(hl)
                ld      hl,bc
                ld      (hl),a
                pop     de
                ld      hl,260
                add     hl,sp
                ld      sp,hl
                push    de
                ret                     ; FIXME: Error reporting

;
; String length. Arguments and result on stack.
;
; Entry:    -
; Exit:     -
; Uses:     HL,DE
;
__length:
        ld      hl,2
        add     hl,sp
        ld      d,0
        ld      e,(hl)
        inc     h
        ld      (hl),de
        ret

;
; Load string from HL to stack, return properly.
;
__loadstr:
        pop     bc
        ex      de,hl
        ld      hl,-256
        add     hl,sp
        ld      sp,hl
        ex      de,hl
        push    bc
        ld      a,255
        call    __movestr
        ret

;
; Store string of max length A from stack to address, cleanup stack, return properly.
;
__storestr:
        ld      hl,258
        add     hl,sp
        ld      de,(hl)
        dec     h
        call    __movestr
        pop     bc
        ld      hl,258
        add     hl,sp
        ld      sp,hl
        push    bc
        ret

;
; Move string of max length A from HL to DE, bypassing the stack.
;
__movestr:
        ld      b,(hl)
        cp      b
        jp      c,__movestr_1
        ld      a,b
__movestr_1:
        ld      (de),a
        and     a
        ret     z
        inc     hl
        inc     de
        ld      b,0
        ld      c,a
        ldir
        ret

__mkstr:        pop     de
                ld      hl,-256
                add     hl,sp
                ld      sp,hl
                ex      de,hl
                jp      (hl)

__rmstr:        pop     de
                ld      hl,256
                add     hl,sp
                ld      sp,hl
                ex      de,hl
                jp      (hl)

__char2str:     ld      b,e
                pop     de
                ld      hl,-254
                add     hl,sp
                ld      sp,hl
                ld      c,1
                push    bc
                ex      de,hl
                jp      (hl)

__load16:       pop     de
                ex      de,hl
                ld      (__load16ret),hl
                and     a
                sbc     hl,hl
                sbc     hl,bc
                add     hl,sp
                ld      sp,hl
                ex      de,hl
                ldir
                ld      hl,(__load16ret)
                jp      (hl)
__load16ret:    ds      2

__store16:      pop     hl
                ld      (__store16ret),hl
                ld      hl,bc
                add     hl,sp
                ld      de,(hl)
                ld      hl,0
                add     hl,sp
                ldir
                ld      sp,hl
                pop     hl
                ld      hl,(__store16ret)
                jp      (hl)
__store16ret:   ds      2

__inc16:        inc     (hl)
                ret     nz
                inc     hl
                inc     (hl)
                ret

__inc16by:      ld      e,(hl)
                inc     hl
                ld      d,(hl)
                ex      de,hl
                add     hl,bc
                ex      de,hl
                ld      (hl),d
                dec     hl
                ld      (hl),e
                ret

__dec16:        ld      a,(hl)
                sub     1
                ld      (hl),a
                ret     nc
                inc     hl
                dec     (hl)
                ret

__dec16by:      ld      e,(hl)
                inc     hl
                ld      d,(hl)
                ex      de,hl
                and     a
                sbc     hl,bc
                ex      de,hl
                ld      (hl),d
                dec     hl
                ld      (hl),e
                ret

                include "math48.asm"

                macro constfp xx,yy,zz
                        ld      hl,xx
                        ld      de,yy
                        ld      bc,zz
                endm

                macro pushfp
                        push    bc
                        push    de
                        push    hl
                endm

                macro popfp
                        pop     hl
                        pop     de
                        pop     bc
                endm

; Load FP at address HL into BCDEHL
__loadfp:
        push    ix
        push    hl
        pop     ix
        ld      l,(ix+0)
        ld      h,(ix+1)
        ld      e,(ix+2)
        ld      d,(ix+3)
        ld      c,(ix+4)
        ld      b,(ix+5)
        pop     ix
        ret

; Store FP into BCDEHL into address HL
__storefp:
        push    ix
        push    hl
        pop     ix
        exx
        ld      (ix+0),l
        ld      (ix+1),h
        ld      (ix+2),e
        ld      (ix+3),d
        ld      (ix+4),c
        ld      (ix+5),b
        pop     ix
        ret

__flteq:
        call    CMP
        jr      z,__flteq1
__flteq0:
        ld      de,0
        ret
__flteq1:
        ld      de,1
        ret

__fltneq:
        call    CMP
        jr      nz,__fltneq1
__fltneq0:
        ld      de,0
        ret
__fltneq1:
        ld      de,1
        ret

__fltlt:
        call    CMP
        jr      c,__fltlt1
__fltlt0:
        ld      de,0
        ret
__fltlt1:
        ld      de,1
        ret

__fltleq:
        call    CMP
        jr      c,__fltleq1
        jr      z,__fltleq1
__fltleq0:
        ld      de,0
        ret
__fltleq1:
        ld      de,1
        ret

__fltpwr2:
        call    EQUAL
        call    FPMUL
        ret

__fltrnd:
        exx
        ld      hl,$0080
        ld      de,$0000
        ld      bc,$0000
        exx
        bit     7,b
        jr      nz,__fltrnd_neg
        call    FPADD
        jp      FIX
__fltrnd_neg:
        call    FPSUB
        jp      FIX

__atof:
        push    ix
        ld      de,hl
        ld      ix,de
        inc     ix
        call    CNVN
        pop     ix
        ret

__ftoa:
        push    ix
        ld      ix,__ftoatmp + 1
        di
        call    FSTRR
        ei
        jr      nc,__ftoaok
        ld      hl,__ftoaerr
        pop     ix
        ret
__ftoaok:
        ld      ix,__ftoatmp
        ld      b,255
__ftoalp:
        inc     ix
        inc     b
        ld      a,(ix)
        cp      0
        jr      nz, __ftoalp
        ld      a,b
        ld      (__ftoatmp),a
        ld      hl,__ftoatmp
        pop     ix
        ret
__ftoatmp:
        db 31,  "                                "
__ftoaerr:
        db      5,"ERROR"

__putf:
        exx
        ld      hl,$090d
        exx
        call __ftoa
        call __puts
        ret

__putf_exp:
        exx
        push    bc
        ld      a,c
        cp      $09
        jr      nc,__putf_exp_0
        ld      a,1
        jr      __putf_exp_1
__putf_exp_0:
        sub     $07
        cp      $0a
        jp      c,__putf_exp_1

        ld      a,$09
__putf_exp_1:
        ld      h,a
        ld      l,$0d
        exx
        call    __ftoa
        pop     bc
        call    __puts_fmt
        ret

__putf_fix:
        exx
        push    de
        ld      a,c
;        cp      $10
;        jp      c,__putf_fix_1
;        ld      a,$0f
__putf_fix_1:
        or      $f0
        ld      h,a
        ld      l,$0a
        exx
        call    __ftoa
        pop     bc
        call    __puts_fmt
        ret

; ========================================================================
; Set support
; ========================================================================

;
; Set membership test
;
; Entry: (SP+2) 32 bytes set, (SP+34) 1 byte element
; Exit:  DE=1 if element in set, 0 otherwise
; Uses:  AF, HL, DE
;
__setin_new:
        ld      hl,34
        ld      d,h
        add     hl,sp       ; HL = sp+32
        ld      e,(hl)
        db      $ed,$94 ; pixelad
        db      $ed,$95 ; setae
        db      $ed,$24 ; mirror
        db      $ed,$34,$00,$c0; add hl,-$4000+2
        add     hl,sp       ; HL = sp+32+bit_offset
        ld      e,d         ; DE = 0 when bit is clear
        and     (hl)
        ret     z
        ld      e,1         ; DE = 1 when bit is set
        ret

; Set index to offset
; Entry E index
; Exit DE offset of byte (D always 0), A correct bit set
__setoff:
        ld      e,a
        srl     e
        srl     e
        srl     e
;        inc     e
;        inc     e
        ld      d,a
        ld      a,1
        bit     0,d
        jr      z,__setoff1
        sla     a
__setoff1:
        bit     1,d
        jr      z,__setoff2
        sla     a
        sla     a
__setoff2:
        bit     2,d
        jr      z,__setoff3
        sla     a
        sla     a
        sla     a
        sla     a
__setoff3:
        ld      d,0
        ret

;
; Set membership test
;
; Entry: (SP+2) 32 bytes set, (SP+34) 1 byte element
; Exit:  DE=1 if element in set, 0 otherwise
; Uses:  AF, HL, DE, BC
;
__setmember:
        ld      hl,34
        add     hl,sp
        ld      a,(hl)
        call    __setoff
        ld      hl,2
        add     hl,de
        ld      de,0
        add     hl,sp
        and     (hl)
        ret     z
        ld      de,1
        ret

__setinclude:
        ld      a,e
        call    __setoff
        add     hl,de
        or      (hl)
        ld      (hl),a
        ret

__setexclude:
        ld      a,e
        call    __setoff
        add     hl,de
        xor     255
        and     (hl)
        ld      (hl),a
        ret

;
; Superset of two sets
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  Result in set 2, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__setadd:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ld      b,c
__setadd1:
        ld      a,(de)
        or      (hl)
        ld      (hl),a
        inc     hl
        inc     de
        djnz    __setadd1
        ret

;
; Difference of two sets
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  Result in set 1, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__setsub:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ld      b,c
__setsub1:
        ld      a,(de)
        xor     255
        and     (hl)
        ld      (hl),a
        inc     hl
        inc     de
        djnz    __setsub1
        ret

;
; Intersection of two sets
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  Result in set 1, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__setmul:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ld      b,c
__setmul1:
        ld      a,(de)
        and     (hl)
        ld      (hl),a
        inc     hl
        inc     de
        djnz    __setmul1
        ret

;
; Set equality test (i.e. set 1 = set 2)
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  DE=1 if equal, 0 otherwise, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__seteq:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ld      b,c
__seteq1:
        ld      a,(de)
        cp      (hl)
        jr      nz,__seteq2
        inc     hl
        inc     de
        djnz    __seteq1
        ld      de,1
        ret
__seteq2:
        ld      de,0
        ret

;
; Subset-or-equal test (i.e. set 1 <= set 2)
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  DE=1 if set 1 <= set 2, 0 otherwise, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__setleq:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ld      b,c
__setleq1:
        ld      a,(de)
        ld      c,a
        or      (hl)
        cp      c
        jr      nz,__setleq2
        inc     hl
        inc     de
        djnz    __setleq1
        ld      de,1
        ret
__setleq2:
        ld      de,0
        ret

;
; Superset-or-equal test (i.e. set 1 >= set 2)
;
; Entry: (SP+2) 32 bytes set 2, (SP+34) 32 byte set 1
; Exit:  DE=1 if set 1 >= set 2, 0 otherwise, SP is unchanged
; Uses:  AF, HL, DE, BC
;
__setgeq:
        ld      hl,2
        add     hl,sp
        ld      de,hl
        ld      bc,32
        add     hl,bc
        ex      de,hl           ; Swap arguments
        ld      b,c
        jr      __setleq1       ; Let __setleq do the actual work

; +  ... a := a or b
; -  ... a := a and not b
; *  ... a := a and b

; =  ... a = b
; >= ... a or b = a
; <= ... a or b = b

; Include shared with in
; Exclude shared with in




__conout:       equ     __putc

;
; Print string to screen
;
; Entry:  HL (string address)
; Exit:   -
; Uses:   AF,BC
;
__puts:         ld      b,(hl)
                inc     b
                jr      __putschk
__putsloop:     ld      a,(hl)
                push    hl
                push    bc
                call    __putc
                pop     bc
                pop     hl
__putschk:      inc     hl
                djnz    __putsloop
                ret

__lineptr:      ds      2

__blanks:       ld      hl,(__lineptr)
__blanks1:      ld      a,(hl)
                cp      '!'
                ret     nc
                cp      0
                ret     z
                inc     hl
                ld      (__lineptr),hl
                jr      __blanks1

WORD:
__word:         ld      hl,(__lineptr)
                ld      de,__buffer + 1
                ld      b,30
                ld      c,0
__word1:        ld      a,(hl)
                ld      (de),a
                inc     de
                cp      '!'
                jr      c, __word2
                inc     hl
                inc     c
                djnz    __word1
__word2:        ld      (__lineptr),hl
                ld      a,c
                ld      (__buffer),a
                ret

__getn:         push    hl
                call    __blanks
                call    __word
                ld      hl,__buffer + 1
                ld      a,6
                call    __atoi
                pop     hl
                ld      (hl),de
                ret

__getr:         push    hl
                push    ix
                call    __blanks
                call    __word
                ld      ix,__buffer + 1
                call    CNVN
                pop     ix
                exx
                pop     hl
                call    __storefp
                ret

; hl address, de table, b=count
__gete:         push    hl
                push    de
                push    bc
                call    __blanks
                call    __word
                ld      hl,__buffer
                pop     bc
                pop     de
                call    __atoe
                pop     hl
                ld      a,d
                and     a
                ret     nz
                ld      (hl),e
                ret

__getc:         push    hl
                ld      hl,(__lineptr)
                ld      a,(hl)
                pop     de
                ld      (de),a
                cp      ' '
                ret     c
                inc     hl
                ld      (__lineptr),hl
                ret

__gets:         push    hl
                ld      hl,(__lineptr)
                ld      de,__linebuf
                and     a
                sbc     hl,de
                ex      de,hl
                ld      hl,(__linelen)
                ld      h,0
                and     a
                sbc     hl,de
                ex      de,hl
                ld      hl,(__lineptr)
                dec     hl
                ld      (hl),e
                pop     de
                call    __movestr
                ld      hl,(__lineptr)
                ld      e,a
                ld      d,0
                add     hl,de
                ld      (__lineptr),hl
                ret

;
; Print number to screen
;
; Entry:  HL (number)
; Exit:   -
; Uses:   AF,BC,DE
;
__strn:         ex      de,hl
                push    hl
                push    af
                ld      hl,__buffer+1
                call    __itoa
                ld      hl,__buffer
                ld      (hl),a
                pop     af
                pop     de
                call    __movestr
                ret

__strn_fmt:
                push    de
                ld      b,a
                push    bc
                call    __strn
                pop     de
                pop     hl
                call    __ralign
                ret

__strc:
                and     a
                ret     z
                ex      de,hl
                ld      (hl),1
                inc     hl
                ld      (hl),e
                ret

__strs:
                ld      hl,2
                add     hl,sp
                call    __movestr
                pop     de
                ld      hl,256
                add     hl,sp
                ld      sp,hl
                push    de
                ret

;
; Convert floating point to string, format 0 (default)
;
; Entry: A (maximum length), DE' (target address), BCDEHL (number)
; Exit:  -
__strf:
                push    af
                exx
                push    de
                ld      hl,$090d
                exx
                call    __ftoa
                pop     de
                pop     af
                call    __movestr
                ret

__strf_exp:
        exx
        push    bc
        push    af
        push    de
        ld      a,c
        cp      $09
        jr      nc,__strf_exp_0
        ld      a,1
        jr      __strf_exp_1
__strf_exp_0:
        sub     $07
        cp      $0a
        jp      c,__strf_exp_1
        ld      a,$09
__strf_exp_1:
        ld      h,a
        ld      l,$0d
        exx
        call    __ftoa
        pop     de
        pop     af
        push    af
        push    de
        call    __movestr
        pop     hl
        pop     af
        pop     de
        ld      d,a
        call    __ralign
        ret

__strf_fix:
        exx
        push    bc
        push    af
        push    hl
        ld      a,e
;        cp      $10
;        jp      c,__putf_fix_1
;        ld      a,$0f
__strf_fix_1:
        or      $f0
        ld      h,a
        ld      l,$0a
        exx
        call    __ftoa
        pop     de
        pop     af
        push    af
        push    de
        call    __movestr
        pop     hl
        pop     af
        pop     de
        ld      d,a
        call    __ralign
        ret

__stre:
                add     hl,bc
                add     hl,bc
                ld      b,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,b
                call    __movestr
                ret

;
; Right-align Pascal string, inserting spaces at the start.
;
; Pre:
;       HL: string address
;       D : maximum string length
;       E : desired string length
; Post:
;       AF / BC / DE / HL changed
;
__ralign:
        ld      a,d
        cp      e               ; Is desired length > maximum length?
        jp      nc,__ralign1
        ld      e,d

__ralign1:
        ld      a,(hl)
        cp      e               ; Do we already have desired length?
        ret     nc

        ld      b,0
        ld      c,a
        ld      a,e
        sub     (hl)
        ld      (hl),e          ; Set new length

        inc     hl
        add     hl,bc           ; Old end of string
        ld      e,a
        ld      d,0
        push    hl
        add     hl,de           ; New end of string
        ex      de,hl
        pop     hl
        inc     bc
        lddr                    ; Move string

        inc     hl
        ld      b,a
        ld      a,32

__ralign2:
        ld      (hl),a          ; Fill with spaces
        inc     hl
        djnz    __ralign2

        ret





;
; Print number to screen
;
; Entry:  HL (number)
; Exit:   -
; Uses:   AF,BC,DE
;
__putn:         ex      hl,de
                ld      hl,__buffer+1
                call    __itoa
                ld      hl,__buffer
                ld      (hl),a
                call    __puts
                ret

;
; Print enum to screen
;
; Entry:  HL (value), DE (literal table)
; Exit:   -
; Uses:   AF,BC,DE
;
__pute:
                add     hl,hl
                add     hl,de
                ld      de,(hl)
                ex      de,hl
                jp      __puts

;
; Formatted output. Similar to normal output, but BC contains field width.
;
__putc_fmt:
                ld      a,l
                ld      hl,__buffer+1
                ld      (hl),a
                dec     hl
                ld      (hl),1
                jp      __puts_fmt

__puts_fmt:
                push    hl
                ld      a,c
                sub     (hl)
                jr      z,__puts_fmt_1
                jr      c,__puts_fmt_1
                ld      b,a
__puts_fmt_loop:
                push    bc
                ld      a, ' '
                call    __putc
                pop     bc
                djnz    __puts_fmt_loop
__puts_fmt_1:
                pop     hl
                jp      __puts

__putn_fmt:     push    bc
                ex      hl,de
                ld      hl,__buffer+1
                call    __itoa
                ld      hl,__buffer
                ld      (hl),a
                pop     bc
                jp      __puts_fmt

__pute_fmt:
                add     hl,hl
                add     hl,de
                ld      de,(hl)
                ex      de,hl
                jp      __puts_fmt

;
; Print assertion failed message
;
; Entry:  HL Source file
;         DE Source line
;         BC Value
; Exit:   -
; Uses:   ?
;
__assert:
                ld      a,c
                and     a
                jr      z,__assert1
                ld      hl,(__assertpassed);
                inc     hl
                ld      (__assertpassed), hl
                ret
__assert1:      push    de
                push    hl
                ld      hl, __assert_msg_1
                call    __puts
                pop     hl
                call    __puts
                ld      hl, __assert_msg_2
                call    __puts
                pop     hl
                call    __putn
                call    __newline
                ld      hl,(__assertfailed)
                inc     hl
                ld      (__assertfailed), hl
                ret
__assert_msg_1: db 24,"*** Assertion failed in "
__assert_msg_2: db 7,", line "

__assertpassed: dw 0
__assertfailed: dw 0

;
; Signed 16 bits integer to string
;
; Entry:  HL (buffer), DE (value)
; Exit:   A (length)
; Uses:   *
;
__itoa:         ld      bc,0
                bit     7,d
                ex      de,hl
                jr      z,__itoa_loop1
                ex      de,hl
                ld      (hl),'-'
                inc     hl
                inc     c
                push    hl
                ld      hl,0
                and     a
                sbc     hl,de
                pop     de
__itoa_loop1:   push    bc              ; mod 10 and push
                call    __div10
                pop     bc
                add     a,'0'
                push    af
                inc     b
                ld      a,h
                or      l
                jr      nz,__itoa_loop1
                ex      de,hl
__itoa_loop2:   pop     af              ; pop and store
                ld      (hl),a
                inc     hl
                inc     c
                djnz    __itoa_loop2
                ld      a,c
                ret

;
; String to signed 16 bits integer
;
; Entry:    HL (buffer), A (length)
; Exit:     DE (value)
; Uses:     *
;
; TODO Report errors via carry or a register?
;
__atoi:         ld      de,0
                and     a
                ret     z
                ld      b,a
                ld      a,(hl)
                ld      c,a
                cp      '-'
                jr      z,__atoi_skip   ; Skip minus sign
                cp      '+'
                jr      z,__atoi_skip   ; Skip plus sign
__atoi_loop:    sub     '0'
                jr      c,__atoi_done   ; Not a digit
                cp      10
                jr      nc,__atoi_done  ; Not a digit
                push    hl
                call    __mul10
                ld      l,a
                ld      h,0
                add     hl,de
                ex      hl,de
                pop     hl
__atoi_skip:    inc     hl
                ld      a,(hl)
                djnz    __atoi_loop
__atoi_done:    ld      a,c             ; Fix sign, if necessary
                cp      '-'
                ret     nz
                and     a
                push    hl
                ld      hl,0
                sbc     hl,de
                ex      de,hl
                pop     hl
                ret

; hl string, de table, b size, out d=0 if found, e contains code, otherwise d=1
__atoe:         ld      c,0
                ex      de,hl
__atoe1:        push    hl
                push    de
                push    bc
                ld      a,(hl)
                inc     hl
                ld      h,(hl)
                ld      l,a
                call    __streqnosp
                ld      a,e
                pop     bc
                pop     de
                pop     hl
                and     a
                jr      nz,__atoe2
                inc     hl
                inc     hl
                inc     c
                djnz    __atoe1
                ld      de,257
                ret
__atoe2:        ld      d,0
                ld      e,c
                ret

__checkstack:
                ld      hl,57344
                and     a
                sbc     hl,sp
                ret     c
                ld      hl,__stackoverflow
                call    __puts
                jp      __done
__stackoverflow:
                db 14,"Stack overflow"
;
; Fill Char
;
; In: HL address, DE count, C value.
;
__fillchar:
                ld      a,d             ; Check for zero length
                or      e
                ret     z               ; Return if this is the case

                ld      (hl),c          ; Set first byte

                dec     de              ; Decrement counter

                ld      a,d             ; Check for zero length
                or      e
                ret     z               ; Return if this is the case

                ld      bc,de           ; Let LDIR do the remaining work
                ld      de,hl
                inc     de
                ldir

                ret

;
; Move
;
; In: HL source, DE destination, BC value.
;
__move:
                ld      a,b             ; Check for zero length
                or      c
                ret     z               ; Return if this is the case

                push    hl              ; Check if destination > source
                and     a
                sbc     hl,de
                pop     hl
                jr      c,__movedn      ; Move "downwards" if this is the case
__moveup:
                ldir
                ret
__movedn:
                add     hl,bc           ; Set HL to end of buffer
                dec     hl
                ex      de,hl           ; Set DE to end of buffer
                add     hl,bc
                dec     hl
                ex      de,hl
                lddr
                ret



__heapptr:
        dw      0

; In: HL pointer address, DE size
; Out: -
__getmem:
        push    ix
        push    hl
        call    __malloc
        pop     hl
        ld      a,ixl
        ld      (hl),a
        inc     hl
        ld      a,ixh
        ld      (hl),a
        pop     ix
        ret

; In: DE size
; Out: IX new block
__malloc:
;        db      $dd,01,00,00
        ld      bc,__heapptr
        ld      ix,(__heapptr)
__malloc_loop:
; Null pointer means heap exhausted
        ld      a,ixh
        or      ixl
        jr      z,__malloc_out_of_memory
; Check size of free block
        ld      l,(ix+2)
        ld      h,(ix+3)
        and     a
        sbc     hl,de
        jr      c,__malloc_next
        jr      nz,__malloc_check_larger
; Case 1: Block of exact size found, just remove if from list
        ld      a,(ix+0)
        ld      (bc),a
        inc     bc
        ld      a,(ix+1)
        ld      (bc),a
        ret
; Larger blocks can be split, if at least 4 bytes remain
__malloc_check_larger:
        ld      a,252
        and     l
        or      h
        jr      z,__malloc_next
; Case 2: Suitable larger block found, split it and adjust pointers
        exx
        ld      c,(ix+0)
        ld      b,(ix+1)
        exx

        push    ix
        add     ix,de
        exx
        ld      (ix+0),c
        ld      (ix+1),b
        exx
        ld      (ix+2),l
        ld      (ix+3),h

        ld      a,ixl
        ld      (bc),a
        inc     bc
        ld      a,ixh
        ld      (bc),a

        pop     ix

        ret
; Try next block
__malloc_next:
        ld      bc,ix
        exx
        ld      e,(ix+0)
        ld      d,(ix+1)
        ld      ix,de
        exx
        jp      __malloc_loop
; Display error message and halt
__malloc_out_of_memory:
        ld      hl,__malloc_error_message
        call    __puts
        jp      __done
__malloc_error_message:
        db      13,'Out of memory'

; In: HL pointer, DE size
__freemem:
;        db      $dd,01,00,00
        ld      bc,(__heapptr)
        ld      (hl),c
        inc     hl
        ld      (hl),b
        inc     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        dec     hl
        dec     hl
        dec     hl
        ld      (__heapptr),hl
        ret

__get_heap_start:
                ld      hl, eof
                ret

__get_heap_bytes:
                ld      hl, 57344
                ld      de, eof
                and     a
                sbc     hl,de
                ret

