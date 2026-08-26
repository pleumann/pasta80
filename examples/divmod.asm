;
; Pascal "DivMod" function: function DivMod(Divisor, Dividend: Integer; var Quotient: Integer): Integer
;
; Entry: HL (Divisor), DE (Dividend), BC (@Quotient)
; Exit:  HL (Remainder, the function result), (BC) set to Quotient
;
__divmod:
                ex      de,hl           ; HL = Dividend, DE = Divisor
                push    bc              ; save @Quotient (__sdiv16 clobbers BC)
                call    __sdiv16        ; HL = quotient, DE = remainder
                pop     bc
                ld      a,l
                ld      (bc),a
                inc     bc
                ld      a,h
                ld      (bc),a          ; store quotient
                ex      de,hl           ; HL = remainder (function result)
                ret
