;       Math48 Floating Point Package
;       Version 1.1 Revision 1
;       by Anders Hejlsberg
;       2532 Bytes

;HOPTABEL

        JP   FPADD
        JP   FPSUB
        JP   FPMUL
        JP   FPDIV
        JP   MOD
        JP   PWR
        JP   CMP

        JP   SQR
        JP   LN
        JP   EXP
        JP   LOG
        JP   SIN
        JP   COS
        JP   TAN
        JP   ATN
        JP   ACPI
        JP   INT
        JP   FRAC

        JP   EQUAL
        JP   MUL10
        JP   FIX
        JP   FLOAT

        JP   FSTRS
        JP   FSTRR
        JP   CNVN


SIGN:   EQU  80H
EXPN:   EQU  80H

IWIDTH: EQU  0F0H
FWIDTH: EQU  0FH


;FLOATING POINT ADDITION.

FPADD:  EXX             ;Er AC negativ?
        BIT  7,B
        EXX
        JP   NZ,SUB1    ;Ja => SUB1

ADD1:   EXX             ;Er AC' nul?
        LD   A,L
        OR   A
        EXX
        RET  Z          ;Ja => Returner

        EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        EXX

        LD   A,L        ;Er AC nul?
        OR   A
        JR   NZ,ADD2    ;Nej => ADD2
        EXX             ;Returner AC'
        RES  7,B        ;Positivt
        JR   ADD10

ADD2:   PUSH BC         ;Gem fortegn
        SET  7,B        ;Saet MSB
        XOR  A          ;Saet Z'
        EX   AF,AF'
        EXX
        SET  7,B        ;Saet MSB'
        LD   A,L        ;A=EXP'-EXP
        EXX
        SUB  L
        JR   Z,ADD4     ;EXP=EXP' => ADD4
        JR   NC,ADD3    ;EXP<EXP' => ADD3

        NEG             ;Goer A positiv
        EX   AF,AF'     ;Nulstil Z'
        DEC  A
        EX   AF,AF'
        EXX             ;Juster AC'

ADD3:   CALL SRIGHT     ;Skift til hoejre
        INC  L          ;Er EXP=EXP'?
        DEC  A
        JR   NZ,ADD3    ;Nej => ADD3

        EX   AF,AF'     ;Var det AC'?
        JR   Z,ADD4     ;Nej => ADD4
        EXX             ;Ja => Ombyt

ADD4:   POP  AF         ;Hent AC fortegn
        AND  SIGN       ;Negativt?
        JR   NZ,ADD5    ;Ja => ADD5

        CALL ADDAC      ;MANT=MANT+MANT'
        JR   NC,ADD9    ;Ikke overflow => ADD9
        CALL RIGHT      ;Roter til hoejre
        OR   A
        INC  L          ;Juster exponent
        JR   NZ,ADD9
        SCF
        JR   ADD10

ADD5:   CALL CMPAC      ;Sammenlign
        CCF             ;Komplementer carry og
        PUSH AF         ;gem som fortegn
        JR   Z,ADDZ     ;AC=AC' => ADDZ
        JR   C,ADD6     ;AC>AC' => ADD6
        EXX             ;AC<AC' => Ombyt
ADD6:   CALL SUBAC      ;MANT=MANT-MANT'
ADD7:   BIT  7,B        ;Normaliseret?
        JR   NZ,ADD8    ;Ja => ADD8
        CALL SLEFT      ;Skift til venstre
        INC  L          ;Er exponent -128?
        DEC  L
        JR   Z,ADDZ     ;Ja => AC lig nul
        DEC  L          ;Traek 1 fra exponent
        JR   ADD7

ADDZ:   CALL ZERO

ADD8:   POP  AF         ;Hent fortegn
ADD9:   JR   C,ADD9A    ;Carry => negativt
        RES  7,B
ADD9A:  OR   A

ADD10:  EXX             ;Hent AC'
        POP  HL
        POP  DE
        POP  BC
        EXX
        RET


;FLOATING POINT SUBTRAKTION

FPSUB:  EXX             ;Er AC negativ?
        BIT  7,B
        EXX
        JP   NZ,ADD1    ;Ja => ADD1

SUB1:   CALL FPNEG      ;AC=-AC
        CALL ADD1       ;Laeg AC' til AC

FPNEG:  INC  L          ;Er AC nul?
        DEC  L
        RET  Z          ;Ja => Returner
        EX   AF,AF'     ;Gem carry
        LD   A,B        ;Komplementer fortegn
        XOR  SIGN
        LD   B,A
        EX   AF,AF'     ;Hent carry
        RET


;FLOATING POINT DIVISION.

FPDIV:  EXX             ;Er AC' nul?
        INC  L
        DEC  L
        EXX
        SCF
        RET  Z          ;Ja => Overflow

        LD   A,L        ;Er AC nul?
        OR   A
        RET  Z          ;Ja => Returner

        EXX             ;Subtraher exponenter
        SUB  L
        EXX
        CCF             ;Juster exponent og
        CALL EXPSGN     ;udregn fortegn

        PUSH HL         ;Opret 6 bytes work-
        PUSH HL         ;space
        PUSH HL
        ADD  IX,SP

        EXX             ;5 bytes
        LD   L,5
        EXX
        LD   A,8        ;Med hver 8 bits

DIVI1:  EX   AF,AF'     ;Gem taeller
        CALL CMPAC      ;Er MANT>MANT'
        JR   C,DIVI2    ;Nej => DIVI2
        CALL SUBAC      ;Traek AC' fra AC

DIVI2:  CCF             ;Komplementer carry
        RL   L          ;Roter ind i resultat
        EX   AF,AF'     ;Hent bittaeller
        DEC  A          ;Byte faerdig?
        JR   NZ,DIVI3   ;Nej => DIVI3

        LD   (IX+5),L   ;Gem byte i buffer
        DEC  IX         ;Peg til naeste
        EXX             ;Er 5 bytes klaret?
        DEC  L
        EXX
        JR   Z,DIVI4    ;Ja => DIVI4
        LD   A,8        ;8 bits

DIVI3:  CALL SLEFT      ;Skift AC til venstre
        JR   NC,DIVI1   ;Ingen carry => DIVI1

        EX   AF,AF'     ;Gem taeller
        CALL SUBAC      ;MANT=MANT-MANT'
        OR   A          ;Nulstil carry
        JR   DIVI2

DIVI4:  CALL SLEFT      ;Udregn afrundingsbit
        JR   C,DIVI5
        CALL CMPAC
        CCF

DIVI5:  POP  HL         ;Hent resultat
        POP  DE
        POP  BC
        BIT  7,B        ;Normaliseret?
        JR   NZ,DIVI6   ;Ja => DIVI6

        CALL LEFT       ;Roter afrundingsbit
        JR   MUL5       ;ind i resultatet

DIVI6:  INC  L          ;Laeg 1 til exponent
        JR   NZ,MUL5
        DEC  L
        SCF
        JR   MUL5A


;FLOATING POINT MULTIPLIKATION

FPMUL:  EXX             ;Er AC' nul?
        LD   A,L
        OR   A
        EXX
        JP   Z,ZERO     ;Ja => Resultat 0

        LD   A,L        ;Er AC nul?
        OR   A
        RET  Z          ;Ja => Retur

        EXX             ;Adder exponenter
        ADD  A,L
        EXX             ;Juster exponent og
        CALL EXPSGN     ;udregn fortegn

        PUSH BC         ;Gem AC
        PUSH DE
        PUSH HL
        ADD  IX,SP      ;Peg IX til AC

        CALL ZERO       ;Nulstil resultat
        EXX             ;5 bytes
        LD   L,5
        EXX

MUL1:   LD   A,8        ;Bittaeller lig 8
        INC  IX         ;Hent ny byte
        LD   L,(IX+0)

MUL2:   EX   AF,AF'     ;Gem taeller
        RR   L          ;Roter byte til hoejre
        JR   NC,MUL3    ;Hvis carry saa laeg
        CALL ADDAC      ;AC' til resultatet

MUL3:   CALL RIGHT      ;Roter res. til hoejre
        EX   AF,AF'     ;Hent taeller
        DEC  A          ;Byte faerdig?
        JR   NZ,MUL2    ;Nej => MUL2
        EXX             ;5 bytes klaret?
        DEC  L
        EXX
        JR   NZ,MUL1    ;Nej => MUL1

        LD   L,(IX-5)   ;Hent exponent
        BIT  7,B        ;Normaliseret?
        JR   NZ,MUL4    ;Ja => MUL4

        EX   AF,AF'     ;Hent sidste carry
        CALL LEFT       ;Roter res. til venstre
        INC  L          ;Traek 1 fra exponent
        DEC  L
        JR   Z,MUL4
        DEC  L

MUL4:   POP  AF         ;Fjern workspace
        POP  AF
        POP  AF

MUL5:   OR   A          ;Status = OK
MUL5A:  EX   AF,AF'     ;Gem status
        POP  AF         ;Hent res. fortegn
        EXX
        POP  BC         ;Hent AC' fortegn
        POP  HL         ;Hent AC' exponent
        EXX
        POP  IX         ;Hent IX
        RES  7,B        ;Erstat MSB i AC med
        OR   B          ;fortegn
        LD   B,A
        EX   AF,AF'     ;Hent status
        INC  L
        DEC  L
        CALL Z,ZERO
        RET

;Juster exponent og udregn fortegn.

EXPSGN: JR   C,EXPS1    ;Carry => EXPS1
        ADD  A,EXPN     ;Juster exponent
        JR   C,EXPS2    ;Carry => EXPS2
        JR   EXPS3      ;Underflow

EXPS1:  ADD  A,EXPN     ;Juster exponent
        JR   C,EXPS3    ;carry => Overflow

EXPS2:  LD   L,A        ;Gem i exponent
        EX   (SP),IX    ;Gem IX
        EXX
        PUSH HL         ;Gem AC' exponent
        PUSH BC         ;Gem AC' fortegn
        LD   A,B        ;Udregn nyt fortegn
        SET  7,B
        EXX
        XOR  B
        AND  SIGN
        PUSH AF
        SET  7,B
        PUSH IX
        LD   IX,0       ;Nulstil IX
        RET

EXPS3:  POP  HL         ;Juster stakken
        RET  C          ;Carry => Returner

;Nulstil AC.

ZERO:   XOR  A          ;Nulstil carry, expo-
        LD   L,A        ;nent og mantissa
        LD   B,A
        LD   C,A
        LD   D,A
        LD   E,A
        LD   H,A
        RET

;Roter AC til hoejre.

SRIGHT: OR   A
RIGHT:  RR   B
        RR   C
        RR   D
        RR   E
        RR   H
        RET

;Roter AC til venstre.

SLEFT:  OR   A
LEFT:   RL   H
        RL   E
        RL   D
        RL   C
        RL   B
        RET

;Laeg AC' til AC.

ADDAC:  LD   A,H
        EXX
        ADD  A,H
AAC1:   EXX
        LD   H,A
        LD   A,E
        EXX
        ADC  A,E
        EXX
        LD   E,A
        LD   A,D
        EXX
        ADC  A,D
        EXX
        LD   D,A
        LD   A,C
        EXX
        ADC  A,C
        EXX
        LD   C,A
        LD   A,B
        EXX
        ADC  A,B
        EXX
        LD   B,A
        RET

;Traek AC' fra AC.

SUBAC:  LD   A,H
        EXX
        SUB  H
SAC1:   EXX
        LD   H,A
        LD   A,E
        EXX
        SBC  A,E
        EXX
        LD   E,A
        LD   A,D
        EXX
        SBC  A,D
        EXX
        LD   D,A
        LD   A,C
        EXX
        SBC  A,C
        EXX
        LD   C,A
        LD   A,B
        EXX
        SBC  A,B
        EXX
        LD   B,A
        RET

;Sammenlign AC med AC'.

CMPAC:  LD   A,B
        EXX
        CP   B
        EXX
        RET  NZ
        LD   A,C
        EXX
        CP   C
        EXX
        RET  NZ
        LD   A,D
        EXX
        CP   D
        EXX
        RET  NZ
        LD   A,E
        EXX
        CP   E
        EXX
        RET  NZ
        LD   A,H
        EXX
        CP   H
        EXX
        RET


;FLOATING POINT COMPARE.

CMP:    EXX             ;Er fortegn ens?
        LD   A,B
        EXX
        XOR  B
        JP   P,CMP1     ;Ja => CMP1
        LD   A,B        ;Fortegn fra AC til
        RLA             ;carry
        RET

CMP1:   BIT  7,B        ;Negative tal?
        JR   Z,CMP2     ;Nej => CMP2

        CALL CMP2       ;Sammenlign abs.vaerdi
        RET  Z          ;Ens => Returner
        CCF             ;Complementer resultat
        RET

CMP2:   LD   A,L        ;Er exponenter ens?
        EXX
        CP   L
        EXX
        RET  NZ         ;Nej => Returner
        OR   A          ;Er exponenter nul?
        RET  Z          ;Ja => Returner
        JP   CMPAC      ;Sammenlign AC med AC'


;FLOATING POINT INTEGER.

INT:    LD   A,L        ;Er exponent mindre
        SUB  EXPN+1     ;end nul?
        JP   C,ZERO     ;Ja => Resultat nul
        INC  A

        EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        EX   AF,AF'
        CALL ZERO       ;Nulstil AC'
        EX   AF,AF'

INT1:   SCF             ;Saet alle bits der har
        CALL RIGHT      ;en exponent stoerre
        DEC  A          ;end eller lig nul
        JR   NZ,INT1

        EXX             ;Nulstil alle bits i AC
        LD   A,H        ;der har en exponent
        EXX             ;mindre end 0
        AND  H
        EXX
        LD   H,A
        LD   A,E
        EXX
        AND  E
        EXX
        LD   E,A
        LD   A,D
        EXX
        AND  D
        EXX
        LD   D,A
        LD   A,C
        EXX
        AND  C
        EXX
        LD   C,A
        LD   A,B
        EXX
        AND  B
        EXX
        LD   B,A
INT2:   JP   ADD10      ;Hent AC'


;FLOATING POINT FRACTION.

;FRAC(X) udregnes af X-INT(X).

FRAC:   EXX
        PUSH BC
        PUSH DE
        PUSH HL
        EXX

        CALL EQUAL
        EXX
        CALL INT
        EXX
        CALL FPSUB
        JR   INT2


;MODULUS.

;X MOD Y beregnes af FRAC(X/Y)*Y.

MOD:    CALL FPDIV
        RET  C
        CALL FRAC
        JP   FPMUL


;KVADRATROD.

;Kvadratroden beregnes med Newton-Raphson
;iterationsmetoden. Et gaet udregnes ud fra
;det foregaaende gaet efter formelen:
;I(n+1)=(X/I(n)+I(n))/2.
;Som foerste gaet halveres X's exponent.
;Der fortsaettes indtil ABS(I(n+1)-I(n)) er
;mindre end den halve exponent af X minus 20.

SQR:    LD   A,L        ;Er AC nul?
        OR   A
        RET  Z          ;Ja => Returer

        BIT  7,B        ;Er AC negativ?
        SCF             ;Saet carry
        RET  NZ         ;Ja => Returner

        EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        EXX
        CALL EQUAL      ;AC'=AC
        LD   A,L        ;Foerste iteration:
        ADD  A,EXPN     ;halver exponenten
        SRA  A
        ADD  A,EXPN
        LD   L,A        ;Sammenligningsvaerdi
        SUB  20         ;er den halve exponent
        PUSH AF         ;Gem s.vaerdi
        EXX

SQR1:   PUSH BC         ;Gem tallet
        PUSH DE
        PUSH HL
        CALL FPDIV      ;Divider med og adder
        CALL FPADD      ;forrige gaet
        DEC  L          ;Halver
        PUSH BC         ;Gem dette gaet
        PUSH DE
        PUSH HL
        CALL FPSUB      ;Udregn forskellen mel-
        LD   A,L        ;lem de to gaet
        POP  HL         ;Hent det nye gaet
        POP  DE
        POP  BC
        EXX
        POP  HL         ;Hent tallet
        POP  DE
        POP  BC
        EX   (SP),HL    ;Hent s.vaerdi ind i H
        CP   H
        EX   (SP),HL    ;Fortsaet indtil forsk.
        JR   NC,SQR1    ;er lille nok

        POP  AF         ;Fjern s.vaerdi
        EXX
        OR   A          ;Nulstil carry
SQR2:   JP   ADD10      ;Hent AC'


;TANGENS.

;TAN(X) beregnes af SIN(X)/COS(X)

TAN:    EXX
        PUSH BC
        PUSH DE
        PUSH HL
        EXX
        CALL EQUAL
        CALL COS
        EXX
        CALL SIN
        CALL FPDIV
        JR   SQR2


;COSINUS.

;COS(X) beregnes af SIN(PI/2-X)

COS:    EXX
        PUSH BC
        PUSH DE
        PUSH HL
        CALL ACPI
        DEC  L
        CALL FPSUB
        EXX
        JR   SINC


;SINUS.

;SIN(X) beregnes paa flg. maade:
;Hvis ABS(X)>2*PI saa X=FRAC(X/(2*PI))*2*PI
;Hvis X<0 saa X=X+2*PI
;Hvis X>PI saa X=X-PI, fortegn -
;Hvis X>PI/2 saa X=PI-X
;Y=X/3, Z=Y^2
;SIN(Y)=Y(((((Z+K1)Z+K2)Z+K3)Z+K4)Z+K5)/K5
;K1=-110      K2=7920       K3=-332640
;K4=6652800   K5=-39916800
;SIN(X)=4(.75*SIN(Y)-SIN(Y)^3)

SIN:    EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
SINC:   CALL ACPI       ;AC'=2PI
        INC  L
        EXX

        LD   A,L        ;Hvis tallet er mindre
        CP   EXPN-20    ;end 1E-7 saa returner
        JP   C,SIN7

        PUSH BC         ;Er ABS(AC)>2PI
        RES  7,B
        CALL CMP
        POP  BC
        CALL NC,MOD     ;Ja => AC=AC MOD 2PI

SIN1A:  BIT  7,B        ;Hvis AC<0 saa laeg
        JR   Z,SIN2     ;2PI til AC
        CALL FPADD

SIN2:   EXX             ;AC'=PI
        DEC  L
        EXX
        CALL CMP        ;Er AC>PI?
        PUSH AF         ;Gem flag som fortegn
        JR   C,SIN3     ;Nej => SIN3
        CALL FPSUB      ;AC=AC-PI

SIN3:   EXX             ;AC'=PI/2
        DEC  L
        EXX
        CALL CMP        ;Er AC>PI/2?
        JR   C,SIN4     ;Nej => SIN4
        EXX             ;AC=PI-AC
        INC  L
        CALL FPSUB

SIN4:   LD   A,L        ;Hvis tallet er mindre
        CP   EXPN-20    ;end 1E-7 saa returner
        JR   C,SIN7A

        EXX             ;AC=AC/3
        LD   BC,02AAAH
        LD   DE,0AAAAH
        LD   HL,0AA7FH
        CALL FPMUL

        PUSH IX
        LD   IX,SINK-6
        LD   A,5
        CALL COMSER
        POP  IX

        CALL EQUAL      ;Gem i AC'
        CALL FPMUL      ;Udregn SIN(X)^3
        CALL FPMUL
        PUSH BC         ;Gem paa stakken
        PUSH DE
        PUSH HL
        EXX
        CALL EQUAL      ;Udregn .75*SIN(X)
        DEC  L
        DEC  L
        EXX
        DEC  L
        CALL FPADD
        EXX             ;Hent SIN(X)^3
        POP  HL
        POP  DE
        POP  BC
        EXX
        CALL FPSUB      ;Traek det fra
        INC  L          ;Gang med 4
        INC  L

SIN7A:  POP  AF         ;Indsaet fortegn
        INC  L
        DEC  L
        JR   Z,SIN7
        JR   C,SIN7
        LD   A,B
        XOR  SIGN
        LD   B,A
SIN7:   OR   A
        JP   ADD10      ;Hent AC'

;Konstanter for udregning af SINUS.

SINK:   DW 0DC00H,00000H,00087H  ;K1

        DW 07780H,00000H,0008DH  ;K2

        DW 0A26CH,00000H,00093H  ;K3

        DW 04B07H,00000H,00097H  ;K4

        DW 09845H,04000H,0009AH  ;K5



;TITALS LOGARITME.

;LOG(X) beregnes af LN(X)/LN(10).

LOG:    CALL LN
        RET  C
        EXX
        PUSH BC
        PUSH DE
        PUSH HL
        LD   BC,05E5BH  ;1/LN(10)
        LD   DE,0D8A9H
        LD   HL,0367FH
        CALL FPMUL
        JP   ADD10


;NATURLIGE LOGARITME.

;LN(X) beregnes paa flg. maade:
;X=Y*2^N, 1<=Y<2
;Z=Y*SQR(2)/2
;U=(Z-1)/(Z+1), V=U^2
;R=U((((((V+K1)V+K2)V+K3)V+K4)V+K5)V+K6)/K6
;Kn=13/(13-2n)
;LN(X)=2*R+LN(2)/2+N*LN(2)

LN:     INC  L          ;Er AC nul?
        DEC  L
        SCF             ;Indiker fejl
        RET  Z          ;Ja => Returner
        BIT  7,B        ;Negativ?
        RET  NZ         ;Ja => Returner

        EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        LD   BC,03504H  ;AC'=SQR(2)/2
        LD   DE,0F333H
        LD   HL,0FB80H
        EXX

        LD   A,L        ;Udregn N
        LD   L,EXPN+1   ;Udregn Y
        SUB  L
        PUSH AF         ;Gem N

        CALL FPMUL      ;Udregn Z
        EXX             ;Udregn U
        CALL AC1
        EXX
        CALL FPSUB
        PUSH BC
        PUSH DE
        PUSH HL
        EXX
        INC  L
        CALL FPADD
        EXX
        POP  HL
        POP  DE
        POP  BC
        CALL FPDIV

        PUSH IX         ;Udregn LN(Z)
        LD   IX,LNK-6
        LD   A,6
        CALL COMSER
        POP  IX

        INC  L          ;Laeg LN(2)/2 til
        EXX
        CALL ACLN2
        DEC  L
        EXX
        CALL FPADD
        POP  AF
        PUSH BC         ;Gem resultat
        PUSH DE
        PUSH HL
        LD   L,A        ;Udregn N*LN(2)
        LD   H,0
        JR   NC,LN1
        DEC  H
LN1:    CALL FLOAT
        EXX
        INC  L
        CALL FPMUL
        EXX
        POP  HL         ;Hent resultat
        POP  DE
        POP  BC
        CALL FPADD      ;Adder
        LD   A,L
        CP   EXPN-25    ;LN(X)<3E-8 => LN(X)=0
        CALL C,ZERO
        JP   ADD10      ;Hent AC'

;Konstanter for udregning af LN.

LNK:    DW 01745H,0D174H,05D81H  ;K1

        DW 038E3H,08E38H,0E481H  ;K2

        DW 06DB6H,0DB6DH,0B781H  ;K3

        DW 02666H,06666H,06682H  ;K4

        DW 00AAAH,0AAAAH,0AB83H  ;K5

        DW 05000H,00000H,00084H  ;K6



;POTENSOPLOEFTNING.

;X^Y beregnes af EXP(Y*LN(X)).

PWR:    LD   A,L
        OR   A
        RET  Z
        CALL LN
        RET  C
        CALL FPMUL
        RET  C


;EXPONENTIALFUNKTIONEN.

;Hvis X<0 saa udregnes EXP(X)=1/EXP(-X).
;EXP(X) beregnes paa flg. maade:
;EXP(X)=2^Y, Y=X/LN(2)
;2^Y=2^INT(Y)*2^Z, Z=FRAC(Y)
;2^Z udregnes af:
;2^Z=(((((((Z+K1)*Z+K2)*Z)2+K3)....)*Z+K7)/K7
;K1=6.6042604723   K2=62.027114868
;K3=444.01034843   K4=2563.5667136
;K5=11095.090786   K6=32013.685271
;K7=46185.984492

EXP:    EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        CALL ACLN2      ;AC'=LN(2)
        EXX

        OR   A          ;Gem fortegn
        BIT  7,B
        PUSH AF
        RES  7,B        ;Goer positivt

        CALL FPDIV      ;Udregn Y
        LD   A,L        ;Er Y>128?
        CP   EXPN+8
        JP   NC,EXP4    ;Ja => EXP4

        CALL EQUAL      ;AC'=Y
        CALL FRAC       ;Udregn Z=FRAC(Y)
        EXX             ;Udregn INT(Y)
        CALL FIX
        LD   A,L
        PUSH AF         ;Gem INT(Y)
        EXX

EXP1:   PUSH IX         ;Udregn 2^Z
        LD   IX,EXPK-6
        LD   A,7
        CALL CALCS
        POP  IX

EXP3:   POP  AF         ;Hent 2^INT(Y)
        ADD  A,L        ;Udregn 2^Z*2^INT(Y)
        LD   L,A
        JR   NC,EXP6    ;Ikke overflow => EXP6

EXP4:   POP  AF         ;Juster stakken
        SCF             ;Indiker overflow
EXP5:   JP   ADD10      ;Hent AC'

EXP6:   POP  AF         ;Hent fortegn
        JR   Z,EXP5     ;Positivt => EXP5
        EXX             ;Tag den reciprokke
        CALL AC1
        CALL FPDIV
        JR   EXP5

;Konstanter for udregning af EXP.

EXPK:   DW 05356H,01A0EH,0DE83H  ;K1

        DW 0781BH,0C3FFH,0FB86H  ;K2

        DW 05E01H,05318H,0F189H  ;K3

        DW 02039H,01142H,0418CH  ;K4

        DW 02D5CH,05CF6H,0DF8EH  ;K5

        DW 07A1BH,05EDBH,0CD8FH  ;K6

        DW 03469H,0FC07H,0E590H  ;K7


;ARCCUS TANGENS.

;Hvis X>1 udregnes ATN(X)=PI/2-ATN(1/X).
;Til beregning af ATN(X) bruges:
;Y=X^2, A=PI/24,
;ATN(X)=X(((((Y+K1)*Y+K2)*Y+K3)*Y+K4)*Y+K5)/K5,
;hvor 0<=X<A og
;K1=-11/9  K2=11/7  K3=-11/5  K4=11/3  K5=-11
;X bestemmes til at ligge i et af interv.:
;1. X<TAN(A)
;2. TAN(A)<=X<TAN(3*A)    ,K=2*A
;3. TAN(3*A)<=X<TAN(5*A)  ,K=4*A
;4. X>=TAN(5*A)           ,K=6*A
;Hvis X er i foerste interv. bruges formelen
;alene, men ellers bruges:
;Y=(X-TAN(K))/(1+X*TAN(K))
;ATN(X)=K+ATN(Y)

ATN:    LD   A,L
        OR   A
        RET  Z
        EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        PUSH IX
        CALL AC1        ;AC'=1
        EXX
        XOR  A          ;Nulstil flagbyte
        BIT  7,B        ;Er AC positiv?
        JR   Z,ATN1     ;Ja => ATN1

        INC  A          ;Saet negativflag
        RES  7,B        ;AC=ABS(AC)

ATN1:   PUSH AF         ;Gem flag
        CALL CMP        ;Er AC>1
        JR   C,ATN2     ;Ja => ATN2

        EXX             ;AC=1/AC
        CALL FPDIV
        POP  AF         ;Saet reciprokflag
        SET  7,A
        PUSH AF

ATN2:   EXX
        LD   BC,006CFH
        LD   DE,0E98EH
        LD   HL,04A7EH
        EXX
        CALL CMP        ;Er AC<TAN(PI/24)?
        JR   NC,ATN3    ;Nej => ATN3

        CALL ARCTAN     ;Udregn ATN(X)
        JR   ATN6

ATN3:   LD   IX,ATNK-18 ;Peg IX til skalerings-
        LD   A,2        ;konstanter

ATN4:   EX   AF,AF'     ;Gem taeller
        EXX
        LD   DE,18      ;Peg til naeste saet
        ADD  IX,DE
        CALL GETCIX     ;Hent oeverste endepkt.
        EXX
        CALL CMP        ;Er X i dette interval?
        JR   C,ATN5     ;Ja => ATN5
        EX   AF,AF'     ;Hent taeller
        DEC  A          ;Faerdig?
        JR   NZ,ATN4    ;Nej => ATN4

        EXX             ;Juster IX
        LD   DE,12
        ADD  IX,DE
        EXX

ATN5:   EXX
        CALL GTNCIX     ;Hent TAN(K)
        SET  7,B        ;Udregn X-TAN(K)
        CALL FPADD
        PUSH BC         ;Gem resultat
        PUSH DE
        PUSH HL
        CALL GETCIX     ;Hent TAN(K)
        CALL FPMUL      ;Udregn X*TAN(K)
        EXX
        CALL AC1        ;Laeg 1 til
        CALL FPADD
        EXX             ;Gem i AC'
        POP  HL         ;Hent forrige resultat
        POP  DE
        POP  BC
        CALL FPDIV      ;Udregn Y
        PUSH IX         ;Udregn ATN(Y)
        CALL ARCTAN
        POP  IX
        EXX
        CALL GTNCIX     ;Hent K
        CALL FPADD      ;Udregn K+ATN(Y)

ATN6:   POP  AF         ;Hent flagbyte
        RLA             ;Var X>1?
        JR   NC,ATN7    ;Nej => ATN7

        PUSH AF         ;Gem flagbyte
        EXX             ;Udregn PI/2-ATN(X)
        CALL ACPI
        DEC  L
        CALL FPSUB
        POP  AF         ;Hent flagbyte

ATN7:   POP  IX         ;Hent IX
        BIT  1,A        ;Var X<0?
        JR   Z,ATN8     ;Nej => ATN8
        SET  7,B        ;Resultat negativt
ATN8:   OR   A
        JP   ADD10      ;Hent AC'

;Konstanter til skalering af X under beregning
;af ATN.

ATNK:   DW 05413H,0CCCFH,0E77FH  ;TAN(3*A)

        DW 00930H,0A2F4H,0F67FH  ;TAN(2*A)

        DW 0060AH,091C1H,06A7FH  ;2*A

        DW 0446FH,08A9EH,0B580H  ;TAN(5*A)

        DW 013CDH,03A2CH,08280H  ;TAN(4*A)

        DW 0060AH,091C1H,06A80H  ;4*A

        DW 00000H,00000H,00081H  ;TAN(6*A)

        DW 0490FH,0DAA2H,02180H  ;6*A


;Konstanter for beregning af ATN.

ARCTK:  DW 09C71H,0C71CH,07281H  ;K1

        DW 04924H,09249H,02581H  ;K2

        DW 08CCCH,0CCCCH,0CD82H  ;K3

        DW 06AAAH,0AAAAH,0AB82H  ;K4

        DW 0B000H,00000H,00084H  ;K5


;Udregn taylorraekken for ARCCUS TANGENS.

ARCTAN: LD   IX,ARCTK-6
        LD   A,5

;COMSER udregner en potensraekke af formen:
;T=X*((((X^2+K1)*X^2+K2)....)*X^2+Kn)/Kn,
;hvor X er i AC, n er i A, og adressen paa
;konstanterne (minus 6) i IX.

COMSER: PUSH BC         ;Gem X
        PUSH DE
        PUSH HL
        PUSH AF         ;Gem laengde
        CALL EQUAL      ;Udregn Z=X^2
        CALL FPMUL
        POP  AF         ;Hent laengde
        CALL CALCS      ;Udregn raekken
        EXX             ;Hent X
        POP  HL
        POP  DE
        POP  BC
        JP   FPMUL      ;Gang med X

;CALCS udregner en potensraekke af formen:
;U=(((((Z+K1)*Z+K2)*Z+K3)....)*Z+Kn)/Kn,
;hvor Z er i AC, n er i A, og adressen paa
;konstanterne (minus 6) i IX.

CALCS:  EXX             ;Gem Z i AC'
        CALL AC1        ;Start med resultat=1
CALC1:  PUSH AF         ;Gang med Z
        CALL FPMUL
        POP  AF
        PUSH AF
        EXX
        PUSH BC         ;Gem Z
        PUSH DE
        PUSH HL
        CALL GTNCIX     ;Hent naeste konstant
        CALL FPADD      ;Laeg til resultat
        EXX             ;Hent Z
        POP  HL
        POP  DE
        POP  BC
        EXX
        POP  AF         ;Faerdig?
        DEC  A
        JR   NZ,CALC1   ;Nej => CALC1
        EXX
        CALL GETCIX
        EXX
        JP   FPDIV

;Saet AC lig den konstant IX peger paa.

GTNCIX: LD   DE,6
        ADD  IX,DE
GETCIX: LD   C,(IX+0)
        LD   B,(IX+1)
        LD   E,(IX+2)
        LD   D,(IX+3)
        LD   L,(IX+4)
        LD   H,(IX+5)
        RET

;Saet AC lig 2*PI.

ACPI:   LD   BC,0490FH
        LD   DE,0DAA2H
        LD   HL,02182H
        RET

;Saet AC lig LN(2).

ACLN2:  LD   BC,03172H
        LD   DE,017F7H
        LD   HL,0D280H
        RET

;FLOATING POINT EQUAL.

EQUAL:  PUSH BC
        PUSH DE
        PUSH HL
        EXX
        POP  HL
        POP  DE
        POP  BC
        RET

;FLOATING POINT TIL 16-BIT INTEGER MED
;2'S COMPLEMENT FORTEGN.

FIX:    OR   A
        BIT  7,L        ;Exponent<0?
        JR   Z,FIX4     ;Ja => FIX4
        BIT  7,B        ;Gem fortegn
        EX   AF,AF'
        SET  7,B        ;Saet MSB

FIX1:   LD   A,EXPN+15  ;Test exponent
        CP   L
        RET  C          ;EXP>15 => overflow
        JR   Z,FIX2     ;EXP=15 => FIX2
        CALL SRIGHT     ;EXP<15 => roter til
        INC  L          ;hoejre og laeg 1 til
        JR   FIX1       ;exponent

FIX2:   CALL SRIGHT     ;Roter til hoejre
        EX   AF,AF'     ;Negativt fortegn?
        JR   Z,FIX3     ;Nej => INT2
        LD   HL,0       ;Tag 2's complement
        SBC  HL,BC
        OR   A          ;Nulstil carry
        RET

FIX3:   LD   H,B        ;Hent tallet
        LD   L,C
        RET

FIX4:   LD   HL,0       ;Underflow
        RET

;16-BIT INTEGER MED 2'S COMPLEMENT FORTEGN
;TIL FLOATING POINT.

FLOAT:  LD   A,H        ;Er HL=0?
        OR   L
        JP   Z,ZERO     ;Ja => ZERO

        BIT  7,H        ;Er HL negativ?
        JR   Z,FLT1     ;Nej => FLT1

        EX   DE,HL      ;Tag 2's complement
        LD   HL,0
        OR   A
        SBC  HL,DE

FLT1:   EX   AF,AF'     ;Gem fortegn i F'
        LD   B,H        ;Saet mantissa
        LD   C,L
        LD   DE,0
        LD   HL,EXPN+16 ;Saet exponent

FLT2:   BIT  7,B        ;Normaliser
        JR   NZ,FLT3
        CALL SLEFT
        DEC  L
        JR   FLT2

FLT3:   EX   AF,AF'     ;Negativt?
        RET  C          ;Ja => Retur
        RES  7,B        ;Positivt
        RET


;FLYDENDE TAL TIL TEKSTSTRENG.

;Resultatet afleveres i den buffer IX peger
;paa, og er afsluttet med et 0.
;Udskriftens format afgoeres af H' og L'.

;Register L':

;Bit 0    Udskriftstype
;           0 - Fastkomma notation
;           1 - Exponentiel notation
;Bit 2-1  Fortegnsformat
;           00 - Intet fortegn
;           01 - AC>=0: Intet fortegn
;                AC<0:  "-"
;           10 - AC>=0: " "
;                AC<0:  "-"
;           11 - AC>=0: "+"
;                AC<0:  "-"
;Bit 3    Decimaldelsformat
;           0 - Kun betydende cifre
;           1 - Skriv alle cifre
;Bit 4    Heltalsdelformat
;           0 - Kun betydende cifre
;           1 - Blanktegn foer betydende cifre

;Register H':

;Bit 3-0  Decimalfeltets laengde (0-15)
;Bit 7-4  Heltalsfeltets laengde (1-15)
;         Bruges kun hvis bit 0 i L' er 0


FSTRR:  EXX
        LD   A,L        ;Gem format i A og A'
        EX   AF,AF'
        LD   A,H
        EXX
        JR   OUTN1

;FLYDENDE TAL TIL TEKSTSTRENG.

;Formatet efterfoelger kaldet som to bytes, der
;har samme betydning som L' henholdsvis H' ved
;kald af FSTRR.

FSTRS:  EX   (SP),IX
        LD   A,(IX+0)    ;Gem format i A og A'
        INC  IX
        EX   AF,AF'
        LD   A,(IX+0)
        INC  IX
        EX   (SP),IX

OUTN1:  PUSH IX         ;Gem IX,IY,AC,AC'
        PUSH IY
        PUSH BC
        PUSH DE
        PUSH HL
        EXX
        PUSH BC
        PUSH DE
        PUSH HL
        EXX
        LD   IY,-13     ;Opret en 13 bytes
        ADD  IY,SP      ;buffer paa stakken
        LD   SP,IY
        EXX
        LD   E,A        ;Gem formatet
        EX   AF,AF'
        LD   D,A
        PUSH DE
        EXX

        INC  L          ;Er AC nul?
        DEC  L
        JR   NZ,DIGITS  ;Nej => DIGITS

        POP  DE         ;Hent formatet
DZERO:  LD   (IY+0),L   ;Marker bufferslut
        JP   OUTM

DIGITS: PUSH BC         ;Gem BC
        RES  7,B        ;Goer AC positiv
        LD   A,L        ;Hent exponent
        EXX

;Udregn titalsexponenten udfra totalsexponenten
;paa foelgende maade:
;E10=INT(E2*LOG(2))=INT((E2*77+5)/256)

        LD   H,0        ;HL=toexponent
        SUB  EXPN
        JR   NC,SC1
        DEC  H
SC1:    LD   L,A
        PUSH HL         ;HL=HL*77+5
        ADD  HL,HL
        ADD  HL,HL
        PUSH HL
        ADD  HL,HL
        LD   D,H
        LD   E,L
        ADD  HL,HL
        ADD  HL,HL
        ADD  HL,HL
        ADD  HL,DE
        POP  DE
        ADD  HL,DE
        POP  DE
        ADD  HL,DE
        LD   DE,5
        ADD  HL,DE
        LD   A,H        ;A=INT(HL/256)
        CP   -39
        JR   NZ,SC2
        INC  A
SC2:    LD   (IY+0),A   ;Gem tiexponent
        NEG             ;Multiplicer AC med
        CALL TENF       ;10^-tiexponent
        LD   A,L        ;Er AC<1?
        CP   EXPN+1
        JR   NC,SC3     ;Nej => SC3
        DEC  (IY+0)     ;Tiexponent-1
        CALL MUL10      ;AC=AC*10

SC3:    SET  7,B
        LD   A,EXPN+4
        SUB  L
        LD   L,0
        JR   Z,DIGI1
SC4:    CALL SRIGHT
        RR   L
        DEC  A
        JR   NZ,SC4

DIGI1:  LD   A,(IY+0)   ;Hent tiexp.
        LD   (IY+0),0   ;Marker bufferstart
        PUSH IY         ;Gem IY
        PUSH AF         ;Gem tiexp.
        LD   A,12       ;Udregn 12 cifre

DIGI2:  EX   AF,AF'     ;Gem taeller
        LD   A,B
        RRA
        RRA
        RRA
        RRA
        AND  0FH
        ADD  A,'0'
        INC  IY
        LD   (IY+0),A
        LD   A,B
        AND  0FH
        LD   B,A

        PUSH BC         ;Gang AC med 10
        PUSH DE
        PUSH HL
        SLA  L
        CALL LEFT
        SLA  L
        CALL LEFT
        EX   DE,HL
        EX   (SP),HL
        ADD  HL,DE
        POP  DE
        EX   (SP),HL
        ADC  HL,DE
        EX   DE,HL
        POP  HL
        EX   (SP),HL
        ADC  HL,BC
        LD   B,H
        LD   C,L
        POP  HL
        SLA  L
        CALL LEFT

        EX   AF,AF'     ;Hent taeller
        DEC  A          ;Faerdig?
        JR   NZ,DIGI2   ;Nej => DIGI2

        POP  AF         ;Hent tiexp.
        POP  IY         ;Hent IY,BC
        POP  BC
        INC  IY         ;Peg til foerste ciffer
        LD   C,A        ;Gem titalsexp. i C

ROUND:  POP  DE         ;Hent format
        LD   A,E        ;Udregn nummeret paa
        AND  FWIDTH     ;det ciffer der skal
        INC  A          ;afrundes fra
        BIT  0,D
        JR   NZ,ROU1
        ADD  A,C
        JP   M,DZERO    ;Neg. => Udskriv 0

ROU1:   CP   12         ;Max. nummer 11
        JR   C,ROU2
        LD   A,11

ROU2:   PUSH IY         ;Udregn adressen paa
        POP  HL         ;det ciffer der skal
        ADD  A,L        ;afrundes
        LD   L,A
        JR   NC,ROU3
        INC  H

ROU3:   LD   A,(HL)     ;Hent ciffer
        LD   (HL),0     ;Marker bufferslut
        CP   '5'        ;Afrunding?
        JR   C,ROU5     ;Nej => ROU5

ROU4:   DEC  HL         ;Tag forrige ciffer
        LD   A,(HL)
        OR   A          ;Bufferstart?
        JR   Z,ROU6     ;Ja => ROU6
        INC  A          ;Laeg 1 til ciffer
        LD   (HL),A
        CP   '9'+1      ;Var ciffer '9'?
        JR   C,OUTM     ;Nej => OUTM
        LD   (HL),0     ;Marker bufferslut
        JR   ROU4

ROU5:   DEC  HL         ;Tag forrige ciffer
        LD   A,(HL)
        SUB  '0'        ;Er det '0'?
        JR   NZ,OUTM    ;Nej => OUTM
        LD   (HL),A     ;Marker bufferslut
        JR   ROU5

ROU6:   INC  HL         ;Tallet var 9999...
        LD   (HL),'1'   ;Lav om til 10000...
        INC  HL
        LD   (HL),0
        INC  C          ;Laeg 1 til tiexp.

OUTM:   LD   A,(IY+0)   ;Er tallet 0?
        OR   A
        JR   NZ,OM1     ;Nej => OM1
        LD   B,A        ;Positivt fortegn
        LD   C,A        ;Tiexp = 0

OM1:    BIT  0,D        ;Exponentielt?
        JR   NZ,OM6     ;Ja => OM6

        LD   A,E        ;Udregn det antal
        AND  IWIDTH     ;blanktegn der skal
        RRCA            ;udskrives inden tallet
        RRCA
        RRCA
        RRCA
        DEC  A
        BIT  2,D
        JR   NZ,OM2
        BIT  1,D
        JR   Z,OM3
        BIT  7,B
        JR   Z,OM3
OM2:    DEC  A
OM3:    BIT  7,C
        JR   NZ,OM4
        SUB  C
OM4:    OR   A          ;Negativt?
        SCF             ;Indiker fejl
        JP   M,POPALL   ;Ja => POPALL

        BIT  4,D        ;Blanktegn?
        JR   Z,OM6      ;Nej => OM6

        LD   H,A        ;Gem blanktegn
        INC  H
OM5:    DEC  H
        JR   Z,OM6
        LD   A,' '
        CALL STOA
        JR   OM5

OM6:    BIT  7,B        ;Gem fortegn
        JR   Z,OM7
        LD   A,'-'
        BIT  2,D
        JR   NZ,OM8
        BIT  1,D
        JR   NZ,OM8
        JR   OM9
OM7:    BIT  2,D
        JR   Z,OM9
        LD   A,' '
        BIT  1,D
        JR   Z,OM8
        LD   A,'+'
OM8:    CALL STOA

OM9:    BIT  0,D        ;Exponentielt?
        JR   Z,OM10     ;Nej => OM10
        LD   H,C        ;Gem tiexp. i H
        LD   C,0        ;tiexp. = 0

OM10:   BIT  7,C        ;Er tiexp.>=0?
        JR   Z,OM11     ;Ja => OM11

        CALL STOZ       ;Gem '0'
        JR   OM12

OM11:   CALL STODIG     ;Gem de cifre der
        DEC  C          ;staar foer kommaet
        JP   P,OM11

OM12:   LD   A,E        ;Skal der cifre efter
        AND  FWIDTH     ;kommaet?
        JR   Z,OM15     ;Nej => OM15
        LD   E,A
        CALL MORED      ;Er der flere cifre?
        JR   Z,OM15     ;Nej => OM15

        LD   A,'.'      ;Gem '.'
        CALL STOA

OM13:   INC  C          ;Gem ubetydende nuller
        JR   Z,OM14
        CALL STOZ
        DEC  E
        JR   NZ,OM13

OM14:   DEC  E          ;Gem betydende cifre
        JP   M,OM15
        CALL STODIG
        CALL MORED
        JR   NZ,OM14

OM15:   BIT  0,D        ;Exponentielt?
        JR   Z,POPA1    ;Nej => POPA1

        LD   A,'E'      ;Gem 'E'
        CALL STOA
        LD   A,'+'      ;Gem fortegn
        BIT  7,H
        JR   Z,OEX1
        LD   A,H
        NEG
        LD   H,A
        LD   A,'-'
OEX1:   CALL STOA
        LD   A,H        ;Udregn 2-cifret exp.
        LD   B,'0'-1
OEX2:   INC  B
        SUB  10
        JR   NC,OEX2
        ADD  A,10+'0'
        LD   (IX+0),B   ;Gem exponent
        INC  IX
        CALL STOA

POPA1:  OR   A          ;Nulstil carry
POPALL: EX   AF,AF'     ;Gem status
        LD   (IX+0),0   ;Marker bufferslut
        LD   HL,13      ;Fjern talbuffer
        ADD  HL,SP
        LD   SP,HL
        POP  HL         ;Hent AC',AC,IY,IX
        POP  DE
        POP  BC
        EXX
        POP  HL
        POP  DE
        POP  BC
        POP  IY
        POP  IX
        EX   AF,AF'     ;Hent status
        RET

;Gem et ciffer i bufferen.

STODIG: LD   A,(IY+0)   ;Hent ciffer
        INC  IY
        OR   A          ;Bufferslut?
        JR   NZ,STOA    ;Nej => STOA
        DEC  IY         ;Juster
STOZ:   LD   A,'0'      ;Gem '0'
STOA:   LD   (IX+0),A
        INC  IX
        RET

;Undersoeg om der er flere cifre.

MORED:  BIT  3,D
        RET  NZ
        LD   A,(IY+0)
        OR   A
        RET

;Multiplicer AC med 10^A.

TENF:   PUSH AF         ;Gem AF
        OR   A          ;Positiv exponent?
        JP   P,TF1      ;Ja => TF1
        NEG             ;A=ABS(A)
TF1:    PUSH AF         ;Gem flag
        SRL  A          ;A=INT(A/4)
        SRL  A
        LD   HL,-6      ;Udregn offset til
        LD   DE,6       ;konstant nummer A
        INC  A
TF2:    ADD  HL,DE
        DEC  A
        JR   NZ,TF2
        EX   DE,HL
        PUSH IX         ;Gem IX
        LD   IX,CON10   ;Hent konstant
        ADD  IX,DE
        CALL GETCIX
        POP  IX         ;Hent IX
        POP  AF         ;Hent exponent
        AND  3          ;Juster faktor
TF3:    JR   Z,TF4
        PUSH AF
        CALL MUL10
        POP  AF
        DEC  A
        JR   TF3
TF4:    POP  AF         ;Hent exponent
        OR   A          ;Positiv?
        JP   P,FPMUL    ;Ja => Multipicer
        EXX             ;Nej => Divider
        JP   FPDIV

;Tier potens konstanter for konvertering.

CON10:  DW 00000H,00000H,00081H ;1E+00

        DW 01C40H,00000H,0008EH ;1E+04

        DW 03EBCH,02000H,0009BH ;1E+08

        DW 0684DH,0A510H,000A8H ;1E+12

        DW 00E1BH,0C9BFH,004B6H ;1E+16

        DW 02D78H,0EBC5H,0ACC3H ;1E+20

        DW 053C2H,01BCEH,0CDD0H ;1E+24

        DW 0013FH,03978H,0F9DEH ;1E+28

        DW 01DC5H,0ADA8H,02BEBH ;1E+32

        DW 04097H,0CE7BH,0C9F8H ;1E+36



;AC=ABS(AC)*10.

MUL10:  LD   A,L
        OR   A
        RET  Z
        SET  7,B
        PUSH BC
        PUSH DE
        LD   A,H
        CALL SRIGHT
        CALL SRIGHT
        ADD  A,H
        LD   H,A
        EX   (SP),HL
        ADC  HL,DE
        EX   DE,HL
        POP  HL
        EX   (SP),HL
        ADC  HL,BC
        LD   B,H
        LD   C,L
        POP  HL
        JR   NC,M10A
        CALL RIGHT
        INC  L
        SCF
        RET  Z
M10A:   LD   A,L
        ADD  A,3
        LD   L,A
        RES  7,B
        RET

;ASCII TIL FLOATING POINT.

CNVN:   EXX             ;Gem AC'
        PUSH BC
        PUSH DE
        PUSH HL
        LD   BC,0       ;Nulstil flag
        EXX
        CALL ZERO       ;Nulstil AC

        LD   A,(IX+0)   ;Hent foerste karakter
        CP   '+'        ;Plus?
        JR   Z,CNV1     ;Ja => CNV1
        DEC  IX
        CP   '-'        ;Minus?
        JR   NZ,CNV1    ;Nej => CNV1
        EXX             ;Saet minusflag
        SET  7,B
        EXX
        INC  IX

CNV1:   INC  IX         ;Hent naeste karakter
        LD   A,(IX+0)

        CP   '.'        ;Decimalpunkt?
        JR   NZ,CNV2    ;Nej => CNV2
        EXX             ;Er det det foerste?
        BIT  6,B
        SCF
        JP   NZ,CNV6    ;Nej => FEJL
        SET  6,B        ;Ja => saet flag
        EXX
        JR   CNV1

CNV2:   CP   'E'        ;Exponentnotation?
        JR   Z,CNV4     ;Ja => CNV4

        CALL DIGTST     ;Er det et ciffer?
        JR   NC,CNV5    ;Nej => CNV5

        EX   AF,AF'     ;Gang resultat med 10
        CALL MUL10
        JR   C,CNV6A
        EX   AF,AF'
        EXX             ;Laeg det nye ciffer
        PUSH BC         ;til
        LD   L,A
        LD   H,0
        CALL FLOAT
        CALL FPADD
        EXX
        POP  BC
        JR   C,CNV6A
        BIT  6,B        ;Er decimalflag sat?
        JR   Z,CNV3     ;Nej => CNV3
        DEC  C          ;Traek 1 fra tiexp.
CNV3:   EXX
        JR   CNV1

CNV4:   CALL MFACT      ;Gang med titalsfaktor
        JR   C,CNV6     ;Overflow => CNV6
        EXX
        INC  IX         ;Hent naeste karakter
        LD   A,(IX+0)
        CP   '+'        ;Plus?
        JR   Z,CNV4A    ;Ja => CNV4A
        CP   '-'        ;Minus?
        JR   NZ,CNV4B   ;Nej => CNV4B
        SET  5,B        ;Saet minusflag
CNV4A:  INC  IX

CNV4B:  CALL GDTST      ;Er der et ciffer?
        CCF             ;Saet carry hvis ikke
CNV6A:  JR   C,CNV6     ;Nej => CNV6
        LD   C,A        ;Gem i C
        INC  IX         ;Er der et mere?
        CALL GDTST
        JR   NC,CNV4C   ;Nej => CNV4C

        INC  IX         ;Gang forrige ciffer
        LD   D,A        ;med 10 og laeg det
        LD   A,C        ;nye til
        ADD  A,A
        ADD  A,A
        ADD  A,C
        ADD  A,A
        ADD  A,D
        LD   C,A

CNV4C:  BIT  5,B        ;Negativt?
        JR   Z,CNV4D    ;Nej => CNV4D
        LD   A,C
        NEG
        LD   C,A
CNV4D:  EXX

CNV5:   CALL MFACT      ;Gang med titalsfaktor
        JR   C,CNV6     ;Overflow => CNV6
        EXX             ;Negativt?
        BIT  7,B
        EXX
        JR   Z,CNV6     ;Nej => CNV6
        SET  7,B        ;Saet minusflag i AC

CNV6:   JP   ADD10      ;Hent AC'

;Gang tallet i AC med 10^C'

MFACT:  EXX
        LD   A,C
        ADD  A,EXPN
        CP   -37+EXPN   ;Tiexp.<-37?
        RET  C          ;Ja => Retur
        CP   38+EXPN    ;Tiexp.>37?
        CCF
        RET  C          ;Ja => Retur

        PUSH BC         ;Gem BC
        LD   A,C        ;Hent tiexponent
        CALL TENF       ;Mul. med 10^tiexp.
        EXX             ;Hent BC
        POP  BC
        EXX
        RET

;Saet carry hvis karakteren i A er et ciffer.

GDTST:  LD   A,(IX+0)
DIGTST: SUB  '0'
        CCF
        RET  NC
        CP   10
        RET

;Saet AC lig 1.

AC1:    LD   BC,00000H
        LD   DE,00000H
        LD   HL,00081H
        RET
;------------END OF MATH48------------
