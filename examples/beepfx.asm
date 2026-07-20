;	org 60000

;BeepFX player by Shiru
;You are free to do whatever you want with this code



playBasic:
	ld a,0
play:
    ld de,sfxData
    add hl,hl
	add hl,de

;    ld hl,sfxData	;address of sound effects data

	di
	push ix
	push iy

	ld b,0
	ld c,a
	add hl,bc
	add hl,bc
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	pop ix			;put it into ix

	ld a,(23624)	;get border color from BASIC vars to keep it unchanged
	rra
	rra
	rra
	and 7
	ld (sfxRoutineToneBorder  +1),a
	ld (sfxRoutineNoiseBorder +1),a
	ld (sfxRoutineSampleBorder+1),a


readData:
	ld a,(ix+0)		;read block type
	ld c,(ix+1)		;read duration 1
	ld b,(ix+2)
	ld e,(ix+3)		;read duration 2
	ld d,(ix+4)
	push de
	pop iy

	dec a
	jr z,sfxRoutineTone
	dec a
	jr z,sfxRoutineNoise
	dec a
	jr z,sfxRoutineSample
	pop iy
	pop ix
	ei
	ret

	

;play sample

sfxRoutineSample:
	ex de,hl
sfxRS0:
	ld e,8
	ld d,(hl)
	inc hl
sfxRS1:
	ld a,(ix+5)
sfxRS2:
	dec a
	jr nz,sfxRS2
	rl d
	sbc a,a
	and 16
sfxRoutineSampleBorder:
	or 0
	out (254),a
	dec e
	jr nz,sfxRS1
	dec bc
	ld a,b
	or c
	jr nz,sfxRS0

	ld c,6
	
nextData:
	add ix,bc		;skip to the next block
	jr readData



;generate tone with many parameters

sfxRoutineTone:
	ld e,(ix+5)		;freq
	ld d,(ix+6)
	ld a,(ix+9)		;duty
	ld (sfxRoutineToneDuty+1),a
	ld hl,0

sfxRT0:
	push bc
	push iy
	pop bc
sfxRT1:
	add hl,de
	ld a,h
sfxRoutineToneDuty:
	cp 0
	sbc a,a
	and 16
sfxRoutineToneBorder:
	or 0
	out (254),a

	dec bc
	ld a,b
	or c
	jr nz,sfxRT1

	ld a,(sfxRoutineToneDuty+1)	 ;duty change
	add a,(ix+10)
	ld (sfxRoutineToneDuty+1),a

	ld c,(ix+7)		;slide
	ld b,(ix+8)
	ex de,hl
	add hl,bc
	ex de,hl

	pop bc
	dec bc
	ld a,b
	or c
	jr nz,sfxRT0

	ld c,11
	jr nextData



;generate noise with two parameters

sfxRoutineNoise:
	ld e,(ix+5)		;pitch

	ld d,1
	ld h,d
	ld l,d
sfxRN0:
	push bc
	push iy
	pop bc
sfxRN1:
	ld a,(hl)
	and 16
sfxRoutineNoiseBorder:
	or 0
	out (254),a
	dec d
	jr nz,sfxRN2
	ld d,e
	inc hl
	ld a,h
	and 31
	ld h,a
sfxRN2:
	dec bc
	ld a,b
	or c
	jr nz,sfxRN1

	ld a,e
	add a,(ix+6)	;slide
	ld e,a

	pop bc
	dec bc
	ld a,b
	or c
	jr nz,sfxRN0

	ld c,7
	jr nextData


sfxData:

SoundEffectsData:
	defw SoundEffect0Data
	defw SoundEffect1Data
	defw SoundEffect2Data
	defw SoundEffect3Data
	defw SoundEffect4Data
	defw SoundEffect5Data
	defw SoundEffect6Data
	defw SoundEffect7Data
	defw SoundEffect8Data
	defw SoundEffect9Data
	defw SoundEffect10Data
	defw SoundEffect11Data
	defw SoundEffect12Data
	defw SoundEffect13Data
	defw SoundEffect14Data
	defw SoundEffect15Data
	defw SoundEffect16Data
	defw SoundEffect17Data
	defw SoundEffect18Data
	defw SoundEffect19Data
	defw SoundEffect20Data
	defw SoundEffect21Data
	defw SoundEffect22Data
	defw SoundEffect23Data
	defw SoundEffect24Data
	defw SoundEffect25Data
	defw SoundEffect26Data
	defw SoundEffect27Data
	defw SoundEffect28Data
	defw SoundEffect29Data
	defw SoundEffect30Data
	defw SoundEffect31Data
	defw SoundEffect32Data
	defw SoundEffect33Data
	defw SoundEffect34Data
	defw SoundEffect35Data
	defw SoundEffect36Data
	defw SoundEffect37Data
	defw SoundEffect38Data
	defw SoundEffect39Data
	defw SoundEffect40Data
	defw SoundEffect41Data
	defw SoundEffect42Data
	defw SoundEffect43Data
	defw SoundEffect44Data
	defw SoundEffect45Data
	defw SoundEffect46Data
	defw SoundEffect47Data
	defw SoundEffect48Data
	defw SoundEffect49Data
	defw SoundEffect50Data
	defw SoundEffect51Data
	defw SoundEffect52Data
	defw SoundEffect53Data
	defw SoundEffect54Data
	defw SoundEffect55Data
	defw SoundEffect56Data
	defw SoundEffect57Data
	defw SoundEffect58Data

SoundEffect0Data:
	defb 1 ;tone
	defw 20,50,2000,65486,128
	defb 0
SoundEffect1Data:
	defb 2 ;noise
	defw 20,50,257
	defb 0
SoundEffect2Data:
	defb 1 ;tone
	defw 100,20,500,2,128
	defb 0
SoundEffect3Data:
	defb 1 ;tone
	defw 100,20,500,2,16
	defb 0
SoundEffect4Data:
	defb 1 ;tone
	defw 10,100,2000,100,128
	defb 0
SoundEffect5Data:
	defb 1 ;tone
	defw 50,100,200,65531,128
	defb 0
SoundEffect6Data:
	defb 2 ;noise
	defw 100,50,356
	defb 0
SoundEffect7Data:
	defb 2 ;noise
	defw 1,1000,10
	defb 2 ;noise
	defw 1,1000,1
	defb 0
SoundEffect8Data:
	defb 2 ;noise
	defw 1,1000,20
	defb 1 ;pause
	defw 1,1000,0,0,0
	defb 2 ;noise
	defw 1,1000,1
	defb 0
SoundEffect9Data:
	defb 1 ;tone
	defw 20,1000,200,0,63104
	defb 0
SoundEffect10Data:
	defb 1 ;tone
	defw 400,50,200,0,63104
	defb 0
SoundEffect11Data:
	defb 1 ;tone
	defw 2000,10,400,0,63104
	defb 0
SoundEffect12Data:
	defb 1 ;tone
	defw 100,100,1000,0,32896
	defb 0
SoundEffect13Data:
	defb 1 ;tone
	defw 1000,10,100,0,25728
	defb 0
SoundEffect14Data:
	defb 1 ;tone
	defw 100,100,1000,0,32640
	defb 0
SoundEffect15Data:
	defb 1 ;tone
	defw 100,20,400,1,25728
	defb 0
SoundEffect16Data:
	defb 2 ;noise
	defw 2,2000,32776
	defb 0
SoundEffect17Data:
	defb 2 ;noise
	defw 1,1000,10
	defb 1 ;tone
	defw 20,100,400,65526,128
	defb 2 ;noise
	defw 1,2000,1
	defb 0
SoundEffect18Data:
	defb 1 ;tone
	defw 100,20,1000,65535,2176
	defb 0
SoundEffect19Data:
	defb 2 ;noise
	defw 20,2000,1290
	defb 0
SoundEffect20Data:
	defb 2 ;noise
	defw 100,400,2562
	defb 0
SoundEffect21Data:
	defb 2 ;noise
	defw 5,1000,5124
	defb 1 ;tone
	defw 50,100,200,65534,128
	defb 0
SoundEffect22Data:
	defb 2 ;noise
	defw 8,200,20
	defb 2 ;noise
	defw 4,2000,5220
	defb 0
SoundEffect23Data:
	defb 2 ;noise
	defw 25,2500,28288
	defb 0
SoundEffect24Data:
	defb 2 ;noise
	defw 100,40,20
	defb 1 ;tone
	defw 100,40,400,65532,128
	defb 2 ;noise
	defw 100,40,40
	defb 1 ;tone
	defw 100,40,350,65532,128
	defb 2 ;noise
	defw 100,40,80
	defb 1 ;tone
	defw 100,40,320,65532,128
	defb 2 ;noise
	defw 100,40,100
	defb 1 ;tone
	defw 100,40,310,65532,128
	defb 2 ;noise
	defw 100,40,120
	defb 1 ;tone
	defw 100,40,300,65532,128
	defb 0
SoundEffect25Data:
	defb 2 ;noise
	defw 5,1000,5130
	defb 1 ;tone
	defw 20,100,200,65526,128
	defb 2 ;noise
	defw 1,10000,200
	defb 0
SoundEffect26Data:
	defb 1 ;tone
	defw 8,400,300,65511,128
	defb 2 ;noise
	defw 6,5000,5270
	defb 0
SoundEffect27Data:
	defb 2 ;noise
	defw 1,1000,4
	defb 1 ;tone
	defw 4,1000,400,65436,128
	defb 2 ;noise
	defw 1,5000,150
	defb 0
SoundEffect28Data:
	defb 1 ;tone
	defw 10,400,1000,65136,2688
	defb 0
SoundEffect29Data:
	defb 1 ;tone
	defw 10,400,1000,65336,2688
	defb 0
SoundEffect30Data:
	defb 1 ;tone
	defw 4,1000,1000,400,128
	defb 0
SoundEffect31Data:
	defb 1 ;tone
	defw 4,1000,1000,65136,128
	defb 0
SoundEffect32Data:
	defb 1 ;tone
	defw 1,1000,1000,0,128
	defb 1 ;pause
	defw 1,1000,0,0,0
	defb 1 ;tone
	defw 1,2000,2000,0,128
	defb 1 ;tone
	defw 1,2000,2000,0,16
	defb 0
SoundEffect33Data:
	defb 1 ;tone
	defw 1,1000,2000,0,64
	defb 1 ;pause
	defw 1,1000,0,0,0
	defb 1 ;tone
	defw 1,1000,1500,0,64
	defb 0
SoundEffect34Data:
	defb 2 ;noise
	defw 1,1000,4
	defb 1 ;tone
	defw 1,1000,2000,0,128
	defb 0
SoundEffect35Data:
	defb 2 ;noise
	defw 1,1000,8
	defb 1 ;tone
	defw 1,1000,800,0,128
	defb 2 ;noise
	defw 1,1000,16
	defb 1 ;tone
	defw 1,1000,700,0,128
	defb 0
SoundEffect36Data:
	defb 1 ;tone
	defw 10,400,400,65516,128
	defb 1 ;pause
	defw 10,400,0,0,0
	defb 1 ;tone
	defw 10,400,350,65516,96
	defb 1 ;pause
	defw 10,400,0,0,0
	defb 1 ;tone
	defw 10,400,300,65516,64
	defb 1 ;pause
	defw 10,400,0,0,0
	defb 1 ;tone
	defw 10,400,250,65516,32
	defb 1 ;pause
	defw 10,400,0,0,0
	defb 1 ;tone
	defw 10,400,200,65516,16
	defb 0
SoundEffect37Data:
	defb 1 ;tone
	defw 5,1800,1000,1000,65408
	defb 0
SoundEffect38Data:
	defb 1 ;tone
	defw 3500,10,2,0,25728
	defb 0
SoundEffect39Data:
	defb 1 ;tone
	defw 20,200,3400,10,64
	defb 0
SoundEffect40Data:
	defb 1 ;tone
	defw 1,2000,400,0,128
	defb 1 ;tone
	defw 1,2000,400,0,16
	defb 1 ;tone
	defw 1,2000,600,0,128
	defb 1 ;tone
	defw 1,2000,600,0,16
	defb 1 ;tone
	defw 1,2000,800,0,128
	defb 1 ;tone
	defw 1,2000,800,0,16
	defb 0
SoundEffect41Data:
	defb 1 ;tone
	defw 1,2000,400,0,128
	defb 1 ;tone
	defw 1,2000,600,0,128
	defb 1 ;tone
	defw 1,2000,800,0,128
	defb 1 ;tone
	defw 1,2000,400,0,16
	defb 1 ;tone
	defw 1,2000,600,0,16
	defb 1 ;tone
	defw 1,2000,800,0,16
	defb 0
SoundEffect42Data:
	defb 1 ;tone
	defw 4,1000,500,100,384
	defb 1 ;tone
	defw 4,1000,500,100,64
	defb 1 ;tone
	defw 4,1000,500,100,16
	defb 0
SoundEffect43Data:
	defb 1 ;tone
	defw 5,2000,200,100,128
	defb 0
SoundEffect44Data:
	defb 1 ;tone
	defw 4,1000,400,100,128
	defb 1 ;tone
	defw 4,1000,400,100,64
	defb 1 ;tone
	defw 4,1000,400,100,32
	defb 1 ;tone
	defw 4,1000,400,100,16
	defb 0
SoundEffect45Data:
	defb 1 ;tone
	defw 4,2000,600,65436,61504
	defb 1 ;tone
	defw 4,2000,600,65436,8
	defb 1 ;tone
	defw 4,2000,600,65436,4
	defb 0
SoundEffect46Data:
	defb 1 ;tone
	defw 2,4000,400,200,64
	defb 1 ;tone
	defw 2,4000,200,200,32
	defb 0
SoundEffect47Data:
	defb 1 ;tone
	defw 2,1000,400,100,64
	defb 1 ;tone
	defw 2,1000,400,100,64
	defb 1 ;tone
	defw 2,1000,400,100,64
	defb 1 ;tone
	defw 2,1000,400,100,64
	defb 0
SoundEffect48Data:
	defb 1 ;tone
	defw 32,1000,2000,16384,320
	defb 0
SoundEffect49Data:
	defb 1 ;tone
	defw 200,20,400,0,384
	defb 1 ;tone
	defw 200,20,800,0,384
	defb 1 ;tone
	defw 200,20,400,0,384
	defb 1 ;tone
	defw 200,20,800,0,384
	defb 1 ;tone
	defw 200,20,400,0,384
	defb 1 ;tone
	defw 200,20,800,0,384
	defb 0
SoundEffect50Data:
	defb 1 ;tone
	defw 20,100,200,10,1025
	defb 1 ;pause
	defw 30,100,0,0,0
	defb 1 ;tone
	defw 50,100,200,10,1025
	defb 0
SoundEffect51Data:
	defb 1 ;tone
	defw 50,200,500,65516,128
	defb 0
SoundEffect52Data:
	defb 1 ;tone
	defw 1,2000,200,0,128
	defb 1 ;pause
	defw 1,2000,0,0,0
	defb 1 ;tone
	defw 1,2000,200,0,32
	defb 1 ;pause
	defw 1,2000,0,0,0
	defb 1 ;tone
	defw 1,2000,200,0,16
	defb 1 ;pause
	defw 1,2000,0,0,0
	defb 1 ;tone
	defw 1,2000,200,0,8
	defb 0
SoundEffect53Data:
	defb 1 ;tone
	defw 10,1000,200,2,272
	defb 1 ;pause
	defw 1,4000,0,0,0
	defb 1 ;tone
	defw 10,1000,200,65534,272
	defb 0
SoundEffect54Data:
	defb 1 ;tone
	defw 20,500,200,5,272
	defb 1 ;pause
	defw 1,1000,0,0,0
	defb 1 ;tone
	defw 30,500,200,8,272
	defb 0
SoundEffect55Data:
	defb 1 ;tone
	defw 40,2125,16384,45459,128
	defb 0
SoundEffect56Data:
	defb 3 ;sample
	defw 262
	defw Sample0Data+0
	defb 45
	defb 0
SoundEffect57Data:
	defb 3 ;sample
	defw 160
	defw Sample1Data+0
	defb 45
	defb 0
SoundEffect58Data:
	defb 3 ;sample
	defw 262
	defw Sample0Data+0
	defb 100
	defb 0

Sample0Data:
	defb 127,255,255,163,255,255,129,255,255,213,31,255,168,31,255,240
	defb 63,255,232,15,255,171,253,71,254,128,127,224,127,128,255,208
	defb 127,245,127,129,255,240,255,234,255,15,254,175,254,191,250,255
	defb 245,127,250,255,111,255,249,255,175,250,253,250,151,253,127,135
	defb 255,168,175,215,252,63,253,87,254,191,171,255,250,215,237,250
	defb 255,255,87,254,175,255,255,245,127,235,250,255,191,171,127,95
	defb 255,191,250,190,247,255,255,255,244,191,255,255,95,255,173,253
	defb 127,255,255,235,171,251,255,255,255,87,223,254,254,255,245,117
	defb 191,191,239,255,87,91,251,255,255,245,117,181,191,255,255,95
	defb 95,235,255,191,250,191,255,223,121,249,213,242,250,223,215,206
	defb 174,191,214,254,63,117,94,254,215,250,251,215,251,123,127,249
	defb 255,175,183,246,223,233,127,175,167,243,223,227,239,215,219,233
	defb 239,248,113,245,244,250,121,248,120,125,124,126,124,62,62,31
	defb 151,207,143,143,207,7,252,158,124,60,126,60,127,248,120,240
	defb 241,241,225,243,243,225,243,231,243,231,199,249,178,253,211,240
	defb 227,227,239,14,30,60,124,120,241,255,227,227,199,143,143,31
	defb 63,255,159,62,124,254

Sample1Data:
	defb 203,203,247,227,248,120,223,60,127,143,56,247,15,113,227,159
	defb 225,238,60,115,248,127,207,252,255,15,225,243,223,195,252,63
	defb 107,248,127,135,244,63,7,248,63,195,240,31,135,252,63,0
	defb 252,63,225,241,135,195,255,31,30,126,63,249,241,243,227,247
	defb 143,191,254,31,239,255,255,247,248,127,249,249,255,5,255,191
	defb 191,199,223,143,239,240,7,254,3,252,3,255,192,254,1,255
	defb 128,255,128,255,193,255,128,255,131,255,1,255,63,240,15,248
	defb 127,224,63,225,255,128,127,227,255,0,255,227,255,0,255,225
	defb 255,224,63,252,31,252,1,255,228,255,192,31,255,231,255,128
	defb 127,255,240,127,224,31,255,255,255,255,192,63,255,255,255,255
