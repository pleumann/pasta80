; Converted to z80 compiler sjasm+ by Shawn Sijnstra 08-Nov-2025
;
;
; Table 12. CPU Code Suffix to Assembled Prefix Mapping
; CPU Code Suffix   Assembled Prefix Byte (hex)
; .SIS					40
; .LIS					49
	MACRO	mklis
	db		49h		;add .LIS to next instruction
	ENDM
; .SIL					52
; .LIL					5B
	MACRO	mklil
	db		5Bh		;add .LIS to next instruction
	ENDM

; LD A,MB	;Segment base
	MACRO	ldamb
	db		0edh			;not supported by sjasm+ so just put in the bytes
	db		06eh
	ENDM

; LD		(IX+0), BC		; can't prefix this because it isn't a valid z80 instruction so compiles incorrectly.
	MACRO	ldixbc	off
	db		05Bh, 0DDh, 00Fh, off
	ENDM
; LD		(IX+0), DE		; can't prefix this because it isn't a valid z80 instruction so compiles incorrectly.
	MACRO	ldixde	off
	db		05Bh, 0DDh, 01Fh, off
	ENDM
; LD		HL, (IX+0)		; can't prefix this because it isn't a valid z80 instruction so compiles incorrectly.
	MACRO	ldhlix	off
	db		05Bh, 0DDh, 027h, off
	ENDM
;  LD HL,(HL) 0/1 4/5 ED, 27
	MACRO	ldhl_hl_
	db		0EDh, 027h
	ENDM

argv_ptrs_max:		EQU	16			; Maximum number of arguments allowed in argv
			
;
; Start in mixed mode. Assumes MBASE is set to correct segment
;
;already present			JP		__init		; Jump to start
			DS		5

RST_08:
			mklis
			RST	08h		; MOS API call
			RET
			DS 		5
			
RST_10:
			mklis
			RST 	10h		; Display character
			RET
			DS		5
			
RST_18:
			mklis
			RST 	18h		; Display string
			RET
			DS		5
RST_20:			DS		8
RST_28:			DS		8
RST_30:			DS		8	
;	
;
;
RST_38:
			mklis
			RST 	38h		; Guru meditation
			RET
;
; The header stuff is from byte 64 onwards
;
			ALIGN	64
			
			DB	"MOS"				; Flag for MOS - to confirm this is a valid MOS command
			DB	00h				; MOS header version 0
			DB	00h				; Flag for run mode (0: Z80, 1: ADL)

_exec_name:
			DB	"MadeWithPasta.BIN", 0		; The executable name, only used in argv - TODO

;
; And the code follows on immediately after the header
;
__init:	
			mklil
			PUSH	IY			; Preserve IY

			LD		IY, 0			; Preserve SPS
			ADD		IY, SP

			mklil
			PUSH	IY
			LD		SP, 0FFFEh		; And set to 0000h, top of the RAM in z80 space
	
			PUSH		AF			; Preserve the rest of the registers
			mklil
			PUSH	BC
			mklil
			PUSH	DE
			mklil
			PUSH	IX

			ldamb	;LD		A, MB			; Segment base

			LD		IX, argv_ptrs		; The argv array pointer address
			CALL		_set_aix24		; Convert to a 24-bit address	
;			mklil		
;			PUSH	IX
			CALL		_parse_params		; Parse the parameters
			LD		B, 0			;  Now BC: argc
			dec		BC				;Pascal does NOT count the filename as a parameter.
			ld		(__parmcount),BC
			call	al_setcoords	;uses AF and BC

;			mklil
;			POP		IX			; IX: argv - don't need to preserve it now.

			CALL		main			; Start user code

			mklil
			POP		IX			; Restore the registers
			mklil
			POP		DE
			mklil
			POP		BC
			POP		AF

			mklil
			POP		IY			; Get the preserved SPS
			LD		SP, IY			; Restore the SP

			mklil			
			POP		IY			; Restore IY
			mklis				;RET.L encodes to RET.LIS
			RET					; Return to MOS
			
; Parse the parameter string into a C array
; Parameters
; -   A: Segment base
; - HLU: Address of parameter string
; - IXU: Address for array pointer storage
; Returns:
; -   C: Number of parameters parsed
;
_parse_params:
			LD		BC, _exec_name		; Get the address of the app name in this segment			
			CALL		_set_abc24		; Convert it to a 24-bit address based upon segment base
;			LD		(IX+0), BC		; ARGV[0] = the executable name
			ldixbc	0
			mklil
			INC		IX
			mklil
			INC		IX
			mklil
			INC		IX
;test:		jp		test
			CALL		_skip_spaces		; Skip HL past any leading spaces
;
			LD		BC, 1			; C: ARGC = 1 - also clears out top 16 bits of BCU
			LD		B, argv_ptrs_max - 1	; B: Maximum number of argv_ptrs
;
_parse_params_1:	PUSH		BC			; Stack ARGC
			mklil	
			PUSH	HL			; Stack start address of token
			CALL		_get_token		; Get the next token
			LD		A, C			; A: Length of the token in characters
			mklil
			POP		DE			; Start address of token (was in HL)
			POP		BC			; ARGC
			OR		A			; Check for A=0 (no token found) OR at end of string
			RET		Z
;			LD		(IX+0), DE		; Store the pointer to the token
			ldixde	0
			mklil
			PUSH	HL			; DE=HL
			mklil
			POP		DE
			CALL		_skip_spaces		; And skip HL past any spaces onto the next character
			XOR		A
			mklil
			LD		(DE), A			; Zero-terminate the token
			mklil
			INC		IX
			mklil
			INC		IX
			mklil
			INC		IX			; Advance to next pointer position
			INC		C			; Increment ARGC
			LD		A, C			; Check for C >= A
			CP		B
			JR		C, _parse_params_1	; And loop
			RET

; Get the next token
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of first character after token
; -  C: Length of token (in characters)
;
_get_token:		LD		C, 0			; Initialise length
_get_token_lp:
			mklil
			LD		A, (HL)			; Get the character from the parameter string
			OR		A			; Exit if 0 (end of parameter string in MOS)
			RET 		Z
			CP		13			; Exit if CR (end of parameter string in BBC BASIC)
			RET		Z
			CP		' '			; Exit if space (end of token)
			RET		Z
			mklil
			INC		HL			; Advance to next character
			INC 		C			; Increment length
			JR		_get_token_lp
	
; Skip spaces in the parameter string
; Parameters:
; - HL: Address of parameter string
; Returns:
; - HL: Address of next none-space character
;    F: Z if at end of string, otherwise NZ if there are more tokens to be parsed
;
_skip_spaces:
			mklil
			LD		A, (HL)			; Get the character from the parameter string	
			CP		' '			; Exit if not space
			RET		NZ
			mklil
			INC		HL			; Advance to next character
			JR		_skip_spaces		; Increment length
			
; Set the MSB of BC (U) to A
; Parameters:
; - BC: 16-bit address
; -  A: Value to stick in U of BC
; Returns:
; - BCU
;
_set_abc24:
			mklil
			PUSH	HL			; Preserve HL
			mklil
			PUSH	BC			; Stick BC onto SPL
			mklil
			LD		HL, 2			; HL: SP+2
			db		0				; need bits 16-23.
			mklil
			ADD		HL, SP
			mklil
			LD		(HL), A			; Store A in it
			mklil
			POP		BC			; Fetch ammended BC
			mklil
			POP		HL			; Restore HL
			RET


; Set the MSB of IX (U) to A
; Parameters:
; - IX: 16-bit address
; -  A: Value to stick in U of BC
; Returns:
; - IXU
;
_set_aix24:
			mklil
			PUSH	IX			; Stick IX onto SPL
			mklil
			LD		IX, 2			; IX: SP+2
			db		0				; need bits 16-23.
			mklil
			ADD		IX, SP
			mklil
			LD		(IX), A			; Store A in it
			mklil
			POP		IX			; Fetch ammended IX
			RET
			
; Storage for the argv array pointers
; THERE SEEMS TO BE A BUG WITH SJASM+ CALCULATING THE SIZE OF THE POINTERS ARRAY
argv_ptrs:		block	argv_ptrs_max,0	  		; Storage for the argv array pointers
				block	argv_ptrs_max,0
				block	argv_ptrs_max,0
__parmcount:	block	2,0						; Storage for the parameter count

; VDP control (VDU 23, 0, n)
;
vdp_gp:			EQU 	80h
vdp_keycode:		EQU 	81h
vdp_cursor:		EQU	82h
vdp_scrchar:		EQU	83h
vdp_scrpixel:		EQU	84h
vdp_audio:		EQU	85h
vdp_mode:		EQU	86h
vdp_rtc:		EQU	87h
vdp_keystate:		EQU	88h
vdp_logicalcoords:	EQU	0C0h
vdp_terminalmode:	EQU	0FFh

; MOS high level functions
;
mos_getkey:		EQU	00h
mos_load:		EQU	01h
mos_save:		EQU	02h
mos_cd:			EQU	03h
mos_dir:		EQU	04h
mos_del:		EQU	05h
mos_ren:		EQU	06h
mos_mkdir:		EQU	07h
mos_sysvars:		EQU	08h
mos_editline:		EQU	09h
mos_fopen:		EQU	0Ah
mos_fclose:		EQU	0Bh
mos_fgetc:		EQU	0Ch
mos_fputc:		EQU	0Dh
mos_feof:		EQU	0Eh
mos_getError:		EQU	0Fh
mos_oscli:		EQU	10h
mos_copy:		EQU	11h
mos_getrtc:		EQU	12h
mos_setrtc:		EQU	13h
mos_setintvector:	EQU	14h
mos_uopen:		EQU	15h
mos_uclose:		EQU	16h
mos_ugetc:		EQU	17h
mos_uputc:		EQU 	18h
mos_getfil:		EQU	19h
mos_fread:		EQU	1Ah
mos_fwrite:		EQU	1Bh
mos_flseek:		EQU	1Ch

; FatFS file access functions
;
ffs_fopen:		EQU	80h
ffs_fclose:		EQU	81h
ffs_fread:		EQU	82h
ffs_fwrite:		EQU	83h
ffs_flseek:		EQU	84h
ffs_ftruncate:		EQU	85h
ffs_fsync:		EQU	86h
ffs_fforward:		EQU	87h
ffs_fexpand:		EQU	88h
ffs_fgets:		EQU	89h
ffs_fputc:		EQU	8Ah
ffs_fputs:		EQU	8Bh
ffs_fprintf:		EQU	8Ch
ffs_ftell:		EQU	8Dh
ffs_feof:		EQU	8Eh
ffs_fsize:		EQU	8Fh
ffs_ferror:		EQU	90h

; FatFS directory access functions
;
ffs_dopen:		EQU	91h
ffs_dclose:		EQU	92h
ffs_dread:		EQU	93h
ffs_dfindfirst:		EQU	94h
ffs_dfindnext:		EQU	95h

; FatFS file and directory management functions
;
ffs_stat:		EQU	96h
ffs_unlink:		EQU	97h
ffs_rename:		EQU	98h
ffs_chmod:		EQU	99h
ffs_utime:		EQU	9Ah
ffs_mkdir:		EQU	9Bh
ffs_chdir:		EQU	9Ch
ffs_chdrive:		EQU	9Dh
ffs_getcwd:		EQU	9Eh

; FatFS volume management and system configuration functions
;
ffs_mount:		EQU	9Fh
ffs_mkfs:		EQU	0A0h
ffs_fdisk		EQU	0A1h
ffs_getfree:		EQU	0A2h
ffs_getlabel:		EQU	0A3h
ffs_setlabel:		EQU	0A4h
ffs_setcp:		EQU	0A5h
	
; File access modes
;
fa_read:		EQU	01h
fa_write:		EQU	02h
fa_open_existing:	EQU	00h
fa_create_new:		EQU	04h
fa_create_always:	EQU	08h
fa_open_always:		EQU	10h	;open if exists, create if not.
fa_open_append:		EQU	30h
