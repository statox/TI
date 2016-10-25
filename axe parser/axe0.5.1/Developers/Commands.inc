;#################################
;#    To Assembly Programmers    #
;#############################################################################################
;# This is a complete list of all the routiens used when compiling programs from Axe Parser. #
;# Almost all of them are hand written by me and are highly optimized and sometimes hard to  #
;# follow, but if you see any optimizations for smaller size and faster code, please contact #
;# me so I can alter the routines to improve newer versions.  Conversely, everyone is free   #
;# to use these routines in their own programs and you don't need to credit me unless you    #
;# want to.                                                                                  #
;#############################################################################################

;Headers
;-----------------------------------------------
p_AsmHeader:
	.db 2
	.dw $6DBB

p_IonHeader:
	.db 5
	.dw $6DBB
	xor	a
	jr	nc,$+0
	.db rp_Byte,1

p_MOSHeader:
	.db 34
	.dw $6DBB
	ret
	.db 1
	.db %00000001,%00000000
	.db %00011010,%10110000
	.db %00111110,%11111000
	.db %00111111,%11111000
	.db %00111111,%11111000
	.db %00111110,%11111000
	.db %00011010,%10110000
	.db %00001010,%10100000
	.db %00000010,%10000000
	.db %11111110,%11111110
	.db %11001101,%01000010
	.db %10110101,%01011110
	.db %10000110,%11000110
	.db %10110101,%01011110
	.db %10110101,%01000010

p_DCSHeader:
	.db 46
	.dw $6DBB
	xor d
	ret
	jr $+0
	.dw $9DC1
	.db $05,$00
	.dw $9DA1
	.dw $0000
	.db %00000001,%10000000
	.db %00011010,%01011000
	.db %00111110,%01111100
	.db %00111111,%11111100
	.db %00111111,%11111100
	.db %00111110,%01111100
	.db %00011010,%01011000
	.db %00001010,%01010000
	.db %00000010,%01000000
	.db %11111110,%01111111
	.db %11001101,%10100001
	.db %10110101,%10101111
	.db %10000110,%01100011
	.db %10110101,%10101111
	.db %10110101,%10100001
	.db %11111111,%11111111
	.db rp_Byte,41

p_APPHeader1:
	.db 18
	.db $80,$0F		;Field: Program length
	.db $00,$00,$00,$00	;Length=0 (N/A for unsigned apps)
	.db $80,$12		;Field: Program type
	.db $01,$04		;Type= Shareware, TI-83Plus
	.db $80,$21		;Field: App ID
	.db $01			;Id = 1
	.db $80,$31		;Field: App Build
	.db $01			;Build = 1
	.db $80,$48		;Field: App Name
;	.db "Name    "		;Name must be 8 characters
p_APPHeader2:
	.db p_APPHeaderEnd-p_APPHeader2-1
	.db $80,$81		;Field: App Pages
	.db $01			;App Pages = 1
	.db $80,$90		;No default splash screen
	.db $03,$26,$09,$04,$04,$6f,$1b,$80	;Field: Date stamp- 5/12/1999
	.db $02,$0d,$40		;Dummy encrypted TI date stamp signature
	.db $a1,$6b,$99,$f6,$59,$bc,$67
	.db $f5,$85,$9c,$09,$6c,$0f,$b4,$03,$9b,$c9
	.db $03,$32,$2c,$e0,$03,$20,$e3,$2c,$f4,$2d
	.db $73,$b4,$27,$c4,$a0,$72,$54,$b9,$ea,$7c
	.db $3b,$aa,$16,$f6,$77,$83,$7a,$ee,$1a,$d4
	.db $42,$4c,$6b,$8b,$13,$1f,$bb,$93,$8b,$fc
	.db $19,$1c,$3c,$ec,$4d,$e5,$75
	.db $80,$7F		;Field: Program Image length
	.db 0,0,0,0		;Length=0, N/A
	.db 0,0,0,0		;Reserved
	.db 0,0,0,0		;Reserved
	.db 0,0,0,0		;Reserved
	.db 0,0,0,0		;Reserved
	call $4000+p_APPHeaderEnd-p_APPHeader1+8-2
	B_CALL(_ReloadAppEntry)
	B_JUMP(_JForceCmdNoChar)
p_APPHeaderEnd:

;Saving and loading numbers
;-----------------------------------------------
p_LoadAnsHL:
	.db 3
	ld	hl,$0000
	.db rp_Ans,2
p_LoadAnsDE:
	.db 3
	ld	de,$0000
	.db rp_Ans,2
p_LoadAnsBC:
	.db 3
	ld	bc,$0000
	.db rp_Ans,2
p_LoadAnsIX:
	.db 4
	ld	ix,$0000
	.db rp_Ans,2

p_LoadAns_HL_:
	.db 3
	ld	hl,($0000)
	.db rp_Ans,2
p_LoadAns_DE_:
	.db 4
	ld	de,($0000)
	.db rp_Ans,2
p_LoadAns_BC_:
	.db 4
	ld	bc,($0000)
	.db rp_Ans,2
p_LoadAns_A_:
	.db 3
	ld	a,($0000)
	.db rp_Ans,2
p_LoadAnsTiny:
	.db 5
	ld	hl,($0000)
	ld	h,0
	.db rp_Ans,4

p_SaveAns_HL_:
	.db 3
	ld	($0000),hl
	.db rp_Ans,2
p_SaveAns_DE_:
	.db 4
	ld	($0000),de
	.db rp_Ans,2
p_SaveAnsTiny:
	.db 4
	ld	a,l
	ld	($0000),a
	.db rp_Ans,2

p_PushHL:
	.db 1
	push	hl
p_PopDE:
	.db 1
	pop	de
p_PopBC:
	.db 1
	pop	bc
p_ExHLDE:
	.db 1
	ex	de,hl
p_LdHLCA:
	.db 2
	ld	h,c
	ld	l,a
p_LdPtr:
	.db 3
	ld	l,(hl)
	ld	h,0
p_LdPtr2:
	.db 4
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a
p_LdPtrRev:
	.db 4
	ld	a,(hl)
	inc	hl
	ld	l,(hl)
	ld	h,a
p_SavePtr:
	.db 1
	ld	(hl),e
p_SavePtr2:
	.db 3
	ld	(hl),e
	inc	hl
	ld	(hl),d
p_SavePtrRev:
	.db 3
	ld	(hl),d
	inc	hl
	ld	(hl),e
p_LdSelf:
	.db 2
	ld	d,h
	ld	e,l
p_SwapHL:
	.db 3
	ld	a,h
	ld	h,l
	ld	l,a

;Optimized Math
;-----------------------------------------------
p_Add1:
	.db 1
	inc	hl
p_Add2:
	.db 2
	inc	hl
	inc	hl
p_Add3:
	.db 3
	inc	hl
	inc	hl
	inc	hl
p_Add254:
	.db 3
	inc	h
	dec	hl
	dec	hl
p_Add255:
	.db 2
	inc	h
	dec	hl
p_Add256:
	.db 1
	inc	h
p_Add257:
	.db 2
	inc	h
	inc	hl
p_Add258:
	.db 3
	inc	h
	inc	hl
	inc	hl
p_Add510:
	.db 4
	inc	h
	inc	h
	dec	hl
	dec	hl
p_Add511:
	.db 3
	inc	h
	inc	h
	dec	hl
p_Add512:
	.db 2
	inc	h
	inc	h
p_Add513:
	.db 3
	inc	h
	inc	h
	inc	hl
p_Add514:
	.db 4
	inc	h
	inc	h
	inc	hl
	inc	hl
p_Add767:
	.db 4
	inc	h
	inc	h
	inc	h
	dec	hl
p_Add768:
	.db 3
	inc	h
	inc	h
	inc	h
p_Add769:
	.db 4
	inc	h
	inc	h
	inc	h
	inc	hl
p_Add1024:
	.db 4
	inc	h
	inc	h
	inc	h
	inc	h

p_Sub1024:
	.db 4
	dec	h
	dec	h
	dec	h
	dec	h
p_Sub769:
	.db 4
	dec	h
	dec	h
	dec	h
	dec	hl
p_Sub768:
	.db 3
	dec	h
	dec	h
	dec	h
p_Sub767:
	.db 4
	dec	h
	dec	h
	dec	h
	inc	hl
p_Sub514:
	.db 4
	dec	h
	dec	h
	dec	hl
	dec	hl
p_Sub513:
	.db 3
	dec	h
	dec	h
	dec	hl
p_Sub512:
	.db 2
	dec	h
	dec	h
p_Sub511:
	.db 3
	dec	h
	dec	h
	inc	hl
p_Sub510:
	.db 4
	dec	h
	dec	h
	inc	hl
	inc	hl
p_Sub258:
	.db 3
	dec	h
	dec	hl
	dec	hl
p_Sub257:
	.db 2
	dec	h
	dec	hl
p_Sub256:
	.db 1
	dec	h
p_Sub255:
	.db 2
	dec	h
	inc	hl
p_Sub254:
	.db 3
	dec	h
	inc	hl
	inc	hl
p_Sub3:
	.db 3
	dec	hl
	dec	hl
	dec	hl
p_Sub2:
	.db 2
	dec	hl
	dec	hl
p_Sub1:
	.db 1
	dec	hl
p_Mul2:
	.db 1
	add	hl,hl
p_Mul3:
	.db 4
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,de
p_Mul4:
	.db 2
	add	hl,hl
	add	hl,hl
p_Mul5:
	.db 5
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,hl
	add	hl,de
p_Mul6:
	.db 5
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,de
	add	hl,hl
p_Mul7:
	.db 6
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,de
p_Mul8:
	.db 3
	add	hl,hl
	add	hl,hl
	add	hl,hl
p_Mul9:
	.db 6
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,de
p_Mul10:
	.db 6
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,hl
	add	hl,de
	add	hl,hl
p_Mul12:
	.db 6
	ld	d,h
	ld	e,l
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
p_Mul16:
	.db 4
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl
p_Mul32:
	.db 5
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	hl,hl
p_Mul64:
	.db 5
	ld	b,6
	add	hl,hl
	djnz	$-1
p_Mul128:
	.db 5
	ld	b,7
	add	hl,hl
	djnz	$-1
p_Mul255:
	.db 6
	ex	de,hl
	ld	h,e
	xor	a
	ld	l,a
	sbc	hl,de
p_Mul256:
	.db 3
	ld	h,l
	ld	l,0
p_Mul257:
	.db 3
	ld	a,l
	add	a,h
	ld	h,a
p_Mul258:
	.db 4
	ld	a,l
	add	hl,hl
	add	a,h
	ld	h,a
p_Mul260:
	.db 5
	ld	a,l
	add	hl,hl
	add	hl,hl
	add	a,h
	ld	h,a
p_Mul264:
	.db 6
	ld	a,l
	add	hl,hl
	add	hl,hl
	add	hl,hl
	add	a,h
	ld	h,a
p_Mul512:
	.db 4
	ld	h,l
	ld	l,0
	add	hl,hl
p_Mul513:
	.db 6
	ex	de,hl
	ld	h,e
	ld	l,0
	add	hl,hl
	add	hl,de
p_Mul514:
	.db 4
	ld	a,l
	add	a,h
	ld	h,a
	add	hl,hl
p_Mul516:
	.db 5
	ld	a,l
	add	hl,hl
	add	a,h
	ld	h,a
	add	hl,hl
p_Mul520:
	.db 6
	ld	a,l
	add	hl,hl
	add	hl,hl
	add	a,h
	ld	h,a
	add	hl,hl
p_Mul768:
	.db 6
	ld	a,l
	add	a,a
	add	a,l
	ld	h,a
	ld	l,0
p_Mul1024:
	.db 5
	ld	h,l
	ld	l,0
	add	hl,hl
	add	hl,hl
p_Mul1028:
	.db 5
	ld	a,l
	add	a,h
	ld	h,a
	add	hl,hl
	add	hl,hl
p_Mul1032:
	.db 6
	ld	a,l
	add	hl,hl
	add	a,h
	ld	h,a
	add	hl,hl
	add	hl,hl
p_Mul2048:
	.db 6
	ld	h,l
	ld	l,0
	add	hl,hl
	add	hl,hl
	add	hl,hl
p_Mul2056:
	.db 6
	ld	a,l
	add	a,h
	ld	h,a
	add	hl,hl
	add	hl,hl
	add	hl,hl
p_Mul4096:
	.db 5
	ld	b,12
	add	hl,hl
	djnz	$-1
p_Mul8192:
	.db 5
	ld	b,13
	add	hl,hl
	djnz	$-1
p_Mul16384:
	.db 5
	ld	b,14
	add	hl,hl
	djnz	$-1
p_Mul32768:
	.db 6
	xor	a
	rr	l
	ld	l,a
	rra
	ld	h,a
p_Div2:
	.db 4
	srl	h
	rr	l
p_Div10:
	.db 3
	B_CALL(_DivHLBy10)
p_Div128:
	.db 5
	xor	a
	add	hl,hl
	rla
	ld	l,h
	ld	h,a
p_Div256:
	.db 3
	ld	l,h
	ld	h,0
p_Div512:
	.db 5
	ld	l,h
	ld	h,0
	srl	l
p_Div32768:
	.db 5
	xor	a
	add	hl,hl
	ld	h,a
	rla
	ld	l,a
p_SDiv2:
	.db 4
	sra	h
	rr	l
p_SDiv64:
	.db 6
	add	hl,hl
	sbc	a,a
	add	hl,hl
	rla
	ld	l,h
	ld	h,a
p_SDiv128:
	.db 4
	add	hl,hl
	sbc	a,a
	ld	l,h
	ld	h,a
p_SDiv256:
	.db 5
	ld	a,h
	rla
	sbc	a,a
	ld	l,h
	ld	h,a
p_SDiv512:
	.db 6
	ld	a,h
	add	hl,hl
	sbc	hl,hl
	rra
	ld	l,a
p_SDiv16384:
	.db 6
	add	hl,hl
	sbc	a,a
	add	hl,hl
	ld	h,a
	rla
	ld	l,a
p_SDiv32768:
	.db 3
	add	hl,hl
	sbc	hl,hl
p_Mod2:
	.db 5
	xor	a
	ld	h,a
	inc	a
	and	l
	ld	l,a
p_Mod4:
	.db 6
	ld	h,0
	ld	a,l
	and	%00000011
	ld	l,a
p_Mod8:
	.db 6
	ld	h,0
	ld	a,l
	and	%00000111
	ld	l,a
p_Mod16:
	.db 6
	ld	h,0
	ld	a,l
	and	%00001111
	ld	l,a
p_Mod32:
	.db 6
	ld	h,0
	ld	a,l
	and	%00011111
	ld	l,a
p_Mod64:
	.db 6
	ld	h,0
	ld	a,l
	and	%00111111
	ld	l,a
p_Mod128:
	.db 4
	ld	h,0
	res	7,l
p_ZeroH:
p_Mod256:
	.db 2
	ld	h,0
p_Mod512:
	.db 4
	ld	a,%00000001
	and	h
	ld	h,a
p_Mod1024:
	.db 4
	ld	a,%00000011
	and	h
	ld	h,a
p_Mod2048:
	.db 4
	ld	a,%00000111
	and	h
	ld	h,a
p_Mod4096:
	.db 4
	ld	a,%00001111
	and	h
	ld	h,a
p_Mod8192:
	.db 4
	ld	a,%00011111
	and	h
	ld	h,a
p_Mod16384:
	.db 4
	ld	a,%00111111
	and	h
	ld	h,a
p_Mod32768:
	.db 2
	res	7,h
p_EQN512:
	.db 9
	inc	h
	inc	h
	ld	a,l
	and	h
	sub	255
	sbc	hl,hl
	inc	hl
p_EQN256:
	.db 8
	inc	h
	ld	a,l
	and	h
	sub	255
	sbc	hl,hl
	inc	hl
p_EQN2:
	.db 8
	inc	l
	ld	a,l
	and	h
	sub	255
	sbc	hl,hl
	inc	hl
p_EQN1:
	.db 7
	ld	a,l
	and	h
	sub	255
	sbc	hl,hl
	inc	hl
p_EQ0:
	.db 7
	ld	a,l
	or	h
	add	a,255
	sbc	hl,hl
	inc	hl
p_EQ1:
	.db 7
	ld	a,l
	dec	a
	or	h
	jr	z,$+4
	sbc	hl,hl
p_EQ2:
	.db 8
	dec	l
	ld	a,l
	dec	a
	or	h
	jr	z,$+4
	sbc	hl,hl
p_EQ256:
	.db 8
	dec	h
	ld	a,l
	or	h
	add	a,255
	sbc	hl,hl
	inc	hl
p_EQ512:
	.db 9
	dec	h
	dec	h
	ld	a,l
	or	h
	add	a,255
	sbc	hl,hl
	inc	hl
p_NEN512:
	.db 9
	inc	h
	inc	h
	ld	a,l
	or	h
	jr	z,$+5
	ld	hl,1
p_NEN256:
	.db 8
	inc	h
	ld	a,l
	or	h
	jr	z,$+5
	ld	hl,1
p_NEN2:
	.db 8
	inc	l
	ld	a,l
	and	h
	add	a,1
	sbc	hl,hl
	inc	hl
p_NEN1:
	.db 7
	ld	a,l
	and	h
	add	a,1
	sbc	hl,hl
	inc	hl
p_NE0:
	.db 7
	ld	a,l
	or	h
	jr	z,$+5
	ld	hl,1
p_NE1:
	.db 8
	ld	a,h
	dec	l
	or	l
	jr	z,$+5
	ld	hl,1
p_NE2:
	.db 9
	ld	a,h
	dec	l
	dec	l
	or	l
	jr	z,$+5
	ld	hl,1
p_NE256:
	.db 8
	dec	h
	ld	a,l
	or	h
	jr	z,$+5
	ld	hl,1
p_NE512:
	.db 9
	dec	h
	dec	h
	ld	a,l
	or	h
	jr	z,$+5
	ld	hl,1
p_GE1 =p_NE0
p_GT0 =p_NE0
p_LE0 =p_EQ0
p_LT1 =p_EQ0
p_SGE0:
	.db 4
	add	hl,hl
	sbc	hl,hl
	inc	hl
p_SLT0 =p_Div32768
p_GetBit0 =p_Div32768
p_GetBit1:
	.db 6
	xor	a
	add	hl,hl
	add	hl,hl
	ld	h,a
	rla
	ld	l,a
p_GetBit2:
	.db 7
	ld	a,h
	set	5,h
	cp	h
	sbc	hl,hl
	inc	hl
p_GetBit3:
	.db 7
	ld	a,h
	set	4,h
	cp	h
	sbc	hl,hl
	inc	hl
p_GetBit4:
	.db 7
	ld	a,h
	set	3,h
	cp	h
	sbc	hl,hl
	inc	hl
p_GetBit5:
	.db 7
	ld	a,h
	set	2,h
	cp	h
	sbc	hl,hl
	inc	hl
p_GetBit6:
	.db 7
	ld	a,%00000010
	and	h
	rrca
	ld	h,0
	ld	l,a
p_GetBit7:
	.db 6
	ld	a,%00000001
	and	h
	ld	h,0
	ld	l,a
p_GetBit8:
	.db 5
	xor	a
	ld	h,a
	add	hl,hl
	ld	l,h
	ld	h,a
p_GetBit9:
	.db 6
	xor	a
	add	hl,hl
	ld	h,a
	add	hl,hl
	ld	l,h
	ld	h,a
p_GetBit10:
	.db 7
	ld	a,l
	set	5,l
	cp	l
	sbc	hl,hl
	inc	hl
p_GetBit11:
	.db 7
	ld	a,l
	set	4,l
	cp	l
	sbc	hl,hl
	inc	hl
p_GetBit12:
	.db 7
	ld	a,l
	set	3,l
	cp	l
	sbc	hl,hl
	inc	hl
p_GetBit13:
	.db 7
	ld	a,l
	set	2,l
	cp	l
	sbc	hl,hl
	inc	hl
p_GetBit14:
	.db 7
	ld	a,%00000010
	and	l
	rrca
	ld	h,0
	ld	l,a
p_GetBit15 =p_Mod2

;Comparing numbers
;-----------------------------------------------
p_IntEq:
	.db 8
	xor	a
	sbc	hl,de
	ld	h,a
	ld	l,a
	jr	nz,$+3
	inc	l
p_IntNe:
	.db 8
	xor	a
	sbc	hl,de
	jr	z,$+5
	ld	hl,1
p_IntGt:
	.db 7
	ex	de,hl
	xor	a
	sbc	hl,de
	ld	h,a
	rla
	ld	l,a
p_IntGe:
	.db 6
	xor	a
	sbc	hl,de
	sbc	hl,hl
	inc	hl
p_IntLt:
	.db 6
	xor	a
	sbc	hl,de
	ld	h,a
	rla
	ld	l,a
p_IntLe:
	.db 7
	ex	de,hl
	xor	a
	sbc	hl,de
	sbc	hl,hl
	inc	hl
p_EQX:
	.db 9
	ld	a,l
	sub	$00
	or	h
	add	a,255
	sbc	hl,hl
	inc	hl
	.db rp_Byte,7
p_EQNX:
	.db 9
	ld	a,l
	sub	$00
	and	h
	sub	255
	sbc	hl,hl
	inc	hl
	.db rp_Byte,7
p_NEX:
	.db 9
	ld	a,l
	sub	$00
	or	h
	sub	1
	sbc	hl,hl
	inc	hl
	.db rp_Byte,7
p_NENX:
	.db 9
	ld	a,l
	sub	$00
	and	h
	add	a,1
	sbc	hl,hl
	inc	hl
	.db rp_Byte,7
p_Min:
	.db 8
	pop	de
	or	a
	sbc	hl,de
	add	hl,de
	jr	c,$+3
	ex	de,hl
p_Max:
	.db 8
	pop	de
	or	a
	sbc	hl,de
	add	hl,de
	jr	nc,$+3
	ex	de,hl

;Signed Stuff
;-----------------------------------------------
p_SIntGt:
	.db 12
	ld	bc,$8000
	add	hl,bc
	ex	de,hl
	add	hl,bc
	xor	a
	sbc	hl,de
	ld	h,a
	rla
	ld	l,a
p_SIntGe:
	.db 11
	xor	a
	ld	b,h
	sbc	hl,de
	rra
	xor	b
	xor	d
	rla
	sbc	hl,hl
	inc	hl
p_SIntLt:
	.db 12
	xor	a
	ld	b,h
	sbc	hl,de
	ld	h,a
	rra
	xor	b
	xor	d
	rlca
	and	1
	ld	l,a
p_SIntLe:
	.db 12
	ex	de,hl
	xor	a
	ld	b,h
	sbc	hl,de
	rra
	xor	b
	xor	d
	rla
	sbc	hl,hl
	inc	hl

;Addition and Subtraction
;-----------------------------------------------
p_Add:
	.db 1
	add	hl,de
p_Sub:
	.db 3
	or	a
	sbc	hl,de
p_IntNeg:
	.db 6
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
p_AbsInt:
	.db 10
	bit	7,h
	jr	z,$+8
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
p_MiniSigned:
	.db 5
	ld	a,(hl)
	ld	l,a
	rlca
	sbc	a,a
	ld	h,a

;Bit Operations
;-----------------------------------------------
p_IntOr:
	.db 6
	ld	a,h
	or	d
	ld	h,a
	ld	a,l
	or	e
	ld	l,a
p_IntAnd:
	.db 6
	ld	a,h
	and	d
	ld	h,a
	ld	a,l
	and	e
	ld	l,a
p_IntXor:
	.db 6
	ld	a,h
	xor	d
	ld	h,a
	ld	a,l
	xor	e
	ld	l,a
p_IntNot:
	.db 6
	ld	a,h
	cpl
	ld	h,a
	ld	a,l
	cpl
	ld	l,a
p_IntOrImm:
	.db 4
	ld	a,h
	or	$00
	ld	h,a
	.db rp_Byte,2
p_IntAndImm:
	.db 4
	ld	a,h
	and	$00
	ld	h,a
	.db rp_Byte,2
p_IntXorImm:
	.db 4
	ld	a,h
	xor	$00
	ld	h,a
	.db rp_Byte,2

p_BoolOr:
	.db 3
	ld	a,l
	or	e
	ld	l,a
p_BoolAnd:
	.db 3
	ld	a,l
	and	e
	ld	l,a
p_BoolXor:
	.db 3
	ld	a,l
	xor	e
	ld	l,a
p_BoolNot:
	.db 3
	ld	a,l
	cpl
	ld	l,a
p_BoolOrImm:
	.db 4
	ld	a,l
	or	$00
	ld	l,a
	.db rp_Byte,2
p_BoolAndImm:
	.db 4
	ld	a,l
	and	$00
	ld	l,a
	.db rp_Byte,2
p_BoolXorImm:
	.db 4
	ld	a,l
	xor	$00
	ld	l,a
	.db rp_Byte,2

;Control Structures
;-----------------------------------------------
p_Jp:
	.db 3
	jp	$0000
	.db rp_Ans,2
p_Jr:
	.db 2
	jr	$+0
	.db rp_Byte,1
p_JpHL:
	.db 1
	jp	(hl)
p_JpSymbol:
	.db 3
	jp	$0000
	.db rp_Sym,2
p_JpIfZero:
	.db 5
	ld	a,h
	or	l
	jp	z,$0000
	.db rp_Sym,2
p_JpIfNotZero:
	.db 5
	ld	a,h
	or	l
	jp	nz,$0000
	.db rp_Sym,2
p_JpIfNotAns:
	.db 5
	ld	a,h
	or	l
	jp	z,$0000
	.db rp_Ans,2
p_JpIfAns:
	.db 5
	ld	a,h
	or	l
	jp	nz,$0000
	.db rp_Ans,2
p_JpCpHLDE:
	.db 6
	or	a
	sbc	hl,de
	jp	c,$0000
	.db rp_Sym,2
p_Call:
	.db 3
	call	$0000
	.db rp_Ans,2
p_CallSymbol:
	.db 3
	call	$0000
	.db rp_Sym,2
p_Ret:
	.db 1
	ret
p_RetIfTrue:
	.db 3
	ld	a,h
	or	l
	ret	nz
p_RetIfFalse:
	.db 3
	ld	a,h
	or	l
	ret	z
p_SkipNext4:
	.db 2
	jr	$+6

;Data Manipulation
;-----------------------------------------------
p_Fill:
	.db 7
	ex	(sp),hl
	pop	bc
	HL_INTO_DE()
	inc	de
	ldir
p_Copy:
	.db 5
	pop	de
	ex	(sp),hl
	pop	bc
	ldir
p_CopyRev:
	.db 5
	pop	de
	ex	(sp),hl
	pop	bc
	lddr
p_Exchange:
	.db 13
	pop	de
	ex	(sp),hl
	pop	bc
	ld	a,(de)
	ldi
	dec	hl
	ld	(hl),a
	inc	hl
	ld	a,b
	or	c
	jr	nz,$-8

p_Nib1:
	.db __Nib1End-$-1
	xor	a
	scf
	rr	h
	rr	l
	ld	b,(hl)
__Nib1Loop:
	rrd
	ccf
	jr	nc,__Nib1Loop
	ld	(hl),b
	ld	l,a
	ld	h,0
	ret
__Nib1End:

p_Nib2:
	.db __Nib2End-$-1
	xor	a
	srl	h
	rr	l
	rrd
	jr	nc,__Nib2Skip
	rld
__Nib2Skip:
	ld	l,a
	ld	h,0
	ret
__Nib2End:

p_NibSto:
	.db __NibStoEnd-$-1
	pop	bc
	pop	de
	push	bc
	scf
	rr	h
	rr	l
	jr	c,__NibStoHigh
	rrd
	ld	a,e
	rld
	ret
__NibStoHigh:
	rld
	ld	a,e
	rrd
	ret
__NibStoEnd:


;Text
;-----------------------------------------------

;Homescreen Text
;-----------------------
p_DispStr:
	.db 3
	B_CALL(_PutS)
p_DispInt:
	.db 3
	B_CALL(_DispHL)
p_DispChar:
	.db 4
	ld	a,l
	B_CALL(_PutC)
p_DispTok:
	.db 4
	ex	de,hl
	B_CALL(_PutTokString)

;Homescreen App Text
;-----------------------
p_DispStrApp:
	.db 10
__DispStrAppLoop:
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,__DispStrAppDone
	B_CALL(_PutC)
	jr	__DispStrAppLoop
__DispStrAppDone:


;Graphscreen Text
;-----------------------
p_TextStr:
	.db 3
	B_CALL(_VPutS)
p_TextInt:
	.db __TextIntEnd-1-$
	B_CALL(_SetXXXXOP2)
	B_CALL(_Op2ToOP1)
	ld a,5
	B_CALL(_DispOP1A)
	ret
__TextIntEnd:

p_TextChar:
	.db 4
	ld	a,l
	B_CALL(_VPutMap)
p_TextTok:
	.db 10
	B_CALL(_Get_Tok_Strng)
	ld	b,a
	ld	hl,OP3
	B_CALL(_VPutSN)

;Graphscreen App Text
;-----------------------
p_TextStrApp:
	.db 10
__TextStrAppLoop:
	ld	a,(hl)
	inc	hl
	or	a
	jr	z,__TextStrAppDone
	B_CALL(_VPutMap)
	jr	nc,__TextStrAppLoop
__TextStrAppDone:

;Cursor Position
;-----------------------
p_SetCurRow:
	.db 4
	ld	a,l
	ld	(CurRow),a
p_SetCurCol:
	.db 4
	ld	a,l
	ld	(CurCol),a
p_SetCurColRow:
	.db 3
	ld	(CurRow),hl
p_SetPenRow:
	.db 4
	ld	a,l
	ld	(PenRow),a
p_SetPenCol:
	.db 3
	ld	(PenCol),hl

;Text Flags
;-----------------------

p_TextSmall:
	.db 4
	res	2,(iy+50)
p_TextLarge:
	.db 4
	set	2,(iy+50)
p_TextNorm:
	.db 4
	res	3,(iy+5)
p_TextInv:
	.db 4
	set	3,(iy+5)
p_TextScrn:
	.db 4
	res	7,(iy+20)
p_TextBuf:
	.db 4
	set	7,(iy+20)
p_TextScroll:
	.db 4
	set	2,(iy+13)
p_TextNoScroll:
	.db 4
	res	2,(iy+13)
p_PlotToScrn:
	.db 4
	res	0,(iy+60)
p_PlotToBuff:
	.db 4
	set	0,(iy+60)

;Other
;-----------------------
p_NewLine:
	.db 3
	B_CALL(_NewLine)
p_Length:
	.db __LengthEnd-$-1
	xor	a
	ld	b,a
	ld	c,a
	cpir
	ld	hl,-1
	sbc	hl,bc
	ret
__LengthEnd:

p_InData:
	.db __InDataEnd-$-1
	ld	d,h
	ld	e,l
__InDataLoop:
	ld	a,(hl)
	inc	hl
	cp	c
	jr	z,__InDataDone
	or	a
	jr	nz,__InDataLoop
	ld	h,d
	ld	l,e
__InDataDone:
	sbc	hl,de
	ret
__InDataEnd:

p_EquStr:
	.db __EquStrEnd-$-1
__EquStrLoop:
	ld	a,(de)
	cp	(hl)
	ret	nz
	inc	hl
	inc	de
	or	a
	jr	nz,__EquStrLoop
	ld	h,a
	ld	l,a
	ret
__EquStrEnd:

p_Input:
	.db __InputEnd-$-1
	res	6,(iy+$1C)
	set	7,(iy+$09)
	xor	a
	ld	(ioPrompt),a
	B_CALL(_GetStringInput)
	B_CALL(_ZeroOP1)
	ld	hl,$2D04
	ld	(OP1),hl
	B_CALL(_ChkFindSym)
	inc	de
	inc	de
	ex	de,hl
	ret
__InputEnd:

p_ToHex:
	.db __ToHexEnd-$-1
	ld	b,4
	ld	de,vx_SptBuff
	push	de
__ToHexLoop:
	ld	a,$1F
__ToHexShift:
	add	hl,hl
	rla
	jr	nc,__ToHexShift
	daa
	add	a,$A0
	adc	a,$40
	ld	(de),a
	inc	de
	djnz	__ToHexLoop
	xor	a
	ld	(de),a
	pop	hl
	ret
__ToHexEnd:

;Screen
;-----------------------------------------------
p_ClearScreen:
	.db 6
	B_CALL(_HomeUp)
	B_CALL(_ClrScrnFull)
p_ClearBuffer:
	.db 3
	B_CALL(_GrBufClr)
p_ClearBackBuffer:
	.db 6
	ld	hl,appBackUpScreen
	B_CALL(_BufClr)
p_CopyBuffer:
	.db 3
	B_CALL(_GrBufCpy)
p_SaveToBuffer:
	.db 6
	ld	hl,plotSScreen
	B_CALL(_SaveDisp)
p_InvBuff:
	.db __InvBuffEnd-1-$
	ld	hl,plotSScreen
	ld	bc,3
__InvBuffLoop:
	ld	a,(hl)
	cpl
	ld	(hl),a
	inc	hl
	djnz	__InvBuffLoop
	dec	c
	jr	nz,__InvBuffLoop
	ret
__InvBuffEnd:

;First Iter:  63 T-states
;Enter Loop:  62 T-states
;Inside Loop: 62 T-states
;Next Loop:   63 T-states
p_FastCopy:
	.db __FastCopyEnd-1-$
FastCopy:
	ld	hl,plotSScreen
	ld	a,i
	push	af
	di
	ld	a,$80
	out	($10),a			;11
	ld	c,-$0C			;7
	ex	(sp),hl			;19 (waste)
	ex	(sp),hl			;19 (waste)
__FastCopyAgain:
	ld	b,64			;7
	ld	a,c			;4
	add	a,$2C			;7
	out	($10),a			;11
	ld	a,(hl)			;7 (waste)
	inc	de			;6 (waste)
__FastCopyLoop:
	push	af			;11 (waste)
	pop	af			;10 (waste)
	ld	de,12			;10
	ld	a,(hl)			;7
	add	hl,de			;11
	out	($11),a			;11
	djnz	__FastCopyLoop		;13/8
	ld	de,1-(12*64)		;10
	add	hl,de			;11
	inc	c			;4
	jr	nz,__FastCopyAgain	;12
	pop	af
	ret	po
	ei
	ret
__FastCopyEnd:

p_DrawAndClr:
	.db __DrawAndClrEnd-1-$
	ld	hl,plotSScreen
	ld	a,i
	push	af
	di
	ld	a,$80
	out	($10),a
	ld	c,-$0C
__DrawAndClrAgain:
	ld	b,64
	ld	de,12
	in	a,($10)
	rla
	jr	c,$-3
	ld	a,c
	add	a,$2C
	out	($10),a
__DrawAndClrLoop:
	in	a,($10)
	rla
	jr	c,$-3
	ld	a,(hl)
	out	($11),a
	ld	(hl),d
	add	hl,de
	djnz	__DrawAndClrLoop
	ld	de,1-(12*64)
	add	hl,de
	inc	c
	jr	nz,__DrawAndClrAgain
	pop	af
	ret	po
	ei
	ret
__DrawAndClrEnd:

p_FrontToBack:
	.db 11
	ld	hl,plotSScreen
	ld	de,appBackUpScreen
	ld	bc,768
	ldir
p_BackToFront:
	.db 11
	ld	hl,appBackUpScreen
	ld	de,plotSScreen
	ld	bc,768
	ldir

;Screen Shifting
;-----------------------------------------------
p_ShiftLeft:
	.db __ShiftLeftEnd-1-$
	ld	hl,plotSScreen+767
	ld	c,64
__ShiftLeftLoop:
	ld	b,12
	or	a
__ShiftLeftShift:
	rl	(hl)
	dec	hl
	djnz	__ShiftLeftShift
	dec	c
	jr	nz,__ShiftLeftLoop
	ret
__ShiftLeftEnd:

p_ShiftRight:
	.db __ShiftRightEnd-1-$
	ld	hl,plotSScreen
	ld	c,64
__ShiftRightLoop:
	ld	b,12
	or	a
__ShiftRightShift:
	rr	(hl)
	inc	hl
	djnz	__ShiftRightShift
	dec	c
	jr	nz,__ShiftRightLoop
	ret
__ShiftRightEnd:

p_ShiftUp:
	.db __ShiftUpEnd-1-$
	ld	hl,plotSScreen+12
	ld	de,plotSScreen
	ld	bc,768-12
	ldir
	ret
__ShiftUpEnd:

p_ShiftDown:
	.db __ShiftDownEnd-1-$
	ld	hl,plotSScreen+767-12
	ld	de,plotSScreen+767
	ld	bc,768-12
	lddr
	ret
__ShiftDownEnd:


;Input
;-----------------------------------------------
p_GetKey:
	.db 6
	B_CALL(_GetCSC)
	ld	h,0
	ld	l,a

p_GetKeyPause:
	.db 6
	B_CALL(_GetKeyRetOff)
	ld	h,0
	ld	l,a

p_DKey:
	.db __DKeyEnd-1-$
	ld	a,h
	out	($01),a
	xor	a
	cp	h
	ld	h,a
	ld	a,(hl)
	in	a,($01)
	jr	z,__DKeyAll
	and	l
	ld	l,h
	ret	nz
__DKeyAll:
	inc	a
	ld	l,a
	ret
__DKeyEnd:

p_DKeyVar:
	.db __DKeyVarEnd-1-$
	dec	l
	ld	a,l
	rra
	rra
	rra
	and	%00000111
	inc	a
	ld	b,a
	ld	a,%01111111
	rlca
	djnz	$-1
	ld	h,a
	ld	a,l
	and	%00000111
	inc	a
	ld	b,a
	ld	a,%10000000
	rlca
	djnz	$-1
	ld	l,a
	ret
__DKeyVarEnd:

p_OnKey:
	.db 9
	in a,(4)
	rra
	rra
	rra
	rra
	sbc hl,hl
	inc hl

p_Rand:
	.db __RandEnd-1-$
	ld	hl,(vx_Seed)
	ld	a,r
	ld	d,a
	ld	e,(hl)
	add	hl,de
	ld	e,a
	add	hl,de
	ld	(vx_Seed),hl
	ret
__RandEnd:

;System
;-----------------------------------------------
p_DiagOn:
	.db 7
	B_CALL(_RunIndicOn)
	set	5,(iy+0)
p_DiagOff:
	.db 7
	B_CALL(_RunIndicOff)
	res	5,(iy+0)
p_FullSpeed:
	.db 9
	FULLSPEED()
	ld	l,a
	ld	h,a
p_NormalSpeed:
	.db 3
	xor	a
	out	($20),a
p_Pause:
	.db 7
	djnz	$+0
	dec	hl
	ld	a,l
	or	h
	jr	nz,$-5
p_Contrast:
	.db 5
	ld	a,l
	or	%11000000
	out	($10),a
p_GetContrast:
	.db 6
	ld	a,(contrast)
	add	a,24
	ld	l,a
;Linking
;-----------------------------------------------
p_PortOut:
	.db 3
	ld	a,l
	out	($00),a
p_PortIn:
	.db 7
	in	a,($00)
	and	%00000011
	ld	l,a
	ld	h,0

p_FreqOut:
	.db __FreqOutEnd-1-$
	xor	a
__FreqOutLoop1:
	push	bc
	ld	e,a
__FreqOutLoop2:
	ld	a,h
	or	l
	jr	z,__FreqOutDone
	dec	hl
	dec	bc
	ld	a,b
	or	c
	jr	nz,__FreqOutLoop2
	ld	a,e
	xor	%00000011
	scf
__FreqOutDone:
	pop	bc
	out	($00),a
	ret	nc
	jr	__FreqOutLoop1
__FreqOutEnd:

p_GetByte:
	.db __GetByteEnd-$-1
	di
	LDBC(8,3)			;Bit counter in b, bit mask in c
	ld	hl,-1
	xor	a
	out	(0),a			;Make sure we are reset
	in	a,(0)
	and	c			;Check to see if sender is ready
	dec	a
	ret	nz			;If not, then go back
	inc	a
	out	(0),a			;Relay a confirmation
	ex	(sp),hl			;Wait at until confirmation is read (59 T-states minimum)
	ex	(sp),hl
	ld	a,(de)			;Bit counter in b and bitmask in c
	xor	a			;Store received byte in l
	nop
	inc	hl
	out	(0),a			;Reset the ports to receive data
__GetByteLoop:
	in	a,(0)			;Wait until first bit is sent Max loop wait (49 T-states max)
	and	c
	jr	z,__GetByteZero		;Go here if we get a zero
	cp	c
	jr	nz,__GetByteLoop
__GetByteOne:				;Go here if we get a one
	scf
__GetByteZero:
	rl	l			;Shift the bits left, zero is in the first bit
	ex	(sp),hl			;Wait for the sender to change (75 T-states min, 107 max)
	ex	(sp),hl
	djnz	__GetByteLoop		;Keep looping if we haven't got all the bits
	ret				;Otherwise, we're done.
__GetByteEnd:

p_SendByte:
	.db __SendByteEnd-$-1
	di
	LDBC(8,3)			;Bit counter in b, bit mask in c
	ld	a,%00000010
	out	(0),a			;Indicate we are ready to send
__SendByteTimeout:
	dec	hl
	ld	a,h
	or	l
	jr	z,__SendByteDone
	in	a,(0)			;Loop is 59 T-states maximum
	and	c
	jr	nz,__SendByteTimeout	;Keep looping till we get it
__SendByteLoop:
	inc	a
	out	(0),a			;This is the waiting state
	ex	(sp),hl			;Wait (115 T-states min)
	ex	(sp),hl
	ex	(sp),hl
	ex	(sp),hl
	xor	a
	ld	h,0
	ld	l,-1
	rl	e			;Check the first bit
	jr	c,__SendByteSkip
	ld	a,c			;Send a one
__SendByteSkip:				;Send a zero
	out	(0),a
	ex	(sp),hl			;Wait (59 T-states)
	ex	(sp),hl
	xor	a
	djnz	__SendByteLoop		;Loop if there are more bits to send
__SendByteDone:
	out	(0),a			;Reset the port
	ret				;We're done
__SendByteEnd:

;Multiplication and division
;-----------------------------------------------
p_Mul:
	.db __MulEnd-1-$
	ld	c,h
	ld	a,l
	ld	hl,0
	ld	b,16
__MulNext:
	add	hl,hl
	rla
	rl	c
	jr	nc,__MulSkip
	add	hl,de
	adc	a,0
	jr	nc,__MulSkip
	inc	c
__MulSkip:
	djnz	__MulNext
	ret
__MulEnd:


p_88Mul:
	.db __88MulEnd-1-$
	ld	a,h
	xor	d
	push	af
	bit	7,h
	jr	z,$+8
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
	bit	7,d
	jr	z,$+8
	xor	a
	sub	e
	ld	e,a
	sbc	a,a
	sub	d
	ld	d,a
	call	$3F00+sub_Mul
	ld	l,h
	ld	h,a
	pop	af
	xor	h
	add	a,a
	ret	nc
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
	ret
__88MulEnd:
	.db rp_Ans,15

p_Div:
	.db __DivEnd-1-$		;1286 T-states avg for Long Div
	ld	b,16			;857  T-states avg for Short Div
	ld	a,d
	or	a
	jr	z,__DivLoop8
	ld	a,h
	ld	c,l
	sbc	hl,hl			;carry flag was reset
__DivLoop:
	scf
	rl	c
	rla
	adc	hl,hl
	sbc	hl,de
	jr	nc,__DivSkip
	add	hl,de
	dec	c
__DivSkip:
	djnz	__DivLoop
	ld	h,a
	ld	l,c
	ret
__DivLoop8:
	add	hl,hl
	rla
	jr	c,__DivOverflow
	cp	e
	jr	c,__DivSkip8
__DivOverflow:
	sub	e
	inc	l
__DivSkip8:
	djnz	__DivLoop8
	ret
__DivEnd:

p_SDiv:
	.db __SDivEnd-1-$
	ld	a,h
	xor	d
	push	af
	bit	7,h
	jr	z,$+8
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
	bit	7,d
	jr	z,$+8
	xor	a
	sub	e
	ld	e,a
	sbc	a,a
	sub	d
	ld	d,a
	call	$3F00+sub_Div
	pop	af
	add	a,a
	ret	nc
	xor	a
	sub	l
	ld	l,a
	sbc	a,a
	sub	h
	ld	h,a
	ret
__SDivEnd:
	.db rp_Ans,12

p_Mod:
	.db __ModEnd-1-$
	ld	a,h
	ld	c,l
	ld	hl,0
	ld	b,16
__ModLoop:
	scf
	rl	c
	rla
	adc	hl,hl
	sbc	hl,de
	jr	nc,__ModSkip
	add	hl,de
	dec	c
__ModSkip:
	djnz	__ModLoop
	ret
__ModEnd:

;Pixel Routines
;-----------------------------------------------
p_Pix:
	.db __PixEnd-1-$		;Draws pixel (e,l)
	ld	bc,plotSScreen
	ld	d,0

	ld	a,l
	cp	64
	ld	a,d
	ret	nc
	ld	a,e
	cp	96
	ld	a,d
	ret	nc

	ld	h,d
	ld	a,l
	add	a,a
	add	a,l
	ld	l,a
	add	hl,hl
	add	hl,hl
	add	hl,bc
	ld	a,e
	srl	e
	srl	e
	srl	e
	add	hl,de
	and	%00000111
	ld	b,a
	ld	a,%10000000
	ret	z
___GetPixLoop:
	rrca
	djnz	___GetPixLoop
	ret
__PixEnd:

p_SetPix:
	.db 2
	or	(hl)
	ld	(hl),a
p_ResPix
	.db 3
	cpl
	and	(hl)
	ld	(hl),a
p_InvPix:
	.db 2
	xor	(hl)
	ld	(hl),a
p_GetPix:
	.db 7
	and	(hl)
	ld	hl,0
	jr	z,$+3
	inc	l

;Sprite Routines
;-----------------------------------------------
p_DrawSteal:
	.db 8
	ld	de,-11
	add	hl,de
	pop	ix
	pop	bc
	pop	de

p_DrawOr:
	.db __DrawOrEnd-1-$
	push	hl
	pop	ix
	ld	hl,plotSScreen-11	;Input hl = Sprite
	ld	b,7			;Input c = Sprite X Position
	ld	d,0			;Input e = Sprite Y Position
	ld	a,c
	add	a,b
	jr	c,__DrawOrClipLeft
	sub	96+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawOrNoClipH
__DrawOrClipRight:
	inc	d
	jr	__DrawOrClipHDone
__DrawOrClipLeft:
	add	a,89
	ld	c,a
__DrawOrClipHDone:
	inc	d		;d,c,e are updated
__DrawOrNoClipH:
	ld	a,e
	add	a,b
	jr	c,__DrawOrClipTop
	sub	64+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawOrNoClipV
	jr	__DrawOrClipBottom
__DrawOrClipTop:
	inc	ix
	inc	e
	jr	nz,__DrawOrClipTop
__DrawOrClipBottom:
	ld	b,a
__DrawOrNoClipV:			;b,ix,e are updated.
	dec	d
	jr	z,__DrawOrNoFix
	inc	e
__DrawOrNoFix:
	push	de
	push	hl
	ld	d,0
	ld	h,d
	ld	l,e
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,c
	ld	a,e
	srl	e
	srl	e
	srl	e
 	add	hl,de
	pop	de
	add	hl,de
	pop	de
	inc	b
 	and	%00000111
	jr	z,__DrawOrAligned
	ld	c,a
__DrawOrLoop:
	push	bc
	ld	b,c
	ld	c,(ix+0)
	xor	a
__DrawOrShift:
	srl	c
	rra
	djnz	__DrawOrShift
	dec	d
	jr	z,__DrawOrSkipRight
	or	(hl)
	ld	(hl),a
__DrawOrSkipRight:
	dec	hl
	inc	d
	jr	z,__DrawOrSkipLeft
	ld	a,c
	or	(hl)
	ld	(hl),a
__DrawOrSkipLeft:
	ld	c,13
	add	hl,bc
	inc	ix
	pop	bc
	djnz	__DrawOrLoop
	ret
__DrawOrAligned:
	dec	hl
	ld	de,12
__DrawOrAlignedLoop:
	ld	a,(ix+0)
	or	(hl)
	ld	(hl),a
	inc	ix
	add	hl,de
	djnz	__DrawOrAlignedLoop
	ret
__DrawOrEnd:

p_DrawXor:
	.db __DrawXorEnd-1-$
	push	hl
	pop	ix
	ld	hl,plotSScreen-11	;Input hl = Sprite
	ld	b,7			;Input c = Sprite X Position
	ld	d,0			;Input e = Sprite Y Position
	ld	a,c
	add	a,b
	jr	c,__DrawXorClipLeft
	sub	96+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawXorNoClipH
__DrawXorClipRight:
	inc	d
	jr	__DrawXorClipHDone
__DrawXorClipLeft:
	add	a,89
	ld	c,a
__DrawXorClipHDone:
	inc	d		;d,c,e are updated
__DrawXorNoClipH:
	ld	a,e
	add	a,b
	jr	c,__DrawXorClipTop
	sub	64+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawXorNoClipV
	jr	__DrawXorClipBottom
__DrawXorClipTop:
	inc	ix
	inc	e
	jr	nz,__DrawXorClipTop
__DrawXorClipBottom:
	ld	b,a
__DrawXorNoClipV:			;b,ix,e are updated.
	dec	d
	jr	z,__DrawXorNoFix
	inc	e
__DrawXorNoFix:
	push	de
	push	hl
	ld	d,0
	ld	h,d
	ld	l,e
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,c
	ld	a,e
	srl	e
	srl	e
	srl	e
 	add	hl,de
	pop	de
	add	hl,de
	pop	de
	inc	b
 	and	%00000111
	jr	z,__DrawXorAligned
	ld	c,a
__DrawXorLoop:
	push	bc
	ld	b,c
	ld	c,(ix+0)
	xor	a
__DrawXorShift:
	srl	c
	rra
	djnz	__DrawXorShift
	dec	d
	jr	z,__DrawXorSkipRight
	xor	(hl)
	ld	(hl),a
__DrawXorSkipRight:
	dec	hl
	inc	d
	jr	z,__DrawXorSkipLeft
	ld	a,c
	xor	(hl)
	ld	(hl),a
__DrawXorSkipLeft:
	ld	c,13
	add	hl,bc
	inc	ix
	pop	bc
	djnz	__DrawXorLoop
	ret
__DrawXorAligned:
	dec	hl
	ld	de,12
__DrawXorAlignedLoop:
	ld	a,(ix+0)
	xor	(hl)
	ld	(hl),a
	inc	ix
	add	hl,de
	djnz	__DrawXorAlignedLoop
	ret
__DrawXorEnd:

p_DrawOff:
	.db __DrawOffEnd-1-$
	push	hl
	pop	ix
	ld	hl,plotSScreen-11	;Input hl = Sprite
	ld	b,7			;Input c = Sprite X Position
	ld	d,0			;Input e = Sprite Y Position
	ld	a,c
	add	a,b
	jr	c,__DrawOffClipLeft
	sub	96+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawOffNoClipH
__DrawOffClipRight:
	inc	d
	jr	__DrawOffClipHDone
__DrawOffClipLeft:
	add	a,89
	ld	c,a
__DrawOffClipHDone:
	inc	d		;d,c,e are updated
__DrawOffNoClipH:
	ld	a,e
	add	a,b
	jr	c,__DrawOffClipTop
	sub	64+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawOffNoClipV
	jr	__DrawOffClipBottom
__DrawOffClipTop:
	inc	ix
	inc	e
	jr	nz,__DrawOffClipTop
__DrawOffClipBottom:
	ld	b,a
__DrawOffNoClipV:			;b,ix,e are updated.
	dec	d
	jr	z,__DrawOffNoFix
	inc	e
__DrawOffNoFix:
	push	de
	push	hl
	ld	d,0
	ld	h,d
	ld	l,e
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,c
	ld	a,e
	srl	e
	srl	e
	srl	e
 	add	hl,de
	pop	de
	add	hl,de
	pop	de
	inc	b
 	and	%00000111
	jr	z,__DrawOffAligned
	ld	c,a
__DrawOffLoop:
	push	bc
	ld	b,c
	ld	c,(ix+0)
	xor	a
	ld	e,a
	dec	a
__DrawOffShift:
	srl	c
	rr	e
	rra
	djnz	__DrawOffShift
	dec	d
	jr	z,__DrawOffSkipRight
	ld	b,a
	and	(hl)
	or	e
	ld	(hl),a
	ld	a,b
__DrawOffSkipRight:
	dec	hl
	inc	d
	jr	z,__DrawOffSkipLeft
	cpl
	and	(hl)
	or	c
	ld	(hl),a
__DrawOffSkipLeft:
	ld	bc,13
	add	hl,bc
	inc	ix
	pop	bc
	djnz	__DrawOffLoop
	ret
__DrawOffAligned:
	dec	hl
	ld	de,12
__DrawOffAlignedLoop:
	ld	a,(ix+0)
	ld	(hl),a
	inc	ix
	add	hl,de
	djnz	__DrawOffAlignedLoop
	ret
__DrawOffEnd:

p_EzSprite:
	.db 7
	pop	de
	ld	a,e
	pop	de
	ld	d,a
	B_CALL(_DisplayImage)

;Advanced Math
;-----------------------------------------------
p_Sqrt:
	.db __SqrtEnd-1-$
	ld	de,$00FF
	ld	b,e
	ld	c,e
__SqrtLoop:
	add	hl,bc
	inc	e
	dec	c
	dec	bc
	jr	c,__SqrtLoop
	ex	de,hl
	ret
__SqrtEnd:


p_SinSetup:
	.db 1
	ld	a,l

p_CosSetup:
	.db 3
	ld	a,l
	add	a,64
p_Sin:
	.db __SinEnd-1-$
	ld	c,a
	add	a,a
	ld	d,a
	cpl
	ld	e,a
	xor	a
	ld	b,8
__SinLoop:
	rra
	rrc	e
	jr	nc,__SinSkip
	add	a,d
__SinSkip:
	djnz	__SinLoop
	ld	l,a
	ld	h,b
	or	c
	ret	p
	xor	a
	sub	l
	ret	z
	ld	l,a
	dec	h
	ret
__SinEnd:

p_Log:
	.db 10
	ld	de,16
	scf
__LogLoop:
	adc	hl,hl
	dec	e
	jr	nc,__LogLoop
	ex	de,hl

p_Exp:
	.db 10
	ld	b,l
	inc	b
	ld	hl,0
	scf
__ExpLoop:
	adc	hl,hl
	djnz	__ExpLoop

;VAT manipulation
;-----------------------------------------------
p_GetCalc:
	.db __GetCalcEnd-1-$
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	ld	hl,0
	ret	c
	inc	b
	dec	b
	ret	nz
	inc	de
	inc	de
	ex	de,hl
	ret
__GetCalcEnd:

p_NewVar:
	.db __NewVarEnd-1-$
	B_CALL(_EnoughMem)
	pop	hl
	ex	(sp),hl
	jr	c,__NewVarFail
	push	de
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	jr	c,__NewVarSkip
	B_CALL(_DelVarArc)
__NewVarSkip:
	pop	hl
	ld	a,(OP1)
	B_CALL(_CreateVar)
	ex	de,hl
	inc	hl
	inc	hl
	ret
__NewVarFail:
	ld	hl,0
	ret
__NewVarEnd:

p_Unarchive:
	.db __UnarchiveEnd-1-$
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	ld	hl,0
	ret	c
	inc	b
	dec	b
	ret	z
	B_CALL(_Arc_Unarc)
	ld	hl,1
	ret
__UnarchiveEnd:

p_Archive:
	.db __ArchiveEnd-1-$
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	ld	hl,0
	ret	c
	inc	b
	dec	b
	ret	nz
	B_CALL(_Arc_Unarc)
	ld	hl,1
	ret
__ArchiveEnd:

p_DelVar:
	.db 9
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	jr	c,$+5
	B_CALL(_DelVarArc)

p_GetArc:
	.db __GetArcEnd-1-$
	push	de
	MOV9TOOP1()
	B_CALL(_ChkFindSym)
	jr	c,__GetArcFail
	push	de
	ex	de,hl
	ld	hl,(progPtr)
	sbc	hl,de
	pop	de
	ld	hl,9
	jr	c,__GetArcName
__GetArcStatic:
	ld	l,12
	and	%00011111
	jr	z,__GetArcDone
	cp	l
	jr	z,__GetArcDone
	ld	l,14
	jr	__GetArcDone
__GetArcName:
	add	hl,de
	B_CALL(_LoadDEIndPaged)
	ld	d,0
	inc	e
	inc	e
__GetArcDone:
	add	hl,de
	ex	de,hl
	pop	hl
	ld	(hl),e
	inc	hl
	ld	(hl),d
	inc	hl
	ld	(hl),b
	ex	de,hl
	ret
__GetArcFail:
	ld	hl,0
	pop	de
	ret
__GetArcEnd:

p_ReadArc:
	.db __ReadArcEnd-1-$
	ld	b,a
	ld	a,h
	rlca
	rlca
	dec	a
	and	%00000011
	add	a,b
	ld	b,a
	set	6,h
	res	7,h
	B_CALL(_LoadDEIndPaged)
	ex	de,hl
	ret
__ReadArcEnd:

p_CopyArc:
	.db __CopyArcEnd-1-$
	pop	ix
	pop	de
	ex	(sp),hl
	ld	b,a
	ld	a,h
	rlca
	rlca
	dec	a
	and	%00000011
	add	a,b
	set	6,h
	res	7,h
	pop	bc
	B_CALL(_FlashToRAM)
	jp	(ix)
__CopyArcEnd:

;GrayScale
;-----------------------------------------------
p_GraySteal2:
	.db 6
	push	hl
	pop	ix
	ld	hl,appBackUpScreen-11

p_DispGS:
	.db __DispGSEnd-1-$
	ld	a,i
	push	af
	di
	ld	a,$80
	out	($10),a
	ld	(OP2),sp
	ld	hl,flags+asm_Flag2
	rr	(hl)
	sbc	a,a
	xor	%01010101
	ld	(hl),a
	ld	c,a
	ld	l,appbackupscreen&$ff-1
	ld	sp,plotSScreen-appbackupscreen
__DispGSNext:
	ld	a,l			;4
	ld	b,64			;7
	add	a,$21-(appbackupscreen&$ff);7
	out	($10),a			;11		Into loop: 59 T-states
	inc	l			;4
	ld	h,appbackupscreen>>8	;7
	ld	de,appbackupscreen-plotSScreen+12;11
__DispGSLoop:
	ld	a,(hl)			;7		Loop: 61 T-states
	rrc	c			;8
	and	c			;4
	add	hl,sp			;11
	or	(hl)			;7
	out	($11),a			;11
	add	hl,de			;11
	djnz	__DispGSLoop		;13/8		Next Loop: 60 T-states
	ld	a,l			;4
	cp	12+(appbackupscreen&$ff);7
	jr	nz,__DispGSNext		;12
__DispGSDone:
	ld	sp,(OP2)
	pop	af
	ret	po
	ei
	ret
__DispGSEnd:


p_Disp4Lvl:
	.db __Disp4LvlEnd-1-$
	ld	a,i
	push	af
	di
	ld	(OP2+2),sp
	ld	a,$80
	out	($10),a
	ld	sp,appbackupscreen - plotSScreen
	ld	e,(plotSScreen-appbackupscreen+12)&$ff
	ld	c,-$0C
	ex	af,af'
	ld	a,%11011011
	ld	hl,flags+asm_flag2
	inc	(hl)
	jr	z,__Disp4Lvlskip
	add	a,a
	ld	b,(hl)
	inc	b
	jr	z,__Disp4Lvlskip
	rlca
	ld	(hl),-2
__Disp4Lvlskip:
	ld	l,plotSScreen&$ff-1
	ex	af,af'
__Disp4Lvlentry:
	ld	a,c
	add	a,$2C
	ld	h,plotSScreen>>8
	inc	l
	ld	b,64
	out	($10),a
__Disp4Lvlloop:
	ld	a,(hl)
	add	hl,sp
	xor	(hl)
	ex	af,af'
	cp	e
	rra
	ld	d,a
	ex	af,af'
	and	d
	xor	(hl)
	out	($11),a
	ld	d,(plotSScreen-appbackupscreen+12)>>8
	add	hl,de
	djnz	__Disp4Lvlloop
	inc	c
	jr	nz,__Disp4Lvlentry
__Disp4LvlDone:
	ld	sp,(OP2+2)
	pop	af
	ret	po
	ei
	ret
__Disp4LvlEnd:

;Geometry Drawing
;-----------------------------------------------

p_Line:
	.db __LineEnd-$-1
	ld	ix,plotSScreen
	ld	a,l
	pop	hl
	pop	bc
	pop	de
	ex	(sp),hl		;Called with 3 args pushed.  Using call-back optimization.
	cp	64
	ret	nc
	ld	b,a
	ld	a,e
	cp	64
	ret	nc
	ld	d,l
	ld	l,b

	ld	a,d
	cp	96
	ret	nc
	ld	a,c
	cp	96
	ret	nc

	ld	h,a
	sub	d
	jr	nc,__LineSkipRev
	ex	de,hl
	neg
__LineSkipRev:
	push	af			; Saving DX (it will be popped into DE below)
	ld	a,d			; IX+=D/8+E*12 (actually E*4+E*4+E*4)
	rra
	rra
	rra
	and	%00011111
	ld	c,a
	ld	b,0
	add	ix,bc
	ld	a,e
	add	a,a
	add	a,a
	ld	c,a
	add	ix,bc
	add	ix,bc
	add	ix,bc
	ld	a,d			; Calculating the starting pixel mask
	and	%00000111
	inc	a
	ld	b,a
	ld	a,%00000001
__LineMaskLoop:
	rrca
	djnz	__LineMaskLoop
	ld	c,a
	ld	a,l			; Calculating delta Y and negating the Y increment if necessary
	sub	e			; This is the last instruction for which we need the original data
	ld	de,12
	jr	nc,__LineSkipNeg
	ld	de,-12
	neg
__LineSkipNeg:
	pop	hl			; Recalling DX
	ld	l,a			; D=DX, E=DY
	cp	h
	jr	c,__LineHoriz		; Line is rather horizontal than vertical
__LineVert:
	ld	b,l			; Pixel counter
	inc	b
	rra				; nc at this point so (A=E/2)
__LineVLoop:
	push	af
	ld	a,(ix+0)
	or	c			; Writing pixel to current position
	ld	(ix+0),a
	pop	af
	add	ix,de
	sub	h			; Handling gradient
	jr	nc,__LineVNext
	add	a,l
	rrc	c			; Rotating mask
	jr	nc,__LineVNext		; Handling byte boundary
	inc	ix
__LineVNext:
	djnz	__LineVLoop
	ret
__LineHoriz:
	ld	b,h			; Pixel counter
	inc	b
	ld	a,h			; Setting up gradient counter
	srl	a
__LineHLoop:
	push	af			; Saving A
	ld	a,(ix+0)
	or	c			; Writing pixel to current position
	ld	(ix+0),a
	pop	af			; Recalling A
	rrc	c			; Rotating mask
	jr	nc,__LineHSkip		; Handling byte boundary
	inc	ix
__LineHSkip:
	sub	l			; Handling gradient
	jr	nc,__LineHNext
	add	a,h
	add	ix,de
__LineHNext:
	djnz __LineHLoop
	ret
__LineEnd:

p_Box:
	.db __BoxEnd-$-1
	pop	de		;Called with 3 args pushed.  Using call-back optimization.
	ex	de,hl
	pop	bc
	ld	b,e
	pop	de
	ex	(sp),hl		;(e,l) = (X,Y)
	ex	de,hl		;(c,b) = (width,height)

	ld	ix,plotSScreen	;Routine can be hijacked here for back buffer

	ld	a,96		;Clip Top
	sub	e
	ret	c
	ret	z
	cp	c		;Clip Bottom
	jr	nc,$+3
	ld	c,a
	ld	a,64		;Clip Left
	sub	l
	ret	c
	ret	z
	cp	b		;Clip Right
	jr	nc,$+3
	ld	b,a

	xor	a		;More clipping...
	cp	b
	ret	z
	cp	c
	ret	z
	ld	h,a
	ld	d,a

	push	bc
	push	ix
	pop	bc
	ld	a,l
	add	a,a
	add	a,l
	ld	l,a
	add	hl,hl
	add	hl,hl		;(e,_) = (X,Y)
	add	hl,bc		;(_,_) = (width,height)

	ld	a,e
	srl	e
	srl	e
	srl	e
	add	hl,de
	and	%00000111	;(a,_) = (X^8,Y)
	pop	de		;(e,d) = (width,height)

	ld	b,a
	add	a,e
	sub	8
	ld	e,0
	jr	c,__BoxSkip
	ld	e,a
	xor	a
__BoxSkip:

__BoxShift:			;Input:  b = Left shift
	add	a,8		;Input:  a = negative right shift
	sub	b		;Output: a = mask
	ld	c,0
__BoxShift1:
	scf
	rr	c
	dec	a
	jr	nz,__BoxShift1
	ld	a,c
	inc	b
	rlca
__BoxShift2:
	rrca
	djnz	__BoxShift2

__BoxLoop1:			;(e,d) = (width,height)
	push	hl		;    a = bitmask
	ld	b,d
	ld	c,a
	push	de
	ld	de,12
__BoxLoop2:
	ld	a,c
	or	(hl)
	ld	(hl),a
	add	hl,de
	djnz	__BoxLoop2
	pop	de
	pop	hl
	inc	hl
	ld	a,e
	or	a
	ret	z
	sub	8
	ld	e,b
	jr	c,__BoxShift
	ld	e,a
	ld	a,%11111111
	jr	__BoxLoop1
__BoxEnd:

p_BoxSteal:
	.db 8
	pop	bc
	ld	b,l
	pop	hl
	pop	de
	ld	ix,appBackUpScreen

p_BoxInv:
	.db __BoxInvEnd-$-1
	pop	de		;Called with 3 args pushed.  Using call-back optimization.
	ex	de,hl
	pop	bc
	ld	b,e
	pop	de
	ex	(sp),hl		;(e,l) = (X,Y)
	ex	de,hl		;(c,b) = (width,height)

	ld	ix,plotSScreen	;Routine can be hijacked here for back buffer

	ld	a,96		;Clip Top
	sub	e
	ret	c
	ret	z
	cp	c		;Clip Bottom
	jr	nc,$+3
	ld	c,a
	ld	a,64		;Clip Left
	sub	l
	ret	c
	ret	z
	cp	b		;Clip Right
	jr	nc,$+3
	ld	b,a

	xor	a		;More clipping...
	cp	b
	ret	z
	cp	c
	ret	z
	ld	h,a
	ld	d,a

	push	bc
	push	ix
	pop	bc
	ld	a,l
	add	a,a
	add	a,l
	ld	l,a
	add	hl,hl
	add	hl,hl		;(e,_) = (X,Y)
	add	hl,bc		;(_,_) = (width,height)

	ld	a,e
	srl	e
	srl	e
	srl	e
	add	hl,de
	and	%00000111	;(a,_) = (X^8,Y)
	pop	de		;(e,d) = (width,height)

	ld	b,a
	add	a,e
	sub	8
	ld	e,0
	jr	c,__BoxInvSkip
	ld	e,a
	xor	a
__BoxInvSkip:

__BoxInvShift:			;Input:  b = Left shift
	add	a,8		;Input:  a = negative right shift
	sub	b		;Output: a = mask
	ld	c,0
__BoxInvShift1:
	scf
	rr	c
	dec	a
	jr	nz,__BoxInvShift1
	ld	a,c
	inc	b
	rlca
__BoxInvShift2:
	rrca
	djnz	__BoxInvShift2

__BoxInvLoop1:			;(e,d) = (width,height)
	push	hl		;    a = bitmask
	ld	b,d
	ld	c,a
	push	de
	ld	de,12
__BoxInvLoop2:
	ld	a,c
	xor	(hl)
	ld	(hl),a
	add	hl,de
	djnz	__BoxInvLoop2
	pop	de
	pop	hl
	inc	hl
	ld	a,e
	or	a
	ret	z
	sub	8
	ld	e,b
	jr	c,__BoxInvShift
	ld	e,a
	ld	a,%11111111
	jr	__BoxInvLoop1
__BoxInvEnd:

p_Circle:
	.db __CircleEnd-$-1
	ld	e,l
	pop	hl
	pop	bc
	ex	(sp),hl
	ld	b,l
	xor	a
	ld	l,a
	sub	e		;(CX,CY) = (b,c)
	ld	d,a		;(er,x,y) = (d,e,l)

__CircleLoop:
	ld	a,8

__CircleDraw8:
	push	af
	and	%00000001
	jr	nz,__CricleSwap
	sub	e
	ld	e,a
	ex	de,hl
__CricleSwap:
	ex	de,hl
	push	de
	push	hl
	push	bc

	ld	a,e
	add	a,b
	ld	e,a
	ld	a,l
	add	a,c
	ld	l,a
	call	$3F00+sub_Pix
	or	(hl)
	ld	(hl),a

	pop	bc
	pop	hl
	pop	de
	pop	af
	dec	a
	jr	nz,__CircleDraw8

	ld	a,d
	add	a,l
	inc	l
	add	a,l
	bit	7,a
	jr	nz,__CircleSkip
	dec	e
	sub	e
	sub	e
__CircleSkip:
	ld	d,a
	ld	a,e
	cp	l
	jr	nc,__CircleLoop
	ret
__CircleEnd:
	.db rp_Ans,28

;Bit
;-----------------------------------------------
p_GetBit:
	.db 12
	ld	a,e
	and	%00000111
	inc	a
	ld	b,a
	xor	a
__GetBitLoop:
	ld	h,a
	add	hl,hl
	djnz	__GetBitLoop
	ld	l,h
	ld	h,b

p_GetBit16:
	.db 12
	ld	a,e
	and	%00001111
	inc	a
	ld	b,a
__GetBit16Loop:
	add	hl,hl
	djnz	__GetBit16Loop
	ld	h,b
	ld	l,b
	rl	l

;Sort
;-----------------------------------------------
p_SortD:	;DE = data     L = size of data
	.db __SortDEnd-1-$
	ld	c,l
	ex	de,hl
__SortDLoop2:
	ld	b,c
	push	hl
	jr	__SortDJumpIn
__SortDLoop1:
	inc	hl
	cp	(hl)
	jr	c,__SortDSkip
__SortDJumpIn:
	ld	a,(hl)
	ld	d,h
	ld	e,l
__SortDSkip:
	djnz	__SortDLoop1
	ld	b,(hl)
	ld	(hl),a
	ld	a,b
	ld	(de),a
	pop	hl
	dec	c
	jr	nz,__SortDLoop2
	ret
__SortDEnd:

;Interrupts
;-----------------------------------------------
p_Halt:
	.db 1
	halt
p_FnOn:
	.db 1
	ei
p_FnOff:
	.db 1
	di
p_IntOff:
	.db 2
	im 1

p_IntSetup:
	.db __IntEnd-p_IntSetup-1
	di
	ld	de,$8B01
	ld	a,d
	ld	i,a
	ld	a,l
	ld	hl,$8B00
	ld	b,e
	ld	c,l
	ld	(hl),$8A
	ldir

	and	%00000110
	out	(4),a
	ld	a,%00001000
	out	(3),a
	ld	a,%00001010
	out	(3),a

	ld	de,$8A8A
	ld	bc,__IntDataEnd-__IntData
	ld	hl,$0000
	ldir

	in	a,(6)
	ld	($8A8A+__IntDataSMC-__IntData+1),a
__IntEnd:
	.db rp_Ans,9

p_IntPart2:
	.db __IntPart2End-p_IntPart2-1
	ld	hl,$0000
	ld	($8A8A+__IntDataRep-__IntData+1),hl
	im	2
	ei
__IntPart2End:
	.db rp_Cust,8

;Special interrupt added as data, not routine.
;-----------------------------------------------
__IntData:
	di
	push	af
	exx
	push	ix
	in	a,(6)
	push	af
__IntDataSMC:
	ld	a,$00
	out	(6),a
__IntDataRep:
	call	$0000
	pop	af
	out	(6),a
	ld	a,%00001000
	out	(3),a
	ld	a,%00001010
	out	(3),a
	pop	ix
	exx
	pop	af
	ei
	ret
__IntDataEnd:

;Ans
;-----------------------------------------------
p_StoreAns:
	.db 9
	B_CALL(_SetXXXXOP2)
	B_CALL(_OP2toOP1)
	B_CALL(_StoAns)
p_RecalAns:
	.db 7
	B_CALL(_RclAns)
	B_CALL(_ConvOP1)
	ex	de,hl

;Specialty Drawing
;----------------------------------------------------------------------------------

p_DrawMsk:
	.db __DrawMskEnd-1-$
	push	hl
	pop	ix			;Input hl = Sprite
	ld	b,7			;Input c = Sprite X Position
	ld	d,0			;Input e = Sprite Y Position
	ld	a,c
	add	a,b
	jr	c,__DrawMskClipLeft
	sub	96+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawMskNoClipH
__DrawMskClipRight:
	inc	d
	jr	__DrawMskClipHDone
__DrawMskClipLeft:
	add	a,89
	ld	c,a
__DrawMskClipHDone:
	inc	d		;d,c,e are updated
__DrawMskNoClipH:
	ld	a,e
	add	a,b
	jr	c,__DrawMskClipTop
	sub	64+7
	ret	nc
	cpl
	cp	b
	jr	nc,__DrawMskNoClipV
	jr	__DrawMskClipBottom
__DrawMskClipTop:
	inc	ix
	inc	e
	jr	nz,__DrawMskClipTop
__DrawMskClipBottom:
	ld	b,a
__DrawMskNoClipV:			;b,ix,e are updated.
	dec	d
	jr	z,__DrawMskNoFix
	inc	e
__DrawMskNoFix:
	push	de
	ld	d,0
	ld	h,d
	ld	l,e
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	e,c
	ld	a,e
	srl	e
	srl	e
	srl	e
 	add	hl,de
	ld	de,plotSScreen-11
	add	hl,de
	pop	de
	inc	b
 	and	%00000111
	jr	z,__DrawMskAligned
	ld	c,a
__DrawMskLoop:
	push	bc

	push	hl

	ld	b,c
	ld	e,(ix+0)
	xor	a
	ld	h,a
	ld	c,(ix+8)
__DrawMskShift:
	srl	e
	rr	h
	srl	c
	rra
	djnz	__DrawMskShift

	ld	b,h
	pop	hl
	push	af

	dec	d
	jr	z,__DrawMskSkipRight1

	push	bc
	xor	b
	cpl
	ld	c,a

	ld	a,(hl)
	or	b
	and	c
	ld	(hl),a
	pop	bc

__DrawMskSkipRight1:
	dec	hl
	inc	d
	push	de
	jr	z,__DrawMskSkipLeft1

	ld	a,c
	xor	e
	cpl
	ld	d,a

	ld	a,(hl)
	or	e
	and	d
	ld	(hl),a

__DrawMskSkipLeft1:
	ld	de,appBackUpScreen-plotSScreen+1
	add	hl,de
	pop	de
	pop	af
	dec	d
	jr	z,__DrawMskSkipRight2

	or	b
	cpl

	and	(hl)
	or	b
	ld	(hl),a

__DrawMskSkipRight2:
	dec	hl
	inc	d
	jr	z,__DrawMskSkipLeft2

	ld	a,c
	or	e
	cpl

	and	(hl)
	or	e
	ld	(hl),a

__DrawMskSkipLeft2:
	ld	a,d
	ld	de,plotSScreen-appBackUpScreen+13
	add	hl,de
	ld	d,a

	inc	ix
	pop	bc
	djnz	__DrawMskLoop
	ret
__DrawMskAligned:
	dec	hl
__DrawMskAlignedLoop:
	push	hl
	ld	de,appBackUpScreen-plotSScreen
	add	hl,de

	ld	a,(ix+0)
	ld	d,a
	xor	(ix+8)
	cpl
	ld	e,a

	and	(hl)
	or	d
	ld	(hl),a

	pop	hl

	ld	a,(hl)
	or	d
	and	e
	ld	(hl),a

	inc	ix
	ld	de,12
	add	hl,de
	djnz	__DrawMskAlignedLoop
__DrawMskEnd:


;Sprite Flipping
;-----------------------------------------------
p_FlipV:
	.db __FlipVEnd-1-$
	ex	de,hl
	ld	hl,vx_SptBuff+8
	ld	b,8
__FlipVLoop:
	dec	l
	ld	a,(de)
	ld	(hl),a
	inc	de
	djnz	__FlipVLoop
	ret
__FlipVEnd:

p_FlipH:
	.db __FlipHEnd-1-$
	ld	de,vx_SptBuff
	push	de
	ld	b,8
__FlipHLoop1:
	ld	c,(hl)
	ld	a,1
__FlipHLoop2:
	rr	c
	rla
	jr	nc,__FlipHLoop2
	ld	(de),a
	inc	l
	inc	de
	djnz	__FlipHLoop1
	pop	hl
	ret
__FlipHEnd:

p_RotC:
	.db __RotCEnd-1-$
	ex	de,hl
	ld	c,8
__RotCLoop1:
	ld	hl,vx_SptBuff+8
	ld	b,8
	ld	a,(de)
__RotCLoop2:
	dec	l
	rra
	rr	(hl)
	djnz	__RotCLoop2
	inc	de
	dec	c
	jr	nz,__RotCLoop1
	ret
__RotCEnd:

p_RotCC:
	.db __RotCCEnd-1-$
	ex	de,hl
	ld	c,8
__RotCCLoop1:
	ld	hl,vx_SptBuff+8
	ld	b,8
	ld	a,(de)
__RotCCLoop2:
	dec	l
	rla
	rl	(hl)
	djnz	__RotCCLoop2
	inc	de
	dec	c
	jr	nz,__RotCCLoop1
	ret
__RotCCEnd:

;Floating point conversions
;-----------------------------------------------
p_FtoD:
	.db 5
	MOV9TOOP1()
	B_CALL(_ConvOP1)
	ex	de,hl

p_DtoF:
	.db 13
	ex	(sp),hl
	B_CALL(_SetXXXXOP2)
	ld	hl,OP2
	pop	de
	ld	bc,9
	ldir