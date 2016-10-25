directInput_Wait:
	call	directInput
	or	a
	ret	nz
	jr	directInput_Wait
directInput:
	ld	bc,07BFh
	ld	d,0
directInput_GroupLoop:
	ld	a,0FFh
	out	(1),a
	ld	a,c
	out	(1),a
	in	a,(1)
	cp	0FFh
	jr	z,directInput_NoKey
	ld	d,8
	ld	e,01111111b
directInput_KeyLoop:
	cp	e
	jr	z,directInput_Ret
	rrc	e
	dec	d
	jr	nz,directInput_KeyLoop
directInput_NoKey:
	rrc	c
	djnz	directInput_GroupLoop
directInput_Ret:
	ld	a,b	;b=Group number (1=$FE ... 7=$BF)
	rlca
	rlca
	rlca
	rlca
	or	d	;d=Key Number	(1=254 ... 8=127)
	ld	hl,diLastKey
	cp	(hl)
	ld	(hl),a
	ret	nz		;exit if key is new
	xor	a		;if repeating, set to zero
	ret

datLetters:
	db	diA,diB,diC,diD,diE,diF,diG,diH,diI,diJ,diK,diL,diM
	db	diN,diO,diP,diQ,diR,diS,diT,diU,diV,diW,diX,diY,diZ

diLeft		equ	12h	;group 1, key 2
diRight		equ	13h
diEnter		equ	21h
diPlus		equ	22h
diMinus		equ	23h
diW		equ	23h
diR		equ	24h
diM		equ	25h
diH		equ	26h
diClear		equ	27h
diV		equ	33h
diQ		equ	34h
diL		equ	35h
diG		equ	36h
diZ		equ	42h
diU		equ	43h
diP		equ	44h
diK		equ	45h
diF		equ	46h
diC		equ	47h
diY		equ	52h
diT		equ	53h
diO		equ	54h
diJ		equ	55h
diE		equ	56h
diB		equ	57h
diX		equ	62h
diS		equ	63h
diN		equ	64h
diI		equ	65h
diD		equ	66h
diA		equ	67h
di2nd		equ	76h
diMode		equ	77h
diDel		equ	78h