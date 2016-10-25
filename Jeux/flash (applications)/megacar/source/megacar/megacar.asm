
;Include file needed to access system routines
	nolist
	include "ti83plus.inc"
	list

GameFlags	equ	asm_Flag2
FinishedRace	equ	0
FlashScreen	equ	1
NewScores	equ	2


FlashTemp	equ	appBackUpScreen

EXT_APP equ 1   ;This definition is required of all apps
cseg            ;This linker directive is required of all apps.

;This is the application header definition area required for all apps.
	db	080h,0Fh		;Field: Program length
	db	00h,00h,00h,00h		;Length=0 (N/A for unsigned apps)
	db	080h,012h		;Field: Program type
	db	01h,04h			;Type= Shareware, TI-83Plus
	db	080h,021h		;Field: App ID
	db	01h			;Id = 1
	db	080h,031h		;Field: App Build
	db	01h			;Build = 1
	db	080h,048h		;Field: App Name
	db	"MegaCar "		;Name = "Megacar " must be 8 characters
	db	080h,081h		;Field: App Pages
	db	01h			;App Pages = 1
	db	080h,090h		;No default splash screen
	db	03h,026h,09h,04h,04h,06fh,01bh,80h	;Field: Date stamp- 5/12/1999
	db	02h,0dh,040h										;Dummy encrypted TI date stamp signature
	db	0a1h,06bh,099h,0f6h,059h,0bch,067h
	db	0f5h,085h,09ch,09h,06ch,0fh,0b4h,03h,09bh,0c9h
	db	03h,032h,02ch,0e0h,03h,020h,0e3h,02ch,0f4h,02dh
	db	73h,0b4h,027h,0c4h,0a0h,072h,054h,0b9h,0eah,07ch
	db	03bh,0aah,016h,0f6h,077h,083h,07ah,0eeh,01ah,0d4h
	db	042h,04ch,06bh,08bh,013h,01fh,0bbh,093h,08bh,0fch
	db	019h,01ch,03ch,0ech,04dh,0e5h,075h
	db	80h,7Fh			;Field: Program Image length
	db	0,0,0,0			;Length=0, N/A
	db	0,0,0,0			;Reserved
	db	0,0,0,0			;Reserved
	db	0,0,0,0			;Reserved
	db	0,0,0,0			;Reserved
 
;------------------------------------------------------------------------------------------------------------

Start:
	di
    	call	directInput			;reset key state
	set	textWrite,(iy+sGrFlags)		;write to graph buffer

drawTitleScreen:
	B_CALL	GrBufClr
	ld	hl,datTitleScreen
	ld	de,plotSScreen+(12*5)
	ld	bc,32*12
	ldir

	set	fracDrawLFont,(iy+fontFlags)	;Large Font
	ld	hl,txtAuthors
	ld	de,41*256+6
	call	putStrFlash
	ld	de,49*256+24
	call	putStrFlash
	res	fracDrawLFont,(iy+fontFlags)	;Small Font
	set	textInverse,(iy+textFlags)	;inverse text
	ld	de,25*256+74
	call	putStrFlash

	ld	hl,plotSScreen
	push	hl
	ld	de,appBackUpScreen
	push	de
	ld	bc,768
	ldir

	B_CALL	GrBufClr

	pop	hl	;hl = appBackUpScreen
	pop	de	;de = plotSScreen
	ld	b,8
drawTitleScreen_ScanLoop1:
	push	bc
	ld	b,8
drawTitleScreen_ScanLoop2:
	push	bc
	ld	bc,12
	ldir
	ld	c,7*12
	add	hl,bc
	ex	de,hl
	add	hl,bc
	ex	de,hl
	pop	bc
	djnz	drawTitleScreen_ScanLoop2
	ld	bc,12-768
	add	hl,bc
	ex	de,hl
	add	hl,bc
	ex	de,hl
	pop	bc
	exx
	call	ionFastCopy
	ld	bc,800
drawTitleScreen_KeyDelayLoop:
	push	bc
	call	directInput
	pop	bc
	or	a
	jr	nz,levelSelect_Init
	cpi
	jp	pe,drawTitleScreen_KeyDelayLoop
	exx
	djnz	drawTitleScreen_ScanLoop1
	call	directInput_Wait

levelSelect_Init:
	ld	hl,shiftLevel_InitData
	ld	de,shiftLevel_WrapVal
	ld	bc,5
	ldir

	xor	a
	ld	(iy+GameFlags),a

	ld	hl,OP1
	ld	(hl),ProtProgObj
	inc	hl
	ld	(hl),a
	ld	(LevelName+8),a
	call	shiftLevel
	jr	c,noLevelsFound
	
levelSelect_Display:
	call	initLevelData
	call	displayField
	call	drawPanel

	ld	hl,plotSScreen+(12*11)
	ld	c,28
	call	drawWhiteBox

	res	textInverse,(iy+textFlags)
	set	fracDrawLFont,(iy+fontFlags)	;Large Font
	ld	hl,OP1+1
	ld	de,LevelName
	push	de
	ld	bc,8
	ldir
	ld	hl,15*256+12
	ld	(penCol),hl
	pop	hl
	B_CALL	VPutS
	res	fracDrawLFont,(iy+fontFlags)	;Small Font
	ld	de,24*256+12
	ld	hl,txtBestRace
	call	putStrFlash
	push	hl
	ld	hl,(lvlBestRace)
	ld	c,63
	call	dispHLatC
	pop	hl
	ld	de,31*256+12
	call	putStrFlash
	ld	hl,(lvlBestLap)
	ld	c,63
	call	dispHLatC
	ld	hl,txtBy
	push	hl
	ld	de,24*256+64
	call	putStrFlash
	ld	hl,lvlBestRaceInitials
	B_CALL	VPutS
	pop	hl
	ld	de,31*256+64
	call	putStrFlash
	ld	hl,lvlBestLapInitials
	B_CALL	VPutS

	set	textInverse,(iy+textFlags)
	call	ionFastCopy

levelSelect_KeyCheck:
	call	directInput_Wait
	cp	diLeft
	jr	z,levelSelect_Down
	cp	diRight
	jr	z,levelSelect_Up
	cp	diMode
	jr	z,exitGame
	cp	di2nd
	jr	z,startLevel
	jr	levelSelect_KeyCheck
levelSelect_Down:
	ld	de,0FE00h + <_FindAlphaDn
	jr	$F
levelSelect_Up:
	ld	de,00h + <_FindAlphaUp
$$	ld	hl,shiftLevel_WrapVal
	ld	(hl),d
	inc	hl
	inc	hl		;hl -> LSB of B_CALL
	ld	(hl),e
	call	writeNewScores
	call	shiftLevel
	jr	levelSelect_Display

noLevelsFound:
	B_CALL	GrBufClr
	res	textInverse,(iy+textFlags)
	ld	hl,txtNoLevels
	ld	de,29*256+20
	call	putStrFlash
	call	ionFastCopy
	call	directInput_Wait
	
exitGame:
	call	writeNewScores
	res	textWrite,(iy+sGrFlags)
	ei
	B_CALL	GetCSC		;reset key state
	B_JUMP	JForceCmdNoChar
	
startLevel:
	ld	hl,Speed
	ld	bc,7
	B_CALL	MemClear
	ld	(iy+GameFlags),a
	
	ld	hl,999
	ld	(BestTime),hl

	call	incLapNum

mainLoop:
	ld	bc,1800	;delay
	cpi
	jp	pe,$-2

	call	displayField
	call	updateSpeed

	bit	FlashScreen,(iy+GameFlags)
	call	nz,invertField

	call	ionFastCopy

	call	moveCar
	call	checkKeys
	
	bit	FinishedRace,(iy+GameFlags)
	jr	z,$F

	ld	a,(Speed)
	or	a
	jr	nz,mainLoop
	jp	finishedRace		;race over. car stopped. go check for high score.

$$	call	incTime
	call	checkPosts
	jr	mainLoop

;--------------------------------------------------
displayField:
    	ld	de,(Max_Disp_Y)		;maximum y coord to display at
    	ld	hl,(CarY)	;hl = car y coordinate (pixels)
	ld	bc,-22
	add	hl,bc		;subtract 20 because car is drawn at row 22
	jr	c,$F
	ld	hl,0		;if drawing above top, set to the top
$$	call	CpHLDE
	jr	nc,$F
	ex	de,hl		;if drawing is not past the bottom, then don't move it up
$$	push	de		;de = y coord to display at
	ld	a,e		;a' = lsb of y coord
	ex	af,af'

	srl	d
	rr	e
	srl	d
	rr	e
	srl	d
	rr	e
	ld	h,d
	ld	l,e
	call	multByLvlWidth	;divide by 8, multiply by row width
	push	hl		;save row offset

	ld	de,(Max_Disp_X)		;maximum x coordinate to display at
	ld	hl,(CarX)	;hl = car's x coordinate (pixels)
	ld	c,-44
	add	hl,bc		;subtract 60 because car is displayed at column 44
	jr	c,$F
	ld	hl,0		;if negative, then zero
$$	call	CpHLDE
	jr	c,$F
	ex	de,hl		;if displaying past maximum x, then set to maximum x
$$	pop	de		;de = row offset
	push	hl		;save x coordinate

	ld	a,l		;a = lsb of X coordinate
	and	7		;a = (x coord) mod 8
	add	a,a		;*2
	add	a,>rotateTable	;go to rotate table for correct bit number
	push	af

	srl	h
	rr	l
	srl	h
	rr	l
	srl	h
	rr	l		;x coordinate / 8
	add	hl,de           ;+ row offset  =  offset in level
	ld	de,(tempMem)
	add	hl,de		;hl -> starting tilemap byte
	ld	de,Tilemap_Temp		;part of text shadow... temp memory
	ld	(Tilemap_Ptr),de

	ld	a,(Row_Offset)
	ld	c,a		;c = amount to add to get from the end of one row to the beginning of the next
	ld	a,8		;copy enough rows to fill the screen, plus some extra
	ld	b,0		;b=0
displayField_CopyTilesLoop:
	push	bc
	ld	c,13		;copy display width + 1
	ldir
	pop	bc
	add	hl,bc		;go to next row
	dec	a
	jr	nz,displayField_CopyTilesLoop

	ld	bc,plotSScreen		;video memory

	pop	hl		
	ld	d,h		;d = msb of lower rotate byte
	inc	h		;h = msb of upper rotate byte
	exx

	ex	af,af'		;a = lsb of lsb of Y coord
	and	7		;y offset within tile
	ld	b,a
	add	a,>spriteTiles
	ld	d,a                  ;de->sprites
	ld	a,8
	sub	b
	ld	b,a                  ;how many rows in the current tile left

	ld	c,51                 ;number of rows on screen

displayField_TransferRow:
	ld	hl,(Tilemap_Ptr)	;hl -> current tiles
	ld	e,(hl)
	inc	l
	ld	a,(de)
	exx
	ld	e,a
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	bc		;next screen byte (this one could land on a 256-byte boundary)
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	bc		;next screen byte (this one could land on a 256-byte boundary)
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)		;e = tile
	inc	l		;next tile
	ld	a,(de)		;a = sprite
	exx
	ld	l,a		;l = byte from sprite
	ld	a,(de)		;a = lower rotate byte - last sprite
	or	(hl)		;or (upper rotate byte) - current sprite
	ld	(bc),a		;write new byte to grbuf
	ld	e,l		;set next byte to current
	inc	c		;next screen byte
	exx

	ld	e,(hl)
	ld	a,(de)
	exx
	ld	l,a
	ld	a,(de)
	or	(hl)
	ld	(bc),a
	inc	bc		;next screen byte (this one could land on a 256-byte boundary)
	exx

	dec	c
	jr	z,displayField_DrawCar

	inc	d           ;next sprite row
	dec	b
	jp	nz,displayField_TransferRow
	ld	b,8
	ld	d,>spriteTiles
	inc	l
	ld	(Tilemap_Ptr),hl
	jp	displayField_TransferRow+3

displayField_DrawCar:
	ld	hl,(CarX)
	pop	de
	sbc	hl,de
	ld	b,l
	
	ld	hl,(CarY)
	pop	de
	sbc	hl,de
	ld	c,l
	
	ld	(Car_XY),bc

	ld	h,>carSprites
	ld	a,(Angle)
	sub	8
	and	01110000b
	scf
	rra
	ld	l,a

	push	hl
	pop	ix

	ld	d,0
	ld	e,c
	ld	h,d
	ld	l,e
	add	hl,hl
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	a,b
	and	11111000b
	rrca
	rrca
	rrca
	ld	e,a
	add	hl,de
	ld	de,plotSScreen
	add	hl,de
	ld	a,b
	and	00000111b
	ld	c,a
	ld	b,8
displayField_DrawCar_SpriteRowLoop:
	ld	d,(ix)
	ld	e,0
	ld	a,c
	or	a
	jr	z,displayField_DrawCar_SpriteRotLoop0
displayField_DrawCar_SpriteRotLoop:
	srl	d
	rr	e
	dec	a
	jr	nz,displayField_DrawCar_SpriteRotLoop
displayField_DrawCar_SpriteRotLoop0:
	ld	a,(hl)
	xor	d
	ld	(hl),a
	inc	hl
	ld	a,(hl)
	xor	e
	ld	(hl),a
	ld	de,11
	add	hl,de
	inc	ix
	djnz	displayField_DrawCar_SpriteRowLoop
	ret

;-------------------------------------------------

finishedRace:
	call	directInput	;reset last key
	res	FinishedRace,(iy+GameFlags)

	xor	a
	
	ld	hl,(BestTime)
	ld	de,(lvlBestLap)
	call	CpHLDE
	rla
	ld	hl,(RaceTime)
	ld	de,(lvlBestRace)
	call	CpHLDE
	adc	a,a	;rotate left	;bit 0 = best race, bit 1 = best lap
	jr	z,levelSelect_Display	;no new scores

	ex	af,af'

	call	drawWhiteBox_Small
	res	textInverse,(iy+textFlags)
	set	fracDrawLFont,(iy+fontFlags)	;Large Font
	ld	hl,txtInitials
	ld	de,22*256+12
	call	putStrFlash

finishedRace_Clear:
	ld	hl,IntBuffer
	ld	(hl),0
	
finishedRace_GetKey:
	push	hl
	ld	de,22*256+67
	push	de
	ld	hl,txtIntSpace	;erase existing text
	call	putStrFlash
	pop	hl
	ld	(penCol),hl
	ld	hl,IntBuffer
	B_CALL	VPutS
	call	ionFastCopy
	call	directInput_Wait
	pop	hl

	cp	diDel
	jr	z,finishedRace_Del
	cp	diClear
	jr	z,finishedRace_Clear
	cp	diMode
	jr	z,finishedRace_Done
	cp	diEnter
	jr	z,finishedRace_Confirm

finishedRace_Letter:
	ld	de,IntBuffer+3
	call	CpHLDE
	jr	z,finishedRace_GetKey

	push	hl
	ld	hl,datLetters+25
	ld	bc,26
	cpdr
	pop	hl
	jr	nz,finishedRace_GetKey
	ld	a,c
	add	a,'A'
	ld	(hl),a
	inc	hl
	ld	(hl),b	;0
	jr	finishedRace_GetKey

finishedRace_Del:
	ld	de,IntBuffer
	call	CpHLDE
	jr	z,finishedRace_GetKey
	dec	hl
	ld	(hl),0
	jr	finishedRace_GetKey

finishedRace_Confirm:
	set	NewScores,(iy+GameFlags)	;update level file later
	ld	b,0
	ex	af,af'		;a = new score state
	rra	;carry = best race
	jr	nc,finishedRace_Confirm_NotBestRace
	ld	hl,(RaceTime)
	ld	(lvlBestRace),hl
	ld	hl,IntBuffer
	ld	de,lvlBestRaceInitials
	ld	c,4
	ldir
finishedRace_Confirm_NotBestRace:
	rra	;carry = best lap
	jr	nc,finishedRace_Confirm_NotBestLap
	ld	hl,(BestTime)
	ld	(lvlBestLap),hl
	ld	hl,IntBuffer
	ld	de,lvlBestLapInitials
	ld	c,4
	ldir
finishedRace_Confirm_NotBestLap:
finishedRace_Done:
	set	textInverse,(iy+textFlags)
	res	fracDrawLFont,(iy+fontFlags)	;small font
	jp	levelSelect_Display
;--------------------------------------------------
writeNewScores:
	bit	NewScores,(iy+GameFlags)	;exit if scores are the same
	ret	z
	res	NewScores,(iy+GameFlags)
	B_CALL	ChkFindSym
	ld	a,b
	or	a
	jr	nz,writeNewScores_Arc
writeNewScores_CopyHeader:
	ld	hl,6
	add	hl,de
	ex	de,hl
	ld	hl,LevelHeader
	ld	bc,12	;copy high scores and initials
	ldir
	ret
writeNewScores_Arc:
	ld	hl,writeNewScores_Error	
	call	APP_PUSH_ERRORH		;go here if user denies a garbage collect
	rst	rPUSHREALO1
	B_CALL	Arc_Unarc		;unarchive
	B_CALL	PopRealO1
	B_CALL	ChkFindSym
	call	writeNewScores_CopyHeader
	rst	rPUSHREALO1
	B_CALL	Arc_Unarc		;archive
	B_CALL	PopRealO1
	call	APP_POP_ERRORH		;disable error trapper
writeNewScores_Error:
	di				;archiving turns the interrupts on (?)
	ret

;--------------------------------------------------

initLevelData:
	ld	hl,lvlCarX	;copy from level header
	ld	de,CarX_Frac	;to 3-byte fractional coordinate
	xor	a
	ld	(de),a		;fractional part of x = 0
	inc	de
	ldi			;copy x-lsb
	ldi			;copy x-msb
	ld	(de),a		;fractional part of y = 0
	inc	de
	ldi			;copy y-lsb
	ld	a,(hl)		
	ld	(de),a		;copy y-msb (this is faster than LDI)

	ld	hl,(lvlHeight)
	ld	h,0		;l = height (0-255)
	add	hl,hl
	add	hl,hl
	add	hl,hl		;hl = height in pixels
	ld	de,-8
	add	hl,de		;maximum car height = (height - car height)
	ld	(Max_Car_Y),hl
	ld	e,-43
	add	hl,de		;hl = height - 51
	ld	(Max_Disp_Y),hl	;don't set level y coord past this

	ld	a,(lvlWidth)
	ld	h,0
	ld	l,a		;l = width (tiles)
	sub	13
	ld	(Row_Offset),a	;row_offset = width - 11 tiles. add this amount to go to the next row
	ld	a,l

	add	hl,hl
	add	hl,hl
	add	hl,hl		;hl = width (pixels)
	ld	e,-8
	add	hl,de
	ld	(Max_Car_X),hl	;max_width = width - car width
	ld	e,-88		;hl = width - 96
	add	hl,de
	ld	(Max_Disp_X),hl	;maximum display x coord

	ld	b,8		;a = width in tiles
initLevelData_CreateMult_Loop1:
	dec	b
	rla
	jr	nc,initLevelData_CreateMult_Loop1
	ld	hl,multByLvlWidth	;write code to here
initLevelData_CreateMult_Loop2:
	ld	(hl),29h		;add hl,hl
	inc	hl
	rla
	jr	nc,$F
	ld	(hl),19h		;add hl,de
	inc	hl
$$	djnz	initLevelData_CreateMult_Loop2
	ld	(hl),0c9h		;ret

	ld	hl,lvlPostCoords
	ld	de,Posts
	ld	c,16
	ldir

	ld	a,(lvlAngle)
	ld	(Angle),a		;copy angle

	jp	displayPosts

;---------------------------------------------
drawWhiteBox_Small:
	ld	hl,plotSScreen+(12*18)
	ld	c,13
drawWhiteBox:
	;hl -> first row of plotSScreen to use
	;c = number of rows inside
	call	drawWhiteBox_TopBottom
$$	inc	hl
	ld	(hl),10000000b
	inc	hl
	ld	b,8
	ld	(hl),00000000b
	inc	hl
	djnz	$-1-2
	ld	(hl),00000001b
	inc	hl
	inc	hl
	dec	c
	jr	nz,$B
drawWhiteBox_TopBottom:
	inc	hl
	ld	b,10
$$	ld	(hl),11111111b
	inc	hl
	djnz	$B
	inc	hl
	ret

;-------------------------------------------------

moveCar:
	ld	hl,CarX
	ld	de,Last_X
	ldi
	ldi
	inc	hl
	ldi
	ldi
	ld	b,-1
	call	checkKeys_Accel		;friction
	inc	b	;0
	ld	a,(Angle)
	push	af
	call	getTrig_Cosine
	ex	de,hl
	ld	hl,(CarX_Frac)		;h = lsb, l = frac
	add	hl,de
	ld	a,(CarX+1)		;a = msb
	bit	7,d			;see if cosine is negative
	jr	nz,moveCar_CosNeg
	adc	a,b
	jr	moveCar_MoveX
moveCar_CosNeg:
	ccf
	sbc	a,b
moveCar_MoveX:
	ld	d,a
	ld	e,h
	ld	a,l
	ld	hl,(Max_Car_X)
	sbc	hl,de
	jr	c,$F
	ld	(CarX_Frac),a
	ld	(CarX),de
$$	pop	af
	call	getTrig_Sine
	ex	de,hl
	ld	hl,(CarY_Frac)
	add	hl,de
	ld	a,(CarY+1)
	bit	7,d
	jr	nz,moveCar_SinNeg
	adc	a,b
	jr	moveCar_MoveY
moveCar_SinNeg:
	ccf
	sbc	a,b
moveCar_MoveY:
	ld	d,a
	ld	e,h
	ld	a,l
	ld	hl,(Max_Car_Y)
	sbc	hl,de
	ret	c
	ld	(CarY_Frac),a
	ld	(CarY),de
	ret

;-------------------------

CpHLDE:
	push	hl
	or	a
	sbc	hl,de
	pop	hl
	ret

;----------------------------------------------------------------------------------

drawPanel:
	ld	hl,plotSScreen+(12*51)
	call	drawPanel_TopBottom
	ld	c,11
$$	inc	hl	;speedometer will be drawn in later
	ld	(hl),11011111b
	ld	b,4
	call	drawPanel_FillB
	ld	(hl),11101111b
	ld	b,5
	call	drawPanel_FillB
	dec	c
	jr	nz,$B
	call	drawPanel_TopBottom
	jr	drawPanel_Text
drawPanel_Text:
	ld	hl,txtLapTime
	ld	de,51*256+14
	call	putStrFlash
	ld	de,57*256+14
	call	putStrFlash
	ld	de,51*256+55
	call	putStrFlash
	ld	de,57*256+55
	call	putStrFlash
	ld	de,57*256+82
	call	putStrFlash
	ld	a,(lvlLaps)
	add	a,'0'
	B_CALL	VPutMap
drawSpeedometer:
	ld	hl,plotSScreen+(12*52)
	ld	de,12
	ld	a,10100000b
	ld	b,11
$$	ld	(hl),a
	xor	01000000b
	add	hl,de
	djnz	$B
	ret

drawPanel_TopBottom:
	ld	(hl),01111111b
	inc	hl
	ld	(hl),10001111b
	ld	b,4
	call	drawPanel_FillB
	ld	(hl),11000111b
	ld	b,4
	call	drawPanel_FillB
	ld	(hl),11111110b
	inc	hl
	ret
drawPanel_FillB:
	inc	hl
$$	ld	(hl),11111111b
	inc	hl
	djnz	$B
	ret

;-----------------

updateSpeed:
	call	drawSpeedometer
	ld	hl,(Speed)	;speed / 3 * 4
	ld	h,b	;0
	ld	a,9
	B_CALL	DivHLByA
	ld	d,h
	ld	e,l
	add	hl,hl	;*2
	add	hl,de	;*3
	add	hl,hl	;*6
	add	hl,hl	;*12
	ex	de,hl
	ld	hl,plotSScreen+(12*62)
	sbc	hl,de
	ld	a,(hl)
	or	00011111b
	ld	(hl),a
	ret

;-----------------------------------------------------------

putStrFlash:
	ld	(penCol),de
putStrFlash_NoLoc:
	ld	de,FlashTemp
	push	de
	B_CALL	StrCopy
	inc	hl
	ex	(sp),hl
	B_CALL	VPutS
	pop	hl
	ret

;---------------------- Trig Looker-Upper -----------------------------------------

getTrig_Cosine:
	add	a,64
getTrig_Sine:
	ld	l,a
	res	7,l
	ld	h,>sinTable
	ld	l,(hl)
	rla
	jr	nc,getTrig_NoNeg
	xor	a
	sub	l
	ld	l,a
getTrig_NoNeg:
	ld	a,(Speed)
	and	11110000b
	rrca
	rrca
	rrca
	ld	h,a

signedMult:	;hl=h*l, signed
	ld	a,h
	rla
	jr	nc,$F	;h is positive
	xor	a
	sub	h	;set carry
	ld	h,a	;h = -h
$$	sbc	a,a	;if carry, then a = -1, else a = 0
	ld	b,a
	ld	a,l
	rla
	jr	nc,$F	;l is positive
	xor	a
	sub	l	;set carry
	ld	l,a	;l = -l
$$	ld	d,0
	ld	a,b
	adc	a,d	;nz if signs are different
	push	af

	ld	e,l
	ld	l,d
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de
	add	hl,hl
	jr	nc,$+3
	add	hl,de

	pop	af
	ret	z
	ex	de,hl
	ld	hl,0
	sbc	hl,de
	ret

;----------------------------------------------------------------------------------
incTime:
	ld	hl,IncTimeCounter
	inc	(hl)
	ld	a,(hl)
	and	3
	ret	nz

	ld	a,51
	ld	(penRow),a

	ld	hl,(LapTime)
	ld	de,999
	call	CpHLDE
	ret	z

	inc	hl
	ld	(LapTime),hl
	ld	c,49
	call	dispHLatC
	
	ld	hl,(RaceTime)
	inc	hl
	ld	(RaceTime),hl
	ld	c,94
	call	dispHLatC

	ret

;----------------------------------------------------------------------------------
checkKeys:
	ld	a,0FFh
	out	(1),a
	ld	a,0FEh
	out	(1),a
	in	a,(1)
	rra
	rra
	push	af
	call	nc,checkKeys_TurnLeft
	pop	af
	rra
	call	nc,checkKeys_TurnRight

	ld	a,0FFh
	out	(1),a
	ld	a,0DFh
	out	(1),a
	in	a,(1)
	rla		;Alpha (brake)
	ld	b,-1
	jr	nc,checkKeys_Accel

	bit	FinishedRace,(iy+GameFlags)
	ret	nz	;don't accelerate if race is over

;check if we're off the road
	ld	bc,(Car_XY)
	ld	hl,0404h
	add	hl,bc

	ld	a,h
	ld	h,0
	ld	b,h
	ld	c,l
	add	hl,bc
	add	hl,bc
	add	hl,hl
	add	hl,hl
	ld	de,plotSScreen
	add	hl,de
	ld	c,a
	and	00000111b
	srl	c
	srl	c
	srl	c
	add	hl,bc
	ld	b,a
	inc	b
	ld	a,%00000001
	rrca
	djnz	$-1
	and	(hl)
	
	jr	nz,checkKeys_OnTheRoad		;on road
	ld	a,(Speed)
	cp	30
	ld	b,-1
	jr	nc,checkKeys_Accel		;too fast

checkKeys_OnTheRoad:
	ld	a,0FFh
	out	(1),a
	ld	a,0BFh
	out	(1),a
	in	a,(1)
	rla				;del
	jr	nc,checkKeys_Pause
	rla				;mode
	jr	nc,checkKeys_Exit
	rla				;2nd
	ret	c
	ld	b,2

checkKeys_Accel:
	ld	hl,Speed
	ld	a,(hl)
	add	a,b
	cp	93			;maximum speed = 92
	ret	nc
	ld	(hl),a
	ret

checkKeys_TurnLeft:
	ld	a,-2
	jr	$F
checkKeys_TurnRight:
	ld	a,2
$$	ld	hl,Angle
	add	a,(hl)
	ld	(hl),a
	ret

checkKeys_Exit:
	pop	hl
	ld	hl,9999
	ld	(RaceTime),hl
	jp	finishedRace

checkKeys_Pause:
	call	directInput	;reset
	call	drawWhiteBox_Small
	set	fracDrawLFont,(iy+fontFlags)
	res	textInverse,(iy+textFlags)
	ld	hl,txtPaused
	ld	de,22*256+30
	call	putStrFlash
	res	fracDrawLFont,(iy+fontFlags)	;Small Font
	set	textInverse,(iy+textFlags)	;inverse text
	call	ionFastCopy
checkKeys_Pause_GetKey:
	call	directInput_Wait
	ld	bc,0127h
	cp	diPlus
	jr	z,checkKeys_Pause_Contrast
	ld	bc,0FF00h
	cp	diMinus
	ret	nz
checkKeys_Pause_Contrast:
	;b = +1 / -1
	;c = min or max value
	ld	hl,contrast
	ld	a,(hl)
	cp	c
	jr	z,checkKeys_Pause_GetKey
	add	a,b
	ld	(hl),a
	add	a,18h+0C0h
	out	(10h),a
	jr	checkKeys_Pause_GetKey

;-----------------------------------------------------------------------------

checkPosts:
	ld	hl,Posts	;copy 4 bytes: x1,y1, x2,y2
	ld	de,x1		;and multiply each value by 8
	push	de
	ld	b,4
checkPosts_CopyCoords:
	push	hl
	ld	l,(hl)	;l = lsb
	ld	h,0
	add	hl,hl
	add	hl,hl
	add	hl,hl	;*8
	ld	a,l
	ld	(de),a
	inc	de
	ld	a,h
	ld	(de),a
	inc	de
	pop	hl
	inc	hl
	djnz	checkPosts_CopyCoords

	ld	hl,Last_X
	ld	c,4
	ldir
	ld	hl,CarX
	ldi
	ldi
	inc	hl	;skip over CarY_Frac
	ldi
	ldi

	pop	hl		;hl = x1
	push	hl
	call	scalePostsToZero
	inc	hl
	inc	hl		;hl = y1
	call	scalePostsToZero

	pop	hl		;hl = x1
	ld	b,8
checkPosts_TestRange:
	srl	(hl)		;divide lsb by 2
	inc	hl
	ld	a,(hl)
	or	a
	ret	nz		;exit if msb is not zero (post too far away)
	inc	hl
	djnz	checkPosts_TestRange
	
	ld	hl,(y2)
	ld	a,(x1)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(y1)
	ld	a,(x2)
	ld	h,a
	call	signedMult
	pop	de
	or	a
	sbc	hl,de           ;x2 * y1 - x1 * y2
	push	hl
	
	ld	hl,(y1)
	ld	a,(y2)
	sub	l
	ld	l,a
	ld	a,(x3)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(x2)
	ld	a,(x1)
	sub	l
	ld	l,a
	ld	a,(y3)
	ld	h,a
	call	signedMult
	pop	de
	add	hl,de
	pop	de
	add	hl,de           ;a1 * x3 + b1 * y3 + c1
	push	hl
	push	de
	
	ld	hl,(y1)
	ld	a,(y2)
	sub	l
	ld	l,a
	ld	a,(x4)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(x2)
	ld	a,(x1)
	sub	l
	ld	l,a
	ld	a,(y4)
	ld	h,a
	call	signedMult
	pop	de
	add	hl,de
	pop	de
	add	hl,de		;a1 * x4 + b1 * y4 + c1
	pop	de
	
	ld	a,h
	or	l
	jr	z,$F
	ld	a,d
	or	e
	jr	z,$F
	ld	a,h
	xor	d
	rla             ;check same sign
	ret	nc

$$	ld	hl,(y4)
	ld	a,(x3)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(y3)
	ld	a,(x4)
	ld	h,a
	call	signedMult
	pop	de
	or	a
	sbc	hl,de           ;x4 * y3 - x3 * y4
	push	hl
	
	ld	hl,(y3)
	ld	a,(y4)
	sub	l
	ld	l,a
	ld	a,(x1)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(x4)
	ld	a,(x3)
	sub	l
	ld	l,a
	ld	a,(y1)
	ld	h,a
	call	signedMult
	pop	de
	add	hl,de
	pop	de
	add	hl,de		;a2 * x1 + b2 * y1 + c2
	push	hl
	push	de
	
	ld	hl,(y3)
	ld	a,(y4)
	sub	l
	ld	l,a
	ld	a,(x2)
	ld	h,a
	call	signedMult
	push	hl
	ld	hl,(x4)
	ld	a,(x3)
	sub	l
	ld	l,a
	ld	a,(y2)
	ld	h,a
	call	signedMult
	pop	de
	add	hl,de
	pop	de
	add	hl,de           ;a2 * x2 + b2 * y2 + c2
	pop	de
	
	ld	a,h
	or	l
	jr	z,$F
	ld	a,d
	or	e
	jr	z,$F
	ld	a,h
	xor	d
	rla             ;check same sign
	ret	nc

$$	ld	hl,(y3)
	ld	a,(y4)
	sub	l
	ld	l,a
	ld	a,(x2)
	ld	h,a
	ld	a,(x1)
	sub	h
	ld	h,a
	call	signedMult
	push	hl
	
	ld	hl,(y1)
	ld	a,(y2)
	sub	l
	ld	l,a
	ld	a,(x4)
	ld	h,a
	ld	a,(x3)
	sub	h
	ld	h,a
	call	signedMult
	pop	de
	or	a
	sbc	hl,de
	ret	z               ;collinear

checkPosts_Interception:
	ld	hl,CurrentPost
	ld	a,(hl)
	inc	a
	and	3
	ld	(hl),a
	jr	nz,checkPosts_RotateToNext	;not a new lap

	call	incLapNum			;go to next lap, write to screen

	set	FlashScreen,(iy+GameFlags)	;invert the screen on next update - notification of lap completion

	ld	hl,(BestTime)
	ld	de,(LapTime)
	call	CpHLDE
	jr	c,checkPosts_NotBestLapTime

	push	de
	ld	hl,plotSScreen+(12*58)+4
	call	clearLapTime	;wipe out the numbers from the screen
	pop	hl

	ld	(BestTime),hl
	ld	c,49
	call	dispHLatC

checkPosts_NotBestLapTime:
	bit	FinishedRace,(iy+GameFlags)
	jr	nz,checkPosts_RotateToNext		;don't reset lap time when you win

	ld	hl,plotSScreen+(12*52)+4
	call	clearLapTime
	ld	hl,0
	ld	(LapTime),hl
	ld	a,51
	ld	(penRow),a
	ld	c,49
	call	dispHLatC

checkPosts_RotateToNext:
	ld	hl,Posts
	push	hl
	ld	de,Posts+16
	ld	bc,4
	ldir
	pop	de
	ld	c,16
	ldir

displayPosts:
	ld	ix,Posts
	ld	bc,0202h
displayPosts_Loop:
	ld	d,0
	ld	e,(ix+1)
	ld	h,d
	ld	l,e
	call	multByLvlWidth
	ld	e,(ix+0)
	add	hl,de
	ld	de,(tempMem)
	add	hl,de
	ld	(hl),c
	inc	ix
	inc	ix
	djnz	displayPosts_Loop
	dec	c
	ret	z
	ld	b,6
	jr	displayPosts_Loop

;---------------------------------------------------------------------

incLapNum:
	ld	hl,LapNum
	ld	a,(lvlLaps)
	ld	de,57*256+77
	cp	(hl)
	jr	z,$F

	inc	(hl)
	ld	a,(hl)
	add	a,'0'
	ex	de,hl
	ld	(penCol),hl
	B_CALL	VPutMap
	ret

$$	dec	e
	ld	hl,txtDone
	set	FinishedRace,(iy+GameFlags)	;start slowing down
	jp	putStrFlash

;-----------------------------------------------------------

scalePostsToZero:	;subtract all coords by the minimum one
	ld	a,4
	ld	b,-1	;bc = negative
scalePostsToZero_FindMinLoop:
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	inc	hl
	inc	hl
	ex	de,hl
	push	hl
	or	a
	sbc	hl,bc
	pop	hl
	ex	de,hl
	jr	nc,scalePostsToZero_NotMinimum
	ld	b,d
	ld	c,e
scalePostsToZero_NotMinimum:
	dec	a
	jr	nz,scalePostsToZero_FindMinLoop
	
	ld	a,4
scalePostsToZero_SubtractLoop:
	dec	hl
	dec	hl
	dec	hl
	ld	d,(hl)
	dec	hl
	ld	e,(hl)
	ex	de,hl
	or	a
	sbc	hl,bc
	ex	de,hl
	inc	hl
	ld	(hl),d
	dec	hl
	ld	(hl),e
	dec	a
	jr	nz,scalePostsToZero_SubtractLoop
	ret

;-----------------------------------------------------------------------------

ionFastCopy:
	ld	a,80h				; 7
	out	(10h),a				; 11
	ld	hl,plotSScreen-12-(-(12*64)+1)	; 10
	ld	a,20h				; 7
	ld	c,a				; 4
	inc	hl				; 6 waste
	dec	hl				; 6 waste
ionFastCopyAgain:
	ld	b,64				; 7
	inc	c				; 4
	ld	de,-(12*64)+1			; 10
	out	(10h),a				; 11
	add	hl,de				; 11
	ld	de,10				; 10
ionFastCopyLoop:
	add	hl,de				; 11
	inc	hl				; 6 waste
	inc	hl				; 6 waste
	inc	de				; 6
	ld	a,(hl)				; 7
	out	(11h),a				; 11
	dec	de				; 6
	djnz	ionFastCopyLoop			; 13/8
	ld	a,c				; 4
	cp	2Bh+1				; 7
	jr	nz,ionFastCopyAgain		; 10/1
	ret					; 10

;------------------------------------------------------

invertField:
	res	FlashScreen,(iy+GameFlags)
	ld	hl,plotSScreen
	ld	bc,12*51
$$	ld	a,(hl)
	cpl
	ld	(hl),a
	cpi
	ret	po
	jr	$B

;------------------------------------------------------

dispHLatC:	;c=column, hl=number
	ld	a,c
	sub	4
	ld	c,a
	ld	(penCol),a
	B_CALL	DivHLBy10
	add	a,'0'
	B_CALL	VPutMap
	ld	a,h
	or	l
	jr	nz,dispHLatC
	ret
;------------------------------------------------------

clearLapTime:
	;hl -> first byte of plotsscreen to clear
	;wipes out 5 rows

	ld	de,11
	ld	b,5
$$	ld	a,(hl)
	or	00000111b
	ld	(hl),a
	inc	hl
	ld	(hl),11111111b
	add	hl,de
	djnz	$B
	ret

;------------------------------------------------------------------------------------------------------

shiftLevel_Wrap:
	ld	a,(shiftLevel_WrapVal)
	cp	7		;put a 7 here if this is the first run
	scf
	ret	z
	ld	(OP1+1),a
shiftLevel:
	call	shiftLevel_FindAlpha
	jr	c,shiftLevel_Wrap
	dec	hl
	dec	hl
	dec	hl	;hl->lsb ptr
	ld	e,(hl)
	dec	hl
	ld	d,(hl)
	dec	hl	;hl->page
	ld	a,(hl)
	ex	de,hl
	inc	hl
	inc	hl
	or	a
	jr	nz,shiftLevel_Arc	;run seperate code for archived programs
shiftLevel_Ram:
	ld	de,strDetect
	ld	b,4
$$	ld	a,(de)
	cp	(hl)
	jr	nz,shiftLevel
	inc	hl
	inc	de
	djnz	$B
	ld	de,LevelHeader
	ld	c,36
	ldir				;copy 36-byte header
	push	hl
	call	shiftLevel_MemCheck
	pop	hl
	jr	c,shiftLevel		;skip to next level if not enough RAM
	ld	de,(tempMem)
shiftLevel_Ram_Extract:
	ld	a,(hl)
	inc	hl
	scf
	adc	a,a
	db	06h
shiftLevel_Ram_ExtractLoop:
	add	a,a
	jr	z,shiftLevel_Ram_Extract
	jr	c,shiftLevel_Ram_ExtractChunk
	ldi
	jr	shiftLevel_Ram_ExtractLoop
shiftLevel_Ram_ExtractChunk:
	ld	b,a
	ld	c,1Fh
	ld	a,(hl)
	cp	c
	ret	z
	push	bc 
	inc	hl
	ld	b,(hl)
	inc	hl
	push	hl
	ld	l,b
	ld	b,a
	or	c
	rlca
	rlca
	rlca
	ld	h,a
	ld	a,b
	and	c
	add	a,3
	ld	c,a
	ld	b,0
	add	hl,de
	ldir
	pop	hl
	pop	af
	jr	shiftLevel_Ram_ExtractLoop

;-----------------------------------------------

shiftLevel_Arc:
	push	af		;save page
	push	hl		;save flash offset
	ld	de,FlashTemp
	ld	bc,64
	push	de
	B_CALL	FlashToRam
	pop	hl
	push	hl
	pop	ix
	ld	a,(ix+7)	;9-2
	add	a,10		;skip over symbol table entry and size
	ld	c,a
	add	hl,bc		;hl -> detect string (ram)
	ex	(sp),hl
	add	a,4+36		;skip detect string and header
	ld	c,a
	add	hl,bc		;(sp) -> compressed level data (flash)
	ex	(sp),hl

	ld	de,strDetect
	ld	b,4
$$	ld	a,(de)
	cp	(hl)
	jr	nz,shiftLevel_Arc_SkipLevel	;go here if detect string mismatches
	inc	hl
	inc	de
	djnz	$B

	ld	de,LevelHeader		;hl -> header (in flashtemp)
	ld	c,36
	ldir
	call	shiftLevel_MemCheck
	jr	c,shiftLevel_Arc_SkipLevel

	ld	hl,shiftLevel_Arc_FlashExecStart
	ld	de,FlashTemp
	ld	bc,shiftLevel_Arc_FlashExecEnd-shiftLevel_Arc_FlashExecStart
	ldir

	pop	hl		;hl -> level data
	pop	bc		;b = page
	ld	de,(tempMem)
	jp	FlashTemp

shiftLevel_Arc_SkipLevel:
	pop	hl
	pop	af
	jr	shiftLevel

;-------------------------------------------------

shiftLevel_MemCheck:	;if there's not enough RAM for the level, then Carry is set
	ld	hl,(lvlWidth)	;h = height, l = width
	ld	d,0
	ld	e,l		;e = width
	ld	l,d		;d,l = 0
	ld	b,8
$$	add	hl,hl
	jr	nc,$+3
	add	hl,de
	djnz	$B		;hl = width * height (decompressed level size)
	B_CALL	EnoughMem
	ret

;--------------------------------------------------

shiftLevel_Arc_FlashExecStart:		;this must be copied to FlashTemp before executing!
	in	a,(6)
	push	af
	ld	a,b
	out	(6),a
shiftLevel_Arc_Extract:
	bit	7,h
	call	nz,shiftLevel_Arc_Extract_Page
	ld	a,(hl)
	inc	hl
	scf
	adc	a,a
	db	06h
shiftLevel_Arc_Extract_Loop:
	add	a,a
	jr	z,shiftLevel_Arc_Extract
	jr	c,shiftLevel_Arc_Extract_Chunk
	ldi
	jr	shiftLevel_Arc_Extract_Loop
shiftLevel_Arc_Extract_Chunk:
	ld	b,a
	ld	c,1Fh
	bit	7,h
	call	nz,shiftLevel_Arc_Extract_Page
	ld	a,(hl)
	cp	c
	jr	z,shiftLevel_Arc_Extract_Done
	push	bc
	inc	hl
	bit	7,h
	call	nz,shiftLevel_Arc_Extract_Page
	ld	b,(hl)
	inc	hl
	push	hl
	ld	l,b
	ld	b,a
	or	c
	rlca
	rlca
	rlca
	ld	h,a
	ld	a,b
	and	c
	add	a,3
	ld	c,a
	ld	b,0
	add	hl,de
	ldir
	pop	hl
	pop	af
	jr	shiftLevel_Arc_Extract_Loop
shiftLevel_Arc_Extract_Done:
	pop	af
	out	(6),a
	ret
shiftLevel_Arc_Extract_Page equ $-shiftLevel_Arc_FlashExecStart+FlashTemp
	res	7,h
	set	6,h
	push	af
	in	a,(6)
	inc	a
	out	(6),a
	pop	af
	ret
shiftLevel_Arc_FlashExecEnd:

;-----------------------------------------------------

shiftLevel_InitData:	;this is copied, not executed
	db	7
	B_CALL	FindAlphaUp
	ret

;--------------------------------------- Data and stuff -----------------------------------------------
	align	256
	nolist
	include	"rottable.asm"
	include	"tiles.asm"
	include	"sintable.asm"
	include "carsprites.asm"
	include "text.asm"
	include "directinput.asm"
	include "titlescreen.asm"
	list

	define	SafeRam,ORG=saveSScreen
	segment	SafeRam
LevelHeader:	;36 bytes
lvlBestLap		blkb 2
lvlBestLapInitials	blkb 4
lvlBestRace		blkb 2
lvlBestRaceInitials	blkb 4
lvlWidth		blkb 1
lvlHeight		blkb 1
lvlAngle		blkb 1
lvlLaps			blkb 1
lvlCarX			blkb 2
lvlCarY			blkb 2
lvlPostCoords		blkb 16

Max_Car_X		blkb 2
Max_Car_Y		blkb 2
Max_Disp_X		blkb 2
Max_Disp_Y		blkb 2

CarX_Frac		blkb 1
CarX			blkb 2
CarY_Frac		blkb 1
CarY			blkb 2

Car_XY			blkb 2		;stores x,y coords of car on screen

Last_X			blkb 2
Last_Y			blkb 2

x1			blkb 2
y1			blkb 2	;post 1
x2			blkb 2
y2			blkb 2	;post 2
x3			blkb 2
y3			blkb 2	;last car
x4			blkb 2
y4			blkb 2	;current car

Posts			blkb 20

Row_Offset		blkb 1
Tilemap_Ptr		blkb 2

Speed			blkb 1
LapNum			blkb 1
CurrentPost		blkb 1
RaceTime		blkb 2
LapTime			blkb 2
BestTime		blkb 2
IncTimeCounter		blkb 1

Angle			blkb 1

IntBuffer		blkb 4

diLastKey		blkb 1
LevelName		blkb 9

shiftLevel_WrapVal	blkb 1
shiftLevel_FindAlpha	blkb 4

multByLvlWidth		blkb 17

	align 128
Tilemap_Temp		blkb 13*8



