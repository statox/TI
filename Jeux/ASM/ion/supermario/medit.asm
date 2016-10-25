	.nolist		
	#include "ion.inc"
	#include "keys.inc"
	.list		
#ifdef TI83P		
	.org progstart-2
	.db $BB,$6D

APD_BUF   = $86EC
MUL_HL = $4276

#else		
	.org progstart

APD_BUF   = $8265
MUL_HL = $4382

#endif		
		xor a	
		jr nc,Start
MenuHeader:
 .db "Super Mario Editor v1.2",0

TEXT_MEM = APD_BUF+300
TEXT_MEM2 = APD_BUF+500

LvlNo = TEXT_MEM+5
Top = TEXT_MEM+6

ypos = TEXT_MEM
xpos = TEXT_MEM+1
cursor = TEXT_MEM+2
level = TEXT_MEM+3
map_offset = TEXT_MEM+4
string = TEXT_MEM+5 ;5 bytes

WorldLoc = TEXT_MEM+12
map_length = TEXT_MEM+15
NewVarName = TEXT_MEM+110
LVarName = NewVarName

CurEnemyTable = APD_BUF  ;no more than 100 entries
  ;entry byte #1 = type of enemy
  ;entry byte #2 = column location
  ;entry byte #3 = Y coord

Start:
 ld (INITSP),sp
 ld hl,APD_BUF
 ld bc,767
 call OTH_CLEAR

 ld hl,TEXT_MEM
 ld c,127
 call OTH_CLEAR

 ld a,1
 ld (level),a
 ld a,5
 ld (NewVarName),a
GMenu:
 call HeadMenu
 ld hl,SText
 ld de,$0818
 call DMX
 ld de,$1D11
 call DMX
 ld d,$25
 call DMX
 res 7,(iy+$14)
GMenuLoop:
 call HighLightChoice2
 call ionFastCopy
 call HighLightChoice2
 bcall(_getcsc)
 cp G_DOWN
 jr z,nchoice
 cp G_UP
 jr z,nchoice
 cp G_ENTER
 jr z,selectch
 cp G_CLEAR
 jr nz,GMenuLoop
 ret

nchoice:
 ld a,(selected)
 xor %1
 ld (selected),a
 jr GMenuLoop

SText:
 .db "by Sam Heald",0
 .db "Load World",0
 .db "New World",0

selectch:
 ld a,(selected)
 or a
 jp z,LoadG
NewG:
 call HeadMenu
 ld de,$0902
 ld hl,NameProg
 call DMX
 call ionFastCopy

 ld de,$1100
 ld hl,NewVarName+1
 ld b,8
 call WriteText

 ld de,$1902
 ld hl,NameProg2
 call DMX

 ld de,$2100
 ld hl,APD_BUF+10
 ld b,24
 call WriteText

 ld de,$2902
 ld hl,NameProg3
 call DMX

 ld de,$3100
 ld hl,APD_BUF+35
 ld b,4
 call WriteText

 ld hl,NewVarName
 bcall(_mov9toop1)
 bcall(_chksysvar)
 jp nc,LoadG                   ;return if var already exists

 ld hl,250
 push hl
 ld a,(NewVarName)
 bcall(_memfree)     ;check if there's enough ram
 pop hl
 ret c      

 bcall(_createprog)    ;Create the prog 
 ret c
 ld (hl),6
 inc de
 inc de
 ld a,$AA
 ld (de),a
 inc de
 ld a,$BB
 ld (de),a
 inc de

 ld hl,APD_BUF+10
 ld bc,24
 ldir      ;copy description

 ld hl,APD_BUF+35
 ld bc,4
 ldir      ;copy password

 ld a,1
 ld (de),a  ;1 level by default
 ld (WorldLoc),de
 inc de

 ld b,25
 xor a
clear_lop:
 ld (de),a
 add a,6
 inc de

 ex de,hl
 ld (hl),0
 ex de,hl

 inc de
 djnz clear_lop

 ld h,b
 ld l,b
 ld (oldsize),hl

 call InitMap
 jp RealProg

NameProg:
 .db "Name of Program:",0
NameProg2:
 .db "Description:",0
NameProg3:
 .db "Password (4 lets):",0

HighLightChoice2:
 ld a,(selected)
 ld b,a
 inc b
 ld hl,GRAPH_MEM+254
 ld de,96
multh2:
 add hl,de
 djnz multh2
 ld c,7
keepinverting2:
 ld b,8
invert_loop22:
 ld a,(hl)
 cpl
 ld (hl),a
 inc hl
 djnz invert_loop22
 ld de,4
 add hl,de
 dec c
 ld a,c
 or a
 jr nz,keepinverting2
 ret



RealProg:
MainLoop:
 bcall(_cleargbuf)
 call DrawMap
 call DrawCursor
 call DrawSideBar
 call ionFastCopy

 bcall(_getcsc)
 cp G_ALPHA
 jp z,ChooseBlock
 cp G_2ND
 jp z,SetBlock
 cp G_DEL
 jp z,ClearBlock
 cp G_YEDIT
 jp z,AddEnemy
 cp G_WINDOW
 jp z,CURSORDOWN
 cp G_ZOOM
 jp z,CURSORUP
 cp G_PLUS
 jp z,IncLevel
 cp G_MINUS
 jp z,DecLevel
 cp G_XTO
 jp z,IncreaseLength
 cp G_STAT
 jp z,DecreaseLength
 cp G_MODE
 jp z,OptionsMenu
 cp G_TRACE
 jp z,JumpLeft
 cp G_GRAPH
 jp z,JumpRight
 cp G_UP
 jr z,MoveUp
 cp G_DOWN
 jr z,MoveDown
 cp G_RIGHT
 jr z,MoveRight
 cp G_LEFT
 jr z,MoveLeft
 cp G_CLEAR
 jr nz,MainLoop

 jp SaveLevel

MoveUp:
 ld a,(ypos)
 or a
 jr z,MainLoop
 sub 8
 ld (ypos),a
 jr MainLoop

MoveDown:
 ld a,(ypos)
 cp 56
 jr z,MainLoop
 add a,8
 ld (ypos),a
 jp MainLoop

MoveRight:
 ld a,(xpos)
 cp 64
 jr z,CheckScrollR
 add a,8
 ld (xpos),a
 jp MainLoop

MoveLeft:
 ld a,(xpos)
 or a
 jr z,CheckScrollL
 sub 8
 ld (xpos),a
 jp MainLoop

CheckScrollL:
 ld hl,map_offset
 ld a,(hl)
 or a
 jp z,MainLoop
 dec (hl)
 jp MainLoop

CheckScrollR:
 ld a,(map_offset)
 ld b,a
 ld a,(map_length)
 sub 9
 cp b
 jp z,MainLoop

 ld hl,map_offset
 inc (hl)
 jp MainLoop

SetBlock:
 call FindMapValue
 ld a,(cursor)
 ld (hl),a
 jp MainLoop

ClearBlock:
 call FindMapValue
 ld (hl),0
 jp MainLoop


decEN:
 dec (hl)
 jr AddEloop

incEN:
 inc (hl)
 jr AddEloop

AddEnemy:
 xor a
 ld (selected),a
 set 3,(iy+5)
 set 7,(iy+$14)
 ld de,$1d01
 ld hl,enemytxt
 call DMX
 res 7,(iy+$14)
 res 3,(iy+5)
AddEloop:
;DrawCurE
 ld de,enemies
 ld a,(selected)
 and %1111
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl ;selected x 8
 add hl,de
 ld bc,$311C
 call PutSprite
 call ionFastCopy

 bcall(_getcsc)
 ld hl,selected
 cp G_LEFT
 jr z,decEN
 cp G_RIGHT
 jr z,incEN
 cp G_ENTER
 jr nz,AddEloop
setEN:
 call findcol
 ;A = column value 
 ld hl,CurEnemyTable+1
 ld de,3
 ld b,100
findifnoe:
 ld c,(hl)
 cp c
 jr z,col_already_exists
 add hl,de
 djnz findifnoe

 ld hl,CurEnemyTable
 ld b,100
 ld de,3
findEmpty:
 ld a,(hl)
 or a
 jr z,setnewE
 add hl,de
 djnz findEmpty
 jp MainLoop

col_already_exists:
 dec hl
 ld a,(selected)
 inc a
 and %1111
 jr nz,setnewE
clearE:
 ld (hl),a
 inc hl
 ld (hl),a
 inc hl
 ld (hl),a
 jp MainLoop
 
setnewE:
 ld a,(selected)
 inc a
 and %1111 ;set enemy value
 ld (hl),a
 inc hl
 call findcol
 ld (hl),a
 inc hl
 ld a,(ypos)
 ld (hl),a
 jp MainLoop

CURSORUP:
 ld a,(cursor)
 inc a
 cp 72
 jr nz,setnewC
 xor a
setnewC: 
 ld (cursor),a
 jp MainLoop

CURSORDOWN:
 ld a,(cursor)
 dec a
 cp 255
 call z,setmax
 jr setnewC

setmax:
 ld a,71
 ret

IncreaseLength:
 ld a,(map_length)
 cp 128
 jp z,MainLoop
InsertColumn:
 call findcol

 ld (Imodcol),a

 ld hl,CurMap
 ld de,MapBackup
 or a
 jr z,nocopy1
 ld c,a
 ld b,0
 ldir

nocopy1:
 ld b,7
insertcol:
 push bc

 xor a
 ld (de),a
 inc de

 ld a,(map_length)
 ld c,a
 ld b,0
 ldir

 pop bc
 djnz insertcol

 xor a
 ld (de),a
 inc de
 call findcol ;A = curcol
 ld b,a
 ld a,(map_length)
 sub b
 ld c,a
 ld b,0
 ldir


 ld hl,MapBackup
 ld de,CurMap
 ld bc,1024
 ldir

 ld hl,map_length
 inc (hl)

 ld c,0
Imodcol = $-1

 ld b,100
 ld hl,CurEnemyTable+1
ModELoop:
 ld a,(hl)
 cp c
 jr c,NoIncreaseE
 inc (hl)
NoIncreaseE:
 inc hl
 inc hl
 inc hl
 djnz ModELoop
 jp MainLoop

DecreaseLength
 ld a,(map_length)
 cp 10
 jp z,MainLoop
RemoveColumn:
 call findcol

 ld hl,CurMap
 ld de,MapBackup

 ld (Dmodcol),a
 inc a
 ld c,a
 ld b,0
 ldir
 dec de
 ld b,7
delcol:
 push bc
 ld a,(map_length)
 ld c,a
 ld b,0
 ldir
 dec de
 pop bc
 djnz delcol

 call findcol
 ld b,a

 ld a,(map_length)
 sub b
 ld c,a
 ld b,0
 ldir

 ld c,0
Dmodcol = $-1

 ld b,100
 ld hl,CurEnemyTable+1
ModELoop2:
 ld a,(hl)
 sub c
 jr z,RemoveE2
 jr c,NoDecreaseE
 dec (hl)
NoDecreaseE:
 inc hl
 inc hl
 inc hl
NextE:
 djnz ModELoop2

 ld hl,MapBackup
 ld de,CurMap
 ld bc,1024
 ldir
 ld hl,map_length
 dec (hl)
 jp CheckScrollL

RemoveE2:
 dec hl
 ld (hl),a
 inc hl
 ld (hl),a
 inc hl
 ld (hl),a
 inc hl
 inc hl
 jr NextE


findcol:
 ld a,(xpos)
 srl a
 srl a
 srl a
 ld b,a
 ld a,(map_offset)
 add a,b
 ret

FindMapValue:
 call findcol
 ld e,a
 ld d,0
 ld hl,CurMap
 add hl,de

 ld a,(ypos)
 srl a
 srl a
 srl a
 ld b,a
 inc b
 ld a,(map_length)
 ld e,a
 ld d,0
 sbc hl,de
MultMap:
 add hl,de
 djnz MultMap
 ret

IncLevel:
 ld hl,(WorldLoc)
 ld b,(hl)
 ld a,(level)
 cp b
 jp z,MainLoop

 call SaveLevel

 ld hl,level
 inc (hl)
 call LoadLevel
 jp MainLoop

DecLevel:
 ld a,(level)
 dec a
 jp z,MainLoop

 call SaveLevel

 ld hl,level
 dec (hl)
 call LoadLevel
 jp MainLoop

 


DrawMap:
 bcall(_cleargbuf)
 ld a,(map_offset)
 ld e,a
 ld d,0
 ld hl,CurMap
 add hl,de

 ld c,0
 ld a,8
DrawY:
 push af
 ld b,0
 ld a,9
DrawX:
 push af

 push hl
 ld l,(hl)
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 ld de,bgtiles
 add hl,de

 push bc
 call PutSprite
 pop bc
 pop hl

 inc hl
 ld a,b
 add a,8
 ld b,a
 pop af
 dec a
 jr nz,DrawX

 ld a,(map_length)
 sub 9
 ld e,a
 ld d,0
 add hl,de

 ld a,c
 add a,8
 ld c,a

 pop af
 dec a
 jr nz,DrawY
 ret


DrawCursor:
 ld h,0
 ld a,(cursor)
 ld l,a
 add hl,hl
 add hl,hl
 add hl,hl
 ld de,bgtiles
 add hl,de

 ld bc,(ypos)
 push hl
 call PutSprite
 pop hl
 ld bc,$5800
 jp PutSprite


InitMap:
 bcall(_cleargbuf)
 ld hl,CurEnemyTable
 ld bc,299
 call OTH_CLEAR

 ld hl,CurMap
 ld bc,1023
 call OTH_CLEAR

 ld a,10
 ld (map_length),a

 xor a
 ld (xpos),a
 ld (ypos),a
 ld (map_offset),a
 inc a
 ld (cursor),a
 ret


OptionsMenu:
 call HeadMenu
 xor a
 ld (selected),a

 ld hl,MenuText
 ld de,$0D19
 ld b,5
OptLoop:
 call DMX
 ld a,8
 add a,d
 ld d,a
 djnz OptLoop
 res 7,(iy+$14)
MenuLoop:
 call HighLightChoice
 call ionFastCopy
 call HighLightChoice
 bcall(_getcsc)
 cp G_DOWN
 jr z,dchoice
 cp G_UP
 jr z,uchoice
 cp G_ENTER
 jr z,selectc
 cp G_CLEAR
 jr nz,MenuLoop
 jp MainLoop

dchoice:
 ld a,(selected)
 inc a
 cp 5
 jr nz,setop
 xor a
setop:
 ld (selected),a
 jr MenuLoop

uchoice:
 ld a,(selected)
 sub 1
 jr nc,setop
 ld a,4
 jr setop

selectc:
 ld a,(selected)
 or a
 jp z,AddLevel
 dec a
 jp z,RemoveLevel
 dec a
 jp z,ChangeInfo
 dec a
 jr z,instructions

 ld hl,(oldsize)
 ld a,l
 or h
 jp z,MenuLoop
 ret

instructions:
 bcall(_clrlcdf)
 ld hl,instructiontext
 ld de,0
 ld b,10
WriteInstr:
 call DMX
 ld a,6
 add a,d
 ld d,a
 djnz WriteInstr
 call OTH_PAUSE
 jp OptionsMenu

AddLevel:
 ld hl,(WorldLoc)
 ld a,(hl) ;# of levels
 cp 25
 jp z,MainLoop
 call SaveLevel ;save current level

 ld hl,0
 ld (oldsize),hl

 ld hl,(WorldLoc)
 inc (hl)
 ld a,(hl)
 ld (level),a
 call InitMap
 jp MainLoop

RemoveLevel:
 ld hl,(WorldLoc)
 ld a,(hl) ;# of levels
 dec a
 jp z,MainLoop

 call SaveLevel  ;save current level

 ld hl,(WorldLoc)
 ld a,(hl)  ;A = level to remove
 dec (hl)
 ld (level),a

 call LoadLevel

 ld de,(oldsize)
 ld hl,0
 or a
 sbc hl,de
 ld (changeSIZE),hl
 call UpdateOff

 call LoadLOffset  ;de = offset from MapLoc+51
 ld hl,83
 add hl,de  ;hl = offset from start of program
            ;hl = level data
 ld de,(oldsize)
 ex de,hl
 call DelMem  ;delete it!!!

 ld a,1
 ld (level),a
 call LoadLevel
 jp MainLoop


ChangeInfo:
 call HeadMenu
 call ionFastCopy
 res 7,(iy+$14)
 ld de,$0902
 ld hl,NameProg2
 call DMX

 ld de,$1100
 ld hl,TEXT_MEM2
 ld b,24
 call WriteText

 ld de,$1902
 ld hl,NameProg3
 call DMX

 ld de,$2100
 ld hl,TEXT_MEM2+35
 ld b,4
 call WriteText

 ld de,(WorldLoc)
 ld hl,-28
 add hl,de
 ex de,hl

 ld hl,TEXT_MEM2
 ld bc,24
 ldir      ;copy description

 ld hl,TEXT_MEM2+35
 ld bc,4
 ldir      ;copy password
 jp OptionsMenu

HeadMenu:
 bcall(_cleargbuf)
 set 7,(iy+$14)
 ld de,$000A
 ld hl,MenuHeader
 call DMX

 ld hl,GRAPH_MEM
 ld b,84
invert_loopy:
 ld a,(hl)
 cpl
 ld (hl),a
 inc hl
 djnz invert_loopy
 xor a
 ld (selected),a
 ret


HighLightChoice:
 ld a,0
selected = $-1
 ld b,a
 inc b
 ld hl,GRAPH_MEM+63
 ld de,96
multh:
 add hl,de
 djnz multh

 ld c,7
keepinverting:
 ld b,8
invert_loop2:
 ld a,(hl)
 cpl
 ld (hl),a
 inc hl
 djnz invert_loop2
 ld de,4
 add hl,de
 dec c
 ld a,c
 or a
 jr nz,keepinverting
 ret


OTH_CLEAR:
 ld e,l
 ld d,h
 inc de
 ld (hl),0
 ldir
 ret

DM_A_DECI:
 push hl
 push de
 ld l,a
 ld h,0           ; Display HL in menu style with leading zeros
 ld de,string+5
 xor a
 ld (de),a
RepUnp:
 dec de
 bcall(_divhlby10)
 add a,48
 ld (de),a
 djnz RepUnp
 ex de,hl
 call DMX2
 pop de
 pop hl
 ret



PutSprite:
PutSprClp:      XOR  A
__Change_1:     LD   DE, 8      ; D = 0, E = Height

                OR   C                            ; If C < 0
                JP   M, _PSC_NoBotClp             ; No bottom clip.

                LD   A, $3F                       ; Is C is offscreen?
                SUB  C
                RET  C

__Change_2:     CP   8-1        ; If C + 7 < 64
                JR   NC, _PSC_NoVertClp           ; No vertical clip.
                INC  A
                LD   E, A
                JR   _PSC_NoVertClp               ; Height = 64 - C

_PSC_NoBotClp:
__Change_3:     CP   -(8-1)     ; Is C is offscreen?
                RET  C

                ADD  A, E                         ; Find how many lines
                LD   C, A                         ; to actually draw
                SUB  E

                NEG
                LD   E, A
                ADD  HL, DE                       ; Move HL down
                LD   E, C                         ; by -C lines
                LD   C, D

_PSC_NoVertClp: PUSH HL                           ; IX -> Sprite
                POP  IX

                LD   A, $77                       ; OP code for
                LD   (_PSC_OPchg_1), A            ;   LD   (HL), A
                LD   (_PSC_OPchg_2), A

                XOR  A                            ; Is B > 0?
                OR   B
                JP   M, _PSC_NoRightClp

                CP   89                           ; Is B < 89?
                JR   C, _PSC_ClpDone
                CP   96
                RET  NC

                LD   HL, _PSC_OPchg_1             ; Modify LD to NOP
                JR   _PSC_ClpModify

_PSC_NoRightClp:CP   -7                           ; Is B is offscreen?
                RET  C

                LD   HL, _PSC_OPchg_2             ; Modify LD to NOP
_PSC_ClpModify: LD   (HL), D

_PSC_ClpDone:   LD   B, E                         ; B = number of rows

                LD   H, A
                CPL
                AND  %00000111
                LD   (_PSC_OPchg_4 + 1), A
                XOR  %00000111
                LD   (_PSC_OPchg_3 + 1), A
                LD   E, A
                XOR  H
                LD   HL, MaskTable
                ADD  HL, DE
                LD   E, C
                LD   C, (HL)

                LD   H, D
                LD   L, E
                ADD  HL, DE                       ; HL = Y * 12
                ADD  HL, DE
                ADD  HL, HL
                ADD  HL, HL

                ADD  A, 8
                RRCA
                RRCA
                RRCA

                LD   E, A                         ; HL = Y*12 + X/8
                ADD  HL, DE

                LD   DE, GRAPH_MEM - 1
                ADD  HL, DE

                LD   A, C
                LD   (_PSC_OPchg_6 + 1), A        ; mask
                LD   (_PSC_OPchg_7 + 1), A        ; mask
                CPL
                LD   (_PSC_OPchg_8 + 1), A        ; mask

_PSC_LineLoop:
__Change_4:     LD   A, (IX+0)
_PSC_OPchg_3:   JR   _PSC_OPchg_3                 ; modify

                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA
                RLCA

                LD   E, A
_PSC_OPchg_6:   AND  %11110000
                LD   C, A
                XOR  E
                LD   D, A

                LD   A, 0 ;(IX+0)
_PSC_OPchg_4:   JR   _PSC_OPchg_4                 ; modify

                RRCA
                RRCA
                RRCA
                RRCA
                RRCA
                RRCA
                RRCA

                LD   E, A
_PSC_OPchg_7:   OR   %11110000
                AND  (HL)                         ; AND with background
                XOR  D                            ; XOR with background
_PSC_OPchg_2:   LD   (HL), A                      ; Write
                INC  HL                           ; HL -> next 8 pixels

                LD   A, E
_PSC_OPchg_8:   OR   %00001111
                AND  (HL)                         ; AND with background
                XOR  C                            ; XOR with background
_PSC_OPchg_1:   LD   (HL), A                      ; Write

                LD   DE, 11
                ADD  HL, DE                       ; HL -> next row

                INC  IX                           ; Increment to next data

                DJNZ _PSC_LineLoop
                RET

MaskTable:      .DB   %00000000
                .DB   %10000000
                .DB   %11000000
                .DB   %11100000
                .DB   %11110000
                .DB   %11111000
                .DB   %11111100
                .DB   %11111110

DrawSideBar:
 ld hl,GRAPH_MEM+9
 ld de,12
 ld b,64
drawline:
 ld (hl),%10000000
 add hl,de
 djnz drawline

 set 7,(iy+$14)
 ld hl,SideBar
 ld de,$0142+8
 call DMX
 ld d,8
 call DMX
 ld d,16
 call DMX
 ld a,(level)
 ld b,2
 call DM_A_DECI
 ld d,23
 call DMX
 ld a,(map_length)
 ld b,3
 call DM_A_DECI
 ld d,51
 call DMX
 ld d,57
 call DMX
 res 7,(iy+$14) 
 call findcol
 ld hl,CurEnemyTable+1
 ld de,3
 ld b,100
findifE:
 ld c,(hl)
 cp c
 jr z,drawnE
 add hl,de
 djnz findifE
 ld hl,bgtiles
drawES:
 ld bc,$5808
 jp PutSprite

drawnE:
 dec hl
 ld a,(hl)
 dec a
 and %1111
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 ld de,enemies
 add hl,de
 jr drawES

DMX:
 ld (pencol),de
DMX2:
 push bc
 bcall(_vputs)
 pop bc
 ret


Compress:
   push ix 
   call RLEcompL 
   pop de 
   push ix 
   pop hl 
   sbc hl, de 
   ret

RLEcompL: 
   ld a, (hl) 
   inc hl 
   cp (hl) 
   jr nz, ContRLEcomp2 
RLEcomp_StoType:
   push af
   add a,$80
   ld (ix),a ;set the byte
   pop af
   inc ix
   ld e,1 
RLEcompRunL: 
   CPI 
   jp po, End_RLEcompRunL 
   jr nz, End_RLEcompRunL 
   inc e 
   jr nz,RLEcompRunL 
   jr ContRLEcomp 
End_RLEcompRunL: 
   inc bc 
   dec hl 
ContRLEcomp: 
   ld a,e
ContRLEcomp2:
   ld (ix),a 
   dec bc 
   ld a,b 
   or c 
   inc ix 
   ret z 
   jr RLEcompL

Decompress:
    ld a, (hl)          ; get the next byte
    sub $80             ; is it a run?
    jr nc, DispRLERun   ; then we need to decode the run
    ldi                 ; copy the byte, and update counters
DispRLEC:
    ld a,b
	or c
	ret z               ; ret if bc hit 0
    jr Decompress       ; otherwise do next byte
DispRLERun:
    ld (hl),a ;make this the copy byte
    inc hl              ; move to the run count
    ld a,(hl)           ; get the run count
DispRLERunL:
    dec hl              ; go back to run value
    ldi                 ; copy byte, dec bc, inc de, inc hl
    dec a               ; decrease run counter
    jr nz, DispRLERunL  ; if we're not done, then loop
    dec hl
    ld a,(hl)
    add a,$80
    ld (hl),a
    inc hl
    inc hl              ; advance the source pointer
    jr DispRLEC         ; check to see if we should loop

WriteText:
 xor a
 ld (alphalock),a
 ld (pencol),de
 res 7,(iy+$14)
 push hl
 ld a,b
 ld (maxlet),a
space_loop:
 ld	(hl), ' '
 inc hl
 djnz space_loop

 pop ix
 bcall(_getcsc)		;to reset the key buffer (8000h)
enter_name_loop:
 push ix
 push hl
 push bc
 ld hl,(pencol)
 push hl
 ld a,(alphalock)
 or a
 jr z,Lock1
Lock2:
 ld hl,AlphaLock2
DoneAlph:
 ld de,$3810
 call DMX
 pop hl
 ld (pencol),hl
 pop bc
 pop hl
 pop ix

 bcall(_getcsc)
 cp G_2ND
 jr z,alphachange
 cp G_CLEAR
 jr z,ExitNOW
 sub G_ENTER
 ret z
 jr	c,enter_name_loop
 ld	hl,chartable-1
 ld	e,a
 ld d,0
 add hl,de
 ld	a,0
maxlet = $-1
 cp	b
 jr	z,enter_name_loop
 ld	a,(hl)
 or	a
 jr	z,enter_name_loop
 ld c,a
 ld a,0
alphalock = $-1
 or a
 call nz,makelower

 ld	(ix),c
 ld a,c
 push ix
 bcall(_vputmap)
 pop ix
 inc b
 inc ix
 jr	enter_name_loop

ExitNOW:
 ld sp,0
INITSP = $-2
 ret

alphachange:
 ld a,(alphalock)
 xor %1
 ld (alphalock),a
 jr enter_name_loop

Lock1:
 ld hl,AlphaLock1
 jr DoneAlph

makelower:
 ld a,c
 cp ' '
 ret z

 ld a,32
 add a,c
 ld c,a
 ret

AlphaLock1:
 .db "Uppercase Enabled ",0

AlphaLock2:
 .db "Lowercase Enabled",0

chartable:
         .db      ":WRMH0"
         .DB      0,"?",5Bh,"VQLG!",0,".ZUPKFC"	;5Bh is theta
         .DB      "> YTOJEB<",0,"XSNIDA"
         .DB      0,"54321",0,0

ID: 
	.db $AA,$BB,0

LoadG:
 call HeadMenu
 ld hl,(progptr)
SearchLoop:
 ld ix,ID
 call iondetect
 jr nz, SearchDone
 ld hl,LvlNo
 inc (hl)
 ex de,hl
 jr SearchLoop

SearchDone:
 ld de, $0801
 ld a,(LvlNo)
 or a
 jr nz,tt
NoLevels
 ld hl, NoLevel
 call DMX
 call ionfastCopy
 call OTH_PAUSE
 scf
 ret

tt:
 xor a
 ld (SetD+1),a
ShowLevels:
 ld hl,GRAPH_MEM+(7*12)		;+112
 ld bc,	50*12-1			;799
 call OTH_CLEAR

 ld b,8
 ld a,(LvlNo)
 cp b
 jr nc,ShowLevel2
 ld b,a
ShowLevel2:
 push bc
 dec b
 ld l,b
 ld h,6
 bcall(MUL_HL)
 ld de, $0801
 ld a,l
 add a,d
 ld d,a
 pop bc
ShowLevel:
 ld (pencol),de

 push de
 push bc
 ld a,(Top)
 add a,b

 call LoadLevelA
 ld b,24
 bcall(_vputsn)
 pop bc
 pop de

 ld a,d
 sub 6
 ld d,a
 djnz ShowLevel

 ld hl,Top
 ld a,(LvlNo)
 sub (hl)
 cp 8
 jr c,SetMax1
 ld a,8
SetMax1:
 ld e,a
 dec e

SetD:
 ld d,0
 call ShowBar
CL_KeyLoop:
 push de
 bcall(_getcsc)
 pop de
 cp G_DOWN
 jr z,ScrollDown
 cp G_UP
 jr z,ScrollUp
 cp G_2ND
 jr z,AcceptLevel
 cp G_ENTER
 jr z,AcceptLevel
 cp G_CLEAR
 jr nz,CL_KeyLoop
 scf
 ret

ScrollDown:
 call ShowBar
 ld a,d
 cp e
 jr z,ScrollL_Down
 inc d
 jr ShowIt

ScrollL_Down:
 ld (SetD+1),a
 ld a,(LvlNo)
 sub 8
 jr c,ShowIt
 ld hl,Top
 cp (hl)
 jr z,ShowIt
 inc (hl)
 jr Sh

ScrollL_Up:
 ld (SetD+1),a
 ld hl,Top
 ld a,(hl)
 or a
 jr z, ShowIt
 dec (hl)
Sh:
 jp ShowLevels

ScrollUp:
 call ShowBar
 ld a,d
 or a
 jr z,ScrollL_Up
 dec d
ShowIt:
 call ShowBar
 jr CL_KeyLoop

AcceptLevel:
 ld a,(Top)
 add a,d
 inc a
 call LoadLevelA
 push hl

 ex de,hl
FindBlankLoop:
 inc hl
 ld a,(hl)
 cp 9
 jr nc,FindBlankLoop

 ld b,(hl)
 ld ix,LVarName
 ld (ix),6
 inc ix
CopyLoop:
 dec hl
 ld a,(hl)
 ld (ix),a
 inc ix
 djnz CopyLoop

 ld hl,LVarName
 bcall(_mov9toop1)
 bcall(_chkfindsym)
 pop hl

#ifdef TI83P
 ld a,b
 or a
 ret nz   ;return if it doesn't exist in RAM
#endif TI83P

 ld de,24
 add hl,de  ; skip length of program + header bytes + description

 ;now check the password
 push hl
 call HeadMenu
 ld de,$0800
 ld hl,passw
 call DMX
 call ionFastCopy

 ld d,$10
 ld hl,APD_BUF  ;store it here
 push hl
 ld b,4
 call WriteText
 pop de
 pop hl
 ld b,4 ;four letters to test
checkpassloop:
 ld c,(hl)
 ld a,(de)
 sub c
 ret nz
 inc hl
 inc de
 djnz checkpassloop

 ld (WorldLoc),hl

 call LoadLevel
 jp RealProg			; return hl = pointer to the program.

passw:
 .db "Input Password:",0

LoadLevelA:
 ld b,a
 ld de,(progptr)
LoadLevelB2:
 ex de,hl
 ld ix,ID
 push bc
 call iondetect
 pop bc
 djnz LoadLevelB2
 ret

NoLevel:
 .db "No Levels Found!",0


ShowBar:
 push de
 ld l,d
 ld h,72
 bcall(MUL_HL)
 ld de,GRAPH_MEM+(8*12)
 add hl,de
 call Invert
 pop de
 ret

OTH_PAUSE:
 bcall(_getcsc)
 cp 9
 jr nz,OTH_PAUSE
 ret

Invert:
 ld bc,84
InvLoop:
 ld a,(hl)
 cpl
 ld (hl),a
 inc hl
 dec bc
 ld a,b
 or c
 jr nz,InvLoop
 jp ionFastCopy


CurMap:
 .ds 1024

MapBackup:
 .ds 1024

SideBar:
 .db "Blk:",0
 .db "En:",0
 .db "Lvl:",0
 .db "C:  ",0
 .db "Mode:",0
 .db "Menu",0

enemytxt:
 .db "Select Enemy:",0

enemies:
 ;Tile 0
 .db $3C,$7E,$DB,$FF,$E7,$7E,$3C,$66
 ;Tile 1
 .db $06,$0E,$0C,$0D,$0F,$1F,$7D,$5C
 ;Tile 2
 .db $00,$00,$00,$3C,$7A,$DF,$F7,$7E
 ;Tile 3
 .db $C3,$E7,$7E,$3C,$99,$7E,$3C,$99
 ;Tile 4
 .db $C3,$BD,$5A,$66,$3C,$99,$A5,$C3
 ;Tile 5
 .db $BD,$66,$DB,$42,$7E,$C3,$7E,$A5
 ;Tile 6
 .db $0C,$0D,$0F,$1F,$7D,$6C,$C4,$8C
 ;Tile 7
 .db $1F,$3E,$5F,$FE,$F6,$4F,$3E,$1F
 ;Tile 8
 .db $E0,$C0,$AE,$19,$3E,$3F,$3F,$1F
 ;Tile 9
 .db $19,$3E,$3F,$3F,$1F,$AE,$C0,$E0
 ;Tile 10
 .db $1C,$39,$7B,$BE,$7E,$7F,$F3,$38
 ;Tile 11
 .db $7C,$42,$42,$7C,$42,$42,$42,$7C
 ;Tile 12
 .db $38,$24,$24,$38,$00,$7F,$81,$7F
 ;Tile 13
 .db $FF,$BF,$7E,$10,$38,$10,$38,$10
 ;Tile 14
 .db $FF,$BF,$7E,$00,$24,$7E,$24,$00

bgtiles:
 ;Tile 0
 .db $00,$00,$00,$00,$00,$00,$00,$00
 ;Tile 1
 .db $7E,$AB,$D5,$AB,$D5,$AB,$D7,$7E
 ;Tile 2
 .db $7E,$83,$9F,$9F,$9F,$9F,$FF,$7E
 ;Tile 3
 .db $FF,$C3,$A7,$9D,$9B,$AD,$D7,$FF
 ;Tile 4
 .db $7E,$CF,$9F,$BB,$F3,$E7,$FF,$7E
 ;Tile 5
 .db $FF,$81,$B1,$A1,$81,$81,$81,$FF
 ;Tile 6
 .db $7F,$EF,$D2,$BD,$BF,$FF,$FF,$FF
 ;Tile 7
 .db $FF,$6F,$56,$B5,$FB,$FF,$FF,$FF
 ;Tile 8
 .db $FE,$6F,$53,$BD,$FD,$FF,$FF,$FF
 ;Tile 9
 .db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 ;Tile 10
 .db $7F,$C0,$87,$F0,$8F,$F9,$FF,$7F
 ;Tile 11
 .db $FF,$00,$07,$F0,$8F,$F9,$FF,$FF
 ;Tile 12
 .db $FE,$03,$07,$F1,$8F,$F9,$FF,$FE
 ;Tile 13
 .db $FF,$82,$81,$82,$81,$FF,$40,$41
 ;Tile 14
 .db $40,$41,$40,$41,$3F,$40,$41,$40
 ;Tile 15
 .db $FF,$BF,$5F,$BF,$5F,$FF,$BE,$5E
 ;Tile 16
 .db $BE,$5E,$BE,$5E,$FC,$BE,$5E,$BE
 ;Tile 17
 .db $7E,$99,$A5,$8D,$99,$81,$99,$7E
 ;Tile 18
 .db $7E,$99,$A5,$8D,$99,$81,$99,$7E
 ;Tile 19
 .db $7E,$99,$A5,$8D,$99,$81,$99,$7E
 ;Tile 20
 .db $7E,$99,$A5,$8D,$99,$81,$99,$7E
 ;Tile 21
 .db $7E,$83,$BB,$A3,$A3,$87,$FF,$7E
 ;Tile 22
 .db $BD,$BD,$BD,$00,$DB,$DB,$DB,$00
 ;Tile 23
 .db $7E,$81,$8D,$89,$B9,$B9,$81,$7E
 ;Tile 24
 .db $7F,$DE,$8E,$C5,$92,$C7,$E2,$D0
 ;Tile 25
 .db $FF,$DE,$8E,$C5,$92,$47,$E2,$50
 ;Tile 26
 .db $FE,$DF,$8F,$C5,$93,$47,$E3,$51
 ;Tile 27
 .db $B9,$94,$8E,$C5,$92,$C7,$E2,$D0
 ;Tile 28
 .db $B9,$14,$8E,$C5,$92,$47,$E2,$50
 ;Tile 29
 .db $B9,$15,$8F,$C5,$93,$47,$E3,$51
 ;Tile 30
 .db $7E,$FF,$FD,$BF,$F7,$FF,$FD,$DF
 ;Tile 31
 .db $FF,$EF,$FD,$BF,$F7,$FF,$FD,$DF
 ;Tile 32
 .db $00,$00,$00,$00,$03,$0F,$3D,$DF
 ;Tile 33
 .db $03,$0F,$3D,$FF,$F7,$FF,$FD,$DF
 ;Tile 34
 .db $01,$03,$05,$0F,$17,$3F,$7D,$DF
 ;Tile 35
 .db $00,$00,$00,$00,$C0,$F0,$FC,$DF
 ;Tile 36
 .db $C0,$F0,$FC,$BF,$F7,$FF,$FD,$DF
 ;Tile 37
 .db $80,$C0,$E0,$B0,$F8,$FC,$FE,$DF
 ;Tile 38
 .db $7F,$80,$98,$AC,$B4,$98,$80,$80
 ;Tile 39
 .db $FF,$00,$00,$00,$00,$00,$00,$00
 ;Tile 40
 .db $F8,$04,$67,$B7,$D7,$67,$07,$07
 ;Tile 41
 .db $80,$80,$80,$80,$80,$80,$80,$80
 ;Tile 42
 .db $07,$07,$07,$07,$07,$07,$07,$07
 ;Tile 43
 .db $80,$80,$98,$AC,$B4,$98,$80,$7F
 ;Tile 44
 .db $00,$00,$00,$00,$00,$00,$00,$FF
 ;Tile 45
 .db $07,$07,$67,$B7,$D7,$67,$07,$FF
 ;Tile 46
 .db $10,$54,$38,$FE,$38,$54,$10,$00
 ;Tile 47
 .db $38,$44,$82,$8A,$8A,$92,$44,$38
 ;Tile 48
 .db $00,$04,$40,$00,$00,$02,$00,$10
 ;Tile 49
 .db $00,$00,$00,$00,$00,$FF,$DA,$A5
 ;Tile 50
 .db $38,$54,$6E,$BF,$EB,$D7,$AF,$7E
 ;Tile 51
 .db $00,$07,$1F,$3F,$39,$7B,$7F,$7F
 ;Tile 52
 .db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
 ;Tile 53
 .db $00,$E0,$F8,$FC,$9C,$DE,$FE,$FE
 ;Tile 54
 .db $F8,$E7,$DF,$BF,$B9,$7B,$7F,$7F
 ;Tile 55
 .db $1F,$E7,$FB,$FD,$9D,$DE,$FE,$FE
 ;Tile 56
 .db $36,$49,$80,$A0,$40,$82,$91,$6E
 ;Tile 57
 .db $EE,$11,$01,$06,$01,$21,$12,$EC
 ;Tile 58
 .db $01,$03,$04,$0F,$10,$3F,$40,$FF
 ;Tile 59
 .db $00,$FF,$00,$FF,$00,$FF,$00,$FF
 ;Tile 60
 .db $55,$FF,$AA,$FF,$55,$FF,$AA,$FF
 ;Tile 61
 .db $80,$C0,$A0,$F0,$58,$FC,$AA,$FF
 ;Tile 62
 .db $3C,$56,$BB,$E7,$A7,$DF,$7E,$3C
 ;Tile 63
 .db $3C,$7E,$FF,$FF,$FF,$56,$42,$3C
 ;Tile 64
 .db $00,$18,$24,$C3,$81,$42,$99,$E7
 ;Tile 65
 .db $3C,$5A,$B3,$E7,$CD,$99,$B3,$E7
 ;Tile 66
 .db $CD,$99,$B3,$E7,$CD,$99,$B3,$E7
 ;Tile 67
 .db $C3,$BD,$5A,$5A,$7E,$5A,$5A,$7E
 ;Tile 68
 .db $7E,$5A,$5A,$7E,$5A,$5A,$7E,$00
 ;Tile 69
 .db $E7,$BD,$E7,$FF,$FF,$FF,$FF,$E7
 ;Tile 70
 .db $0C,$16,$2F,$5F,$BF,$F3,$61,$2C
 ;Tile 71
 .db $2C,$61,$F3,$BF,$5F,$2F,$16,$08


MenuText:
 .db "Add level",0
 .db "Remove level",0
 .db "Change Info",0
 .db "Instructions",0
 .db "Quit w/o save",0

instructiontext:
 .db "Y= : Add Enemy",0
 .db "WINDOW : Previous Block",0
 .db "ZOOM : Next Block",0
 .db "TRACE/GRAPH : Shift 8 L/R",0
 .db "2nd : Place Block",0
 .db "ALPHA: Select Block",0
 .db "DEL : Clear Block",0
 .db "XTO : Insert Column",0
 .db "STAT : Remove Column",0
 .db "+/- : Change Level",0

AddMem:
 push hl
 push de
 push hl
 ld hl,LVarName
 bcall(_mov9toop1)
 bcall(_chksysvar)
 pop bc
 push de
 ex de,hl ;hl = size
 bcall(_ldhlind)
 add hl,bc
 ex de,hl
 pop hl
 ld (hl),e
 inc hl
 ld (hl),d
 dec hl
 pop de     ;# of bytes to offset
 add hl,de
 ex de,hl
 pop hl
 bcall(_insertmem)
 ret

DelMem:
 push hl
 push de
 push hl
 ld hl,LVarName
 bcall(_mov9toop1)
 bcall(_chksysvar)
 pop bc

 push de
 ex de,hl ;hl = size
 bcall(_ldhlind)
 or a
 sbc hl,bc
 ex de,hl
 pop hl

 ld (hl),e
 inc hl
 ld (hl),d
 dec hl
 pop de     
 add hl,de  ;hl = location
 pop de     ;de = # of bytes
 bcall(_deletemem)
 ret


LoadLevel:
 call InitMap
 call LoadLOffset
 push hl
 ld c,(hl)
 inc hl
 ld b,(hl)  ;BC = # of bytes to Decompress

 push bc
 push hl

 ld h,b
 ld l,c
 ld a,8
 bcall(_divhlbya)
 ld a,l
 ld (map_length),a
 pop hl
 pop bc

 inc hl
 ld e,(hl)
 inc hl
 ld d,(hl)  ;DE = # of bytes when Compressed
 inc hl
 ld (oldsize),de

 push de
        ;bc = # of bytes to decompress
        ;hl now points to data to decompress
 ld de,CurMap
 call Decompress
 pop de
 pop hl
 add hl,de       ;now point to enemy_table
 inc hl
 inc hl
 inc hl
 inc hl

 ld e,(hl)
 inc hl
 ld d,(hl)
 inc hl     ;now points to data

 push de

 ld de,CurEnemyTable
 ld bc,300           ;always 300
 call Decompress
 pop de
 ld hl,(oldsize)
 add hl,de
 ld (oldsize),hl  ;oldsize = data + table (compressed)
 ret

LoadLOffset:
 ld hl,(WorldLoc)  ;points to external data
 inc hl  ;skip # of levels
 ld a,(level)
 dec a
 add a,a 
 ld e,a
 ld d,0
 add hl,de  ;find offset value
 ld e,(hl)
 inc hl
 ld d,(hl)      ;de = offset from MapLoc+51

 ld hl,(WorldLoc)
 ld bc,51
 add hl,bc
 add hl,de  ;now points to level data
 ret

SaveLevel:
 ld a,(map_length)
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 push hl
 pop bc  ;x8 ;bc = # of bytes

 ld hl,CurMap
 ld ix,MapBackup
 call Compress  ;hl = # of bytes

 push hl
 ld hl,CurEnemyTable
 ld ix,MapBackup
 ld bc,300
 call Compress ;hl = # of bytes
 pop de
 add hl,de     ;new size of enemytable & map

 ld de,0
oldsize = $-2  ;old size
 bcall(_cphlde)      ;find out change in size
 jr z,NothingToChange
 jr c,ProgramShrinks
ProgramGrows:
 or a
 sbc hl,de
 ld (changeSIZE),hl

     ;hl = number of bytes to increase...
 push hl
 bcall(_memfree) ;check if there is enough memory
 pop hl
 ret c  ;return if not enough memory

 push hl
 call LoadLOffset  ;de = offset from MapLoc+51
 ld hl,83
 add hl,de  ;hl = offset from start of program
 ex de,hl   ;now it's DE
 pop hl

 call AddMem
 jr RealSaveNow


ProgramShrinks:
 push hl
 or a
 sbc hl,de 
 ld (changeSIZE),hl
 pop hl

 ex de,hl
 or a
 sbc hl,de
;hl = number of bytes to decrease... 

 push hl
 call LoadLOffset  ;de = offset from MapLoc+51
 ld hl,83
 add hl,de  ;hl = offset from start of program
 ex de,hl   ;now it's DE
 pop hl

 call DelMem

RealSaveNow:  ;now that the memory shit is done, really 
		  ;save the game!!!

 call UpdateOff

NothingToChange:
 ld hl,(WorldLoc)
 inc hl
 ;hl points to lookup table
 push hl
 ld a,(level)
 dec a
 add a,a
 ld e,a
 ld d,0
 add hl,de
 bcall(_ldhlind)
 pop de
 ld bc,50
 add hl,de
 add hl,bc  ;points to level data

 push hl
 ld a,(map_length)
 ld l,a
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 push hl
 pop bc    ;x8 ;bc = # of bytes
 pop hl
 ld (hl),c
 inc hl
 ld (hl),b  ;load new # of bytes
 inc hl
            ;need to load new # byte compress
 push hl
 inc hl
 inc hl   ;but skip for now
 push hl
 pop ix

 ld hl,CurMap
 call Compress  ;hl = # of bytes

 ex de,hl
 pop hl
 ld (hl),e
 inc hl
 ld (hl),d
 inc hl

 add hl,de   ;now point to enemy_data_table

 push hl   ;need to write down # of bytes compress

 inc hl
 inc hl

 push hl
 pop ix

 ld hl,CurEnemyTable
 ld bc,300
 call Compress

 ex de,hl
 pop hl
 ld (hl),e
 inc hl
 ld (hl),d ;now that it's written. We can go!!!
 ret

UpdateOff:
 ld hl,(WorldLoc)  ;points to external data
 inc hl  ;skip # of levels
 ld a,(level)
 ld b,a

 add a,a 
 ld e,a
 ld d,0
 add hl,de  ;find offset value

 ld a,25
 sub b
 ret z
 ld b,a
UpdateOffsets:
 push hl
 bcall(_ldhlind)
 ld de,0
changeSIZE = $-2
 add hl,de
 ex de,hl   ;DE = new offset
 pop hl

 ld (hl),e
 inc hl
 ld (hl),d
 inc hl
 djnz UpdateOffsets
 ret


ChooseBlock:
 bcall(_cleargbuf)
 ld hl,bgtiles
 ld c,0
 ld a,6    ;6 rows 
DrawRow:
 push af
 ld b,0
 ld a,12   ;12 sprites per row
DrawRow2:
 push af
 push bc
 push hl
 call PutSprite
 pop hl
 ld bc,8
 add hl,bc
 ld a,c
 pop bc
 add a,b
 ld b,a
 pop af
 dec a
 jr nz,DrawRow2
 ld a,c
 add a,8
 ld c,a
 pop af
 dec a
 jr nz,DrawRow
 ld a,(cursor)
 ld (cursor_temp),a
 xor a
 ld (cursor),a
 call drawselector
ChooseLoop:
 bcall(_getcsc)
 cp G_2ND
 jr z,BlockChosen
 cp G_LEFT
 jr z,BLeft
 cp G_RIGHT
 jr z,BRight
 cp G_DOWN
 jr z,BDown
 cp G_UP
 jr z,BUp
 cp G_CLEAR
 jr nz,ChooseLoop
 ld a,0
cursor_temp = $-1
 ld (cursor),a
BlockChosen:
 jp MainLoop

BLeft:
 call drawselector
 ld hl,cursor
 ld a,b
 or a
 jr nz,NGR
 ld a,(hl)
 add a,12
 ld (hl),a
NGR:
 dec (hl)
 call drawselector
 jr ChooseLoop

BRight:
 call drawselector
 ld hl,cursor
 ld a,b
 cp 11
 jr nz,NGL
 ld a,(hl)
 sub 12
 ld (hl),a
NGL:
 inc (hl)
 call drawselector
 jr ChooseLoop

BUp:
 call drawselector
 ld hl,cursor
 ld a,c
 or a
 jr nz,NGU
 ld a,(hl)
 add a,72
 ld (hl),a
NGU:
 ld a,(hl)
 sub 12
 ld (hl),a
 call drawselector
 jr ChooseLoop

BDown:
 call drawselector
 ld hl,cursor
 ld a,c
 cp 5
 jr nz,NGD
 ld a,(hl)
 sub 72
 ld (hl),a
NGD:
 ld a,(hl)
 add a,12
 ld (hl),a
 call drawselector
 jr ChooseLoop

drawselector:
 call ldselcoord
 push bc
 ld hl,GRAPH_MEM
 ld e,b
 ld d,0
 add hl,de

 push hl
 ld l,c
 ld h,96
 bcall(MUL_HL)
 pop de
 add hl,de

 ld b,8
 ld de,12
Invert8:
 ld a,(hl)
 xor $FF
 ld (hl),a
 add hl,de
 djnz Invert8
 call ionFastCopy
 pop bc
 ret

ldselcoord:
 ld a,(cursor)
 ld l,a
 ld h,0   
 ld a,12
 bcall(_divhlbya)
 ld b,a
 ld c,l
 ret

JumpLeft:
 ld hl,map_offset
 ld a,(hl)
 sub 8
 jr nc,NsetzeroH
 xor a
NsetzeroH:
 ld (hl),a
 jp MainLoop

JumpRight:
 ld a,(map_length)
 sub 9
 ld b,a

 ld hl,map_offset
 ld a,(hl)
 add a,8
 cp b
 jr c,NsetzeroH

 ld a,(map_length)
 sub 9
 ld (hl),a
 jr NsetzeroH
 


.end
