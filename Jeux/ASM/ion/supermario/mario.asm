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

TitleTxt:
TitleScreenText2:
.db "Super Mario v1.2",0

Start:
 ld (INITSP),sp
 set 7,(iy+$14) ;always write to screen

 call ClearKeys

 ld a,96
 call LoadClip

 ld c,0
DrawMushroomSides:
 ld hl,mushroom+10
 ld b,0
 call PutSprite
 ld b,88
 call PutSprite
 ld a,8
 add a,c
 ld c,a
 cp 64
 jr nz,DrawMushroomSides

 ld a,80
 call LoadClip

 ld hl,TitleScreenText
 ld de,$0115
 call DMX
 ld de,$090B
 call DMX
 ld de,$1122
 call DMX
 push hl
 ld hl,TitleScreenText2
 ld de,$1912
 call DMX
 pop hl
 ld de,$210D
 call DMX
 ld de,$290C
 call DMX
 ld de,$311A
 call DMX

 call CopyGRAPHtoAPD

 bcall(_cleargbuf)
 ld hl,TitlePic
 ld de,GRAPH_MEM
 call DecodeZCP

ScrollAPD_Loop2:
 ld b,64
 ld hl,APD_BUF
ScrollAPD_Loop:
 push bc
 push hl
 push hl
 ld hl,GRAPH_MEM+540
 ld de,GRAPH_MEM+528
 ld bc,228
 ldir
 pop hl
 ld de,GRAPH_MEM+756
 ld c,12
 ldir
 call fastCopy
 ld b,12
 call FlashDelay2
 call OTH_ARROW
 bit 5,a
 pop hl
 pop bc
 jr z,GameStart
 ld a,%01111101
 out (1),a
 xor a
 ld d,a   ;delays 8 cycles and clears D            
 in a,(1)
 bit 0,a
 jr z,GameStart
 bit 6,a
 ret z
 ld e,12
 add hl,de
 djnz ScrollAPD_Loop
 jr ScrollAPD_Loop2

GameStart:
 ld hl,APD_BUF
 call OTH_CLEAR2
 ld hl,TEXT_MEM3
 ld bc,127
 call OTH_CLEAR
 call ClearKeys
 inc a
 ld (level),a
 ld a,3
 ld (lives),a ;intro stuff

 call Loadlevel
 ret c

NewLevel:
 xor a
 ld (map_offset),a
 ld (xpos),a
 ld (ypos),a
 ld (scroll_x),a
 ld (direction),a
 ld (jumpcounter),a
 ld (blockcounter),a
 inc a
 ld (fallflag),a

 call drawinitmap

 bcall(_clrlcdf)
 set 3,(iy+5)
 ld de,$0000
 ld hl,TitleTxt
 call DTX
 res 3,(iy+5)
 ld hl,(level)
 ld h,e
 ld de,$0704
 ld (currow),de
 bcall(_disphl)
 ld hl,LevelTxt
 ld de,$0404
 call DTX
 call OTH_PAUSE

MainLoop:
 call OTH_ARROW
 ld (KEYPRESS),a
 bit 1,a
 call z,MoveLeft

 ld a,(KEYPRESS)
 bit 2,a
 call z,MoveRight

 ld a,(KEYPRESS)
 bit 5,a
 call nz,NoJump

 ld a,(KEYPRESS)
 bit 5,a
 call z,Jump

 xor a
 ld (superjumpflag),a

 ld hl,alphapressed
 ld a,(KEYPRESS)
 push af
 bit 4,a
 call nz,NoFireBall
 call z,Fireball
 pop af
 bit 6,a
 call z,Pause

 ld a,(jumpcounter)
 or a
 jp z,CheckBelowCollision
 call nz,CheckEwithM
CheckDelay:
 ld a,(ypos)
 or a
 call m,delaygame

 ei \ halt

 ld a,(updateflag)
 or a
 call nz,updatestats

 call MoveEnemies
 call CheckShellswithE
 call DnEMario
 ld a,%01111101
 out (1),a
 nop
 nop             
 in a,(1)
 bit 6,a
 jp nz,MainLoop
 ret

NoFireBall:
 ld (hl),0
 ret

delaygame:
 neg
 cp 12
 call nc,setb1
 ld b,a
delay_loop:
 push bc
 ld b,30
delay_loop_inner:
 push ix
 pop ix
 djnz delay_loop_inner
 pop bc
 djnz delay_loop
 ret


Pause:
  DI                  ; disable interrupts
  LD  A, 01           ; bit 3 = lcd status
  OUT ($03), A        ; bit 0 = ON-interrupt status
  EI                  ; enable interrupts
  HALT                ; wait for ON (that's the only interrupt)
  RET

Jump:
 ld a,(fallflag)
 or a
 ret nz

 ld a,(superjumpflag)
 ld hl,jumpcounter
 or a
 call nz,jumphigher

 ld a,(hl)
 or a
 jr nz,alreadyjumping
 ld (hl),18
alreadyjumping:
 dec (hl)
 call FindJumpValue2
MoveUp:
 ld a,b
 ld (upcount),a
 ld hl,ypos
 inc (hl)
jumploop:
 ld hl,ypos
 dec (hl)
 call CheckUpM
 jr nz,clearjump
 ld a,0
upcount = $-1
 dec a
 ld (upcount),a
 jr nz,jumploop
 ret

jumphigher:
 ld (hl),24
 ret

NoJump:
 ld a,(jumpflag)
 or a
 ld hl,jumpcounter
 jr nz,alreadyjumping
clearjump:
 xor a
 ld (jumpcounter),a
 ld (jumpflag),a
 ret

MoveLeft:
 ld a,1
 ld (direction),a
 call LoadMarioStats
 call CheckLeftCollision
 ret nz
 ld hl,xpos
 ld a,(hl)
 or a
 ret z
 ld (runflag),a
 dec (hl)
 ret

MoveRight:
 xor a
 ld (direction),a
 call LoadMarioStats
 call CheckRightCollision
 ret nz
 ld a,(xpos)
 ld (runflag),a
 cp 42
 jr z,ScrollScreen
ScrollNo:
 ld hl,xpos
 ld a,(hl)
 cp 72
 ret z
 inc (hl)
 ret

ScrollScreen:
 ld a,0
endlevelflag = $-1
 or a
 jr z,ScrollNo

 ld hl,scroll_x
 inc (hl)
 ld a,(hl)
 and %111
 ld (hl),a
 call z,inc_off

 call CheckEndLevel

Scroll_L:                       ; [17024 Clock Cycles]
  LD  HL, GRAPH_MEM+765 ;-2 for sidebar      ; Shift starting from bottom row
  LD  B, 64                     ; Do 64 rows
  ld  de,-3
_LLoop:
  SLA (HL)                      ; 96-bit shift left
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  DEC HL
  RL  (HL)
  add hl,de
  DJNZ _LLoop

ShiftEnemies:
 ld hl,Enemies_On_Screen+2
 ld b,32
shift_loop:
 dec (hl)
 ld a,(hl)
 sub 164
 call z,kille2
 push hl
 pop ix
 dec (ix+2)
 ld a,(ix-2) ;enemy
 cp 8
 jr c,dontcheckt
 cp 11
 jr nc,dontcheckt
 ld a,(ix+2)
 sub 228
 call z,kille2
dontcheckt:
 ld de,7
 add hl,de
 djnz shift_loop

 ld hl,map
 ld a,(map_offset)
 add a,10
 ld c,a
 add hl,bc
 ld c,b   ;clear C
 ld a,(scroll_x)
 or a
 jr z,DrawLastLine
 sub 80
 neg
 ld b,a
 jr DrawingLastLine
DrawLastLine:
 dec hl  ;last sprite
 ld b,72 ;last column
DrawingLastLine
 ld a,8  ;8 loops
PutNewSpriteCol:
 push af
 push hl
 call PutSpriteHL
 ld a,c
 add a,8
 ld c,a
 pop hl
 call LoadMapLenghDE
 add hl,de
 pop af
 dec a
 jr nz,PutNewSpriteCol
 ret

kille2:
 dec hl
 dec hl
 ld (hl),a
 inc hl
 inc hl
 ret

inc_off:
 ld hl,map_offset
 inc (hl)
 ld a,89
 ld (initx),a
TestForNewEnemies:
 ld a,(map_offset)
 add a,11
 ld e,a
 ld hl,enemy_table+1
 ld b,100
search_enemy_loop:
 ld a,(hl)
 cp e
 jr z,CreateNewEnemy
 inc hl
 inc hl
 inc hl
 djnz search_enemy_loop
 ret

CreateNewEnemy:
 push hl
 pop ix

 call FindESlot
 ret nz
 ld d,a      ;clear D

 ld a,(ix-1)
 ld (hl),a
 ld e,a
 dec e

 push hl
 ld hl,InitEStatusTable
 add hl,de
 ld b,(hl)
 pop hl
 inc hl
 ld (hl),b
 inc hl
 ld (hl),0
initx = $-1 ;x coord
 ld a,(ix-1)
 cp 4
 jr nz,noshiftright4
 inc (hl)
 inc (hl)
 inc (hl)
 inc (hl)
noshiftright4:
 push hl
 call LoadEHeight2
 ld a,(hl)
 inc hl
 ld e,(hl)
 sub 8
 ld b,a
 ld a,(ix+1)
 sub b
 pop hl

 ld d,a

 ld a,e
 sub 8
 ld b,a
 ld a,(hl)
 sub b
 ld (hl),a
 inc hl

 ld a,(ix-1)
 dec a  ;check for goomba
 jr nz,nogoombadec
goombadec:
 dec d
nogoombadec:
 ld (hl),d ;d = init y
 dec hl
 ld b,(hl) ;b = init x
 inc hl
 inc hl
 ld (hl),b  ;store init x
 inc hl
 ld (hl),d  ;store init y
 inc hl
 ld (hl),0
 ld a,(ix-1)
 cp 4
 ret nz
 ld (hl),12
 ret

Fireball:
 ld a,(hl)
 or a
 ret nz

 ld a,(bigflag)
 cp 2
 ret nz
 ld (hl),a

 ld bc,$2003 
 ld hl,Enemies_On_Screen
checkballs:
 ld a,(hl)
 cp 21
 jr c,skipballs
 dec c
 ld a,c
 or a
 ret z
skipballs:
 ld de,7
 add hl,de
 djnz checkballs

 call FindESlot
 ret nz

 ld a,(direction)
 ld b,21
 add a,b
 ld (hl),a
 inc hl
 ld (hl),4 ;start by moving down
 inc hl
 ld a,(xpos)
 add a,5
 ld (hl),a
 inc hl
 ld a,(ypos)
 add a,5
 ld (hl),a  ;done!!!
 ret

testanimated:
 ld a,(ix+1)
 and %111
 cp 3
 jr z,notanimated
 jr doanimate 

DrawEnemies:
 ld hl,Enemies_On_Screen
 ld b,32
drawenemy_loop:
 push bc
 push hl
 ld a,(hl)
 or a
 jr z,SkipE
 ld a,(hl)
 cp 8
 jr c,NoBulletTest
 cp 11
 jr nc,NoBulletTest
BulletTest:
 inc hl
 ld a,(hl)
 or a
 jp z,SkipE
 dec hl
 ld a,(hl)
NoBulletTest:
 push af
 push hl
 dec a
 ld e,a
 ld d,0
 inc hl
 ld a,(hl)
 ld (revflag),a
 inc hl
 ld b,(hl)
 inc hl
 ld c,(hl)
 call LoadEHeight2
 ld e,(hl)
 inc hl
 ld a,(hl)  ;width
 ld (widthflag),a
 inc hl
 pop ix
 pop af

 cp 18
 jr z,animated
 cp 19
 jr z,animated
 cp 11
 jr z,animated
 cp 5
 jr z,testanimated
 cp 7
 jr z,animated
 cp 3
 jr nc,notanimated
animated:
 ld a,0
ecounter = $-1
 and %1
 jr z,notanimated

doanimate:
 ld d,0
 add hl,de
 add hl,de
notanimated:
 ld a,0
revflag = $-1
 and %111
 cp 2
 push ix
 call z,RevSprite2
 pop ix

 ld a,0
widthflag = $-1
 cp 9
 jr nc,WideSprite

 ld a,(ix)
 cp 4
 jr z,drawplant

 ld a,e
 call PutSpriteMask
SkipE:
 pop hl
 ld de,7
 add hl,de
 pop bc
 djnz drawenemy_loop
 ret

drawplant:
 ld a,e
 LD (__Change_4+2), A
 ld a,(ix+6) ;load timer
 dec a
 cp 100
 jr nc,SkipE
 cp 12
 jr c,noset12
 ld a,11
noset12:
 inc a
 call PutSpriteMask2
 jr SkipE

WideSprite:
 ld a,e
 call PutSpriteMaskW
 jr SkipE

MoveEnemies: 
 ld a,(animcounter)
 and %11
 jr nz,NoIncECounter
 ld hl,ecounter
 inc (hl)
NoIncECounter:
 ld hl,Enemies_On_Screen
 ld b,32
moveenemy_loop:
 push bc
 push hl
 ld a,(hl)
 or a
 jr z,SkipME   ;no enemy
 dec a
 jr z,GoombaAI ;Goomba
 dec a
 jp z,TurtleAI ;Turtle
 dec a
 jp z,ShellAI  ;Shell
 dec a
 jp z,PlantAI  ;Plant
 dec a
 jp z,FireBallAI
 dec a
 jp z,PoundAI
 dec a
 jp z,FTurtleAI
 dec a
 jp z,BulletAI
 dec a
 jp z,BulletVAI
 dec a
 jp z,BulletVAI
 dec a
 jp z,FishAI
 dec a
 jp z,BowserAI
 dec a
 jp z,EndBarAI
 dec a
 jp z,PaddleVAI
 dec a
 jp z,PaddleHAI
 dec a
 jr z,MushroomAI
 dec a
 jr z,MushroomAI
 sub 3
 jp z,PlantFireAI
 dec a
 inc hl \ inc hl
 jp z,MFireAI
 dec a
 jp z,MFireAI2
SkipME:
 pop hl
 ld de,7
 add hl,de
 pop bc
 djnz moveenemy_loop
 ret

MushroomAI:
GoombaAI:
 xor a
 ld (shellflag),a
ShellAI2:
 push hl
 push bc
 push hl
 call CheckDownE
 pop hl
 pop bc
 jp nz,clearEfall
Efallthrough:
 inc hl
 push hl
 ld de,5
 add hl,de
 call FindJumpValue2
 pop hl   

 inc hl
 inc hl  ;hl = ypos of E
 dec (hl)
Efallloop:
 inc (hl)
 push bc
 push hl
 dec hl
 dec hl
 dec hl
 call CheckDownE
 pop hl
 pop bc
 jr nz,clearEfall2
 djnz Efallloop
 inc hl
 inc hl
 inc hl ;timer
 ld a,(hl)
 inc a
 cp 21
 jr nc,DoneFall
 ld (hl),a
 jr DoneFall

clearEfall2:
 dec hl
 dec hl
 dec hl
clearEfall:
 ld de,6
 add hl,de
 ld (hl),0
DoneFall:
 pop hl

GoombaAI2:
 ld a,0
shellflag = $-1
 or a
 jr nz,GoombaAI3

 ld a,(animcounter)
 and %11
 jr nz,SkipME
GoombaAI3:
 inc hl
 ld a,(hl)
 dec hl
 or a
 jr z,SkipME
 dec a
 jr z,MoveELeft
MoveERight:
 push hl
 call CheckRightE
 pop hl
 jr nz,ChangeEDir
 inc hl
 inc hl
 inc (hl)
CheckIfGone:
 ld a,(hl)
 sub 164
 call z,kille2
 jr SkipME

MoveELeft:
 push hl
 call CheckLeftE
 pop hl
 jr nz,ChangeEDir
 inc hl
 inc hl
 dec (hl)
 jr CheckIfGone

ChangeEDir:
 call ChangeEDir3
 jp SkipME

ChangeEDir3:
 inc hl
 ld a,(hl)
 dec a   ;0-1
 xor %1  ;1-0
 inc a   
 ld (hl),a
 ret

CheckChangeEDir2:
 inc hl
 ld a,(hl)
 and %111
 dec hl         ;not moving...
 jp z,GoombaAI
 ld (shellflag),a
 push hl
 call ChangeEDir3
 pop hl
 jp GoombaAI2

TurtleAI:
 push hl
 call CheckDownE
 pop hl
 jr z,CheckChangeEDir2

 inc hl
 ld a,(hl)
 or a
 jr nz,FishAI
 ld (hl),1
FishAI:
 dec hl
 xor a
 ld (shellflag),a
 jp GoombaAI2

ShellAI:
 ld a,1
 ld (shellflag),a
 jp ShellAI2

PaddleHAI:
 ld a,(animcounter)
 and %11
 jp nz,SkipME
 inc hl
 ld a,(hl)
 dec hl
 dec a
 jr z,MovePLeft
MovePRight:
 push hl
 call CheckRightE
 pop hl
 jr nz,ChangePDir
 inc hl
 inc hl
 inc (hl)
 ld a,(hl)
 sub 164
 call z,kille2
 push hl
 inc hl
 inc hl ;init x
 ld a,(hl) ;initx
 pop hl
 add a,56
FinalHPaddleCheck:
 ld b,(hl)
 cp b
 dec hl
 dec hl
 jr z,ChangePDir
 jp SkipME

MovePLeft:
 push hl
 call CheckLeftE
 pop hl
 jr nz,ChangePDir
 inc hl
 inc hl
 dec (hl)
 ld a,(hl)
 sub 164
 call z,kille2
 push hl
 inc hl
 inc hl ;init x
 ld a,(hl) ;initx
 dec a
 pop hl
 jr FinalHPaddleCheck

ChangePDir:
 call ChangeEDir3
 dec hl
 jp GoombaAI3

ResetBullet:
 ld (hl),a
 dec hl
 dec hl
 ld a,(hl)
 cp 88
 ret nc  ;don't restart if offscreen
 inc hl
 ld c,(hl)
 dec hl
 dec hl
 ld (hl),c
 dec hl
 ld (hl),a
 ld b,a
 dec hl
 dec hl
 ld a,(hl)
 inc hl
 ld c,1
 cp 8
 jr nz,fireleft
FindBulDir:
 ld c,1
 ld a,(xpos)
 cp b
 jr c,fireleft
 inc c
fireleft:
 ld (hl),c
 ret    ;Bullet Reset

BulletVAI:
 ld a,(hl)
 add a,a
 sub 19
 ld b,a

 ld a,(animcounter)
 and %1
 jp nz,SkipME

 push hl
 inc hl
 inc hl
 inc hl ;y coord
 ld a,(hl)
 add a,b
 cp 64
 jr c,BulletOnScreen
 push hl
 dec hl
 dec hl
 ld (hl),0
 pop hl
BulletOnScreen:
 ld (hl),a
 pop hl
 jr BulletAI2

BulletAI:
 ld a,(animcounter)
 and %11
 jp z,SkipME
BulletAI2:
 push hl
 ld de,6
 add hl,de ;load the timer
 inc (hl)
 ld a,(hl)
 sub 128   ;check timer
 call z,ResetBullet
 pop hl

 inc hl
 ld a,(hl)
 inc hl
 dec a
 jr nz,MoveBulRight
 dec (hl)
 dec (hl)
MoveBulRight:
 inc (hl)
 jp SkipME

FTurtleAI:
 ld a,24
 jr EndBarAI2

PaddleVAI:
 ld a,32
 jr EndBarAI2

EndBarAI:
 ld a,40
EndBarAI2:
 ld (LowHeight),a
 ld a,(animcounter)
 and %11
 jp nz,SkipME

 inc hl
MoveBar:
 ld a,(hl)
 inc hl
 inc hl
 push hl
 inc hl
 inc hl ;now init y
 ld b,(hl)
 pop hl
 cp 4
 jr z,MoveEDown
MoveEUp:
 ld a,(hl)
 sub b
 jr z,ChangeVDir
 dec (hl)
 jp SkipME

MoveEDown:
 ld a,(hl)
 sub 40
LowHeight = $-1
 cp b
 jr z,ChangeVDir
 inc (hl)
 jp SkipME

ChangeVDir:
 dec hl
 dec hl
 ld a,(hl)
 and %1
 add a,3
 ld (hl),a
 jp SkipME

PoundAI:
 inc hl ; skip type
 ld a,(hl)
 or a
 jr z,testMarioBeneath
 cp 3
 jr z,Returning
PoundAttack:
 inc hl
 inc hl  ;y coord
 ld a,(hl)
 inc a
 ld (hl),a
 dec hl
 dec hl
 push hl
 dec hl
 call CheckDownE
 pop hl
 jp z,SkipME
 ld (hl),3 ;moving up
 jp SkipME

Returning:
 inc hl
 inc hl
 dec (hl)
 ld a,(hl)
 dec hl
 dec hl

 push hl
 ld de,5
 add hl,de ;now points to original location
 ld b,(hl)
 sub 8
 sub b
 pop hl
 jp nz,SkipME
 ld (hl),0
 jp SkipME


testMarioBeneath:
 inc hl
 ld a,(xpos)
 sub (hl)
 cp 16
 jr c,SetPoundA
 neg
 cp 8
 jp nc,SkipME
SetPoundA:
 dec hl
 ld (hl),4 ;moving down
 jp SkipME


BowserAI:
 inc hl
 push hl
 pop ix

 call MoveLeftorRight

 ld a,(animcounter)
 and %1
 jp nz,SkipME

 push ix
 ld e,(ix+5)  ;timer holds fall counter
 call FindJumpValue
 dec b
 pop hl
 dec hl ;so CheckDownE works

 ld a,(ix)
 cp 3
 jr nc,MoveBUp
MoveBDown:
 inc b
 dec (ix+2)
BowserDownLoop:
 inc (ix+2)
 push ix
 push bc
 push hl
 call CheckDownE
 pop hl
 pop bc
 pop ix
 jp nz,ChangeBUp
 djnz BowserDownLoop

 ld a,(ix+5)
 cp 23
 jp z,SkipME
 inc (ix+5)
 jp SkipME

MoveBUp:
 ld a,(ix+2)  ;hl = ypos of E
 sub b
 ld (ix+2),a
 ld a,(ix+5)
 dec a
 call z,ChangeBDown
 ld (ix+5),a
 jp SkipME

ChangeBUp:
;now set direction
 ld hl,xpos
 ld a,(ix+1) ;x coord
 cp (hl)
 jr nc,BowLeft
BowRight:
 ld a,4
SetBowDir:
 ld (ix),a
 ld a,18
 ld (ix+5),a
 jp SkipME

BowLeft:
 ld a,3
 jr SetBowDir
 
ChangeBDown:  ;still move him in same X direction
 dec (ix)
 dec (ix)
 ret

MoveLeftorRight:
 ld a,(animcounter)
 and %11
 ret nz

 ld hl,xpos
 ld a,(ix+1) ;x coord
 sub (hl)
 neg
 cp 8
 ret c ;he's beneath
 ld a,(ix+1)
 cp (hl)
 jr nc,MoveBowLeft
MoveBowRight:
 ld a,(ix) ;cp direction
 and %1
 ret nz
 inc (ix+1)
 ret
MoveBowLeft:
 ld a,(ix) ;cp direction
 and %1 ;right
 ret z
 dec (ix+1)
 ret

MFireAI:
 inc (hl) ;x coord
 push hl
 dec hl
 dec hl
 call CheckRightE
 jr MFireAI3

MFireAI2:
 dec (hl) ;x coord
 push hl
 dec hl
 dec hl
 call CheckLeftE
MFireAI3:
 pop hl
 jr z,KeepMoving
MFireAI4:
 push hl
 dec hl
 dec hl
 ld (hl),0
 pop hl
 jp SkipME

KeepMoving:
 ld a,(hl)
 cp 86
 jr nc,MFireAI4
 dec hl ;now the direction
 ld a,(hl)
 cp 4
 inc hl
 inc hl ;y coord
 jr z,MoveEDown2
MoveEUp2:
 dec (hl)
 inc hl
 inc hl
 inc hl ;now points to timer
 dec (hl)
 ld a,(hl)
 or a
 jp nz,SkipME
SwitchFire:
 ld de,-5
 add hl,de
 ld (hl),4 ;moving down
 jp SkipME

MoveEDown2:
 inc (hl)
 push hl
 dec hl
 dec hl
 dec hl
 call CheckDownE
 pop hl
 jp z,SkipME
SwitchFire2:
 dec hl
 dec hl
 ld (hl),3
 ld de,5
 add hl,de
 ld (hl),8  ;only go up 8
 jp SkipME

FireBallAI:
 ld a,(animcounter)
 and %1
 jp nz,SkipME

 inc hl
 ld a,(hl)

 push hl
 pop ix

 ld de,5
 add hl,de ;


 and %111 ;get right info
 cp 3
 jr z,MoveFBup

MoveFBdown:
 push hl
 call FindJumpValue2
 ld e,(hl)
 ld a,(ix+2)  ;hl = ypos of E
 add a,e
 ld (ix+2),a
 pop hl 

 ld a,(hl)
 inc a
 cp 24
 jr nc,ChangeVDir2
 ld (hl),a
 jp SkipME

ChangeVDir2:
 ld a,24
ChangeBDir:
 ld (hl),a
ChangeVDir4:
 ld a,(ix)
 and %1
 add a,3
 ld (ix),a
 jp SkipME

ChangeVDir3:
 xor a
 jr ChangeBDir

MoveFBup:
 ld a,(ix+4)
 ld c,a

 push hl
 dec (hl)
 call FindJumpValue2
 pop hl

 inc (ix+2)  ;go to y coord 
Ejumploop:
 ld a,(ix+2)
 dec a
 ld (ix+2),a
 cp c
 jp z,ChangeVDir3
 djnz Ejumploop
 jp SkipME

PlantAI:
 ld a,(animcounter)
 and %111
 jp nz,SkipME

 inc hl
 ld a,(hl)
 inc hl
 inc hl
 cp 4
 jr z,PlantDown
PlantUp:
 ld e,l
 ld d,h

 inc hl
 inc hl
 inc hl
 inc (hl)  ;increase timer
 ld a,(hl)
 cp 13
 jp nc,NoIncY

 push hl
 ex de,hl
 dec (hl)  ;decrease y coord
 pop hl
NoIncY:
 cp 16
 jp nz,SkipME
 ld (hl),12 ;set default

 push hl
 call FirePlant
 pop hl

SwitchPlantDir:
 ld de,-5
 add hl,de ;back to dir
 ld a,(hl)
 and %1
 add a,3
 ld (hl),a
 jp SkipME

PlantDown:
 inc (hl)
 inc hl
 inc hl
 inc hl
 dec (hl)
 ld a,(hl)
 cp -4  ;timer at -4
 jp nz,SkipME

 push hl
 pop ix
 ld a,(ix-3)
 sub 4
 ld (ix-3),a

 ld (hl),0 ;set default
 jr SwitchPlantDir

FirePlant:
 push hl
 pop ix

 call FindESlot
 ret nz

 ld (hl),20
 inc hl
 ld a,(ix-4)
 ld b,a
 call FindBulDir
 inc hl
 ld a,(ix-4)
 add a,3
 ld (hl),a
 inc hl
 ld a,(ix-1)
 ld (hl),a
 ret  ;done!!!

FindESlot:
 ld hl,Enemies_On_Screen
 ld b,32
FindEmptySlot:
 ld a,(hl)
 or a
 ret z
 ld de,7
 add hl,de
 djnz FindEmptySlot
 or a
 ret


PlantFireAI:
 ld a,(animcounter)
 and %11
 jp nz,SkipME
 inc hl
 push hl
 inc hl
 inc hl
 dec (hl)
 ld a,(hl)
 pop hl
 cp -12
 jr nz,NotPastScreen
 dec hl
 ld (hl),0 ;set zero
 jp SkipME
NotPastScreen:
 ld a,(hl)
 inc hl
 dec a
 jr nz,MoveFRight
MoveFLeft:
 dec (hl)
 dec (hl)
MoveFRight:
 inc (hl)
 jp SkipME

RevSprite2:
 ld a,(ix)
 cp 12
 ret z  ;don't rev bowswer
 ld a,e
 push de
 add a,a
 push bc
 call RevSprite
 pop bc
 pop de
 ret

drawinitmap:
 ld hl,0
compressedmaps = $-2
 ld a,(level)
 ld b,(hl)
 inc b
 cp b
 jp z,endgame
 inc hl

 push hl
 dec a
 add a,a
 ld e,a
 ld d,0
 add hl,de
 bcall(_ldhlind)
 ex de,hl
 pop hl
 ld bc,50
 add hl,bc
 add hl,de

 push hl
 bcall(_ldhlind)
 ld (decompressT),hl
 ld a,8
 bcall(_divhlbya)  ;divide by 8
 ld a,l
 ld (maplength),a
 pop hl

 inc hl
 inc hl

 push hl
 bcall(_ldhlind)
 ld (et_off),hl
 pop hl
 inc hl
 inc hl

 push hl
 ld bc,0
decompressT = $-2
 ld de,map
 call Decompress
 pop hl

 ld de,0
et_off = $-2
 add hl,de
 inc hl
 inc hl
 ld bc,300
 ld de,enemy_table
 call Decompress
 
 ld hl,SideBar
 ld de,GRAPH_MEM+10
 ld a,64
 ld (updateflag),a
drawsideloop:
 ldi
 ldi
 ex de,hl
 ld bc,10
 add hl,bc
 ex de,hl 
 dec a
 jr nz,drawsideloop

 call CheckEndLevel

DrawingInitMap:
 ld hl,map
 ld de,GRAPH_MEM
 ld b,8
drawmapv:
 push bc
 ld b,10
drawmaph:
 push bc
 push hl
 push de
 push de
 call FindSpriteHL
 pop de
 ld b,8
draw8x8:
 ldi
 inc bc
 push hl
 ld hl,11
 add hl,de
 ex de,hl
 pop hl
 djnz draw8x8
 pop de
 inc de
 pop hl
 inc hl
 pop bc
 djnz drawmaph
 ld a,(maplength)
 sub 10
 ld c,a
 add hl,bc

 push hl
 ld hl,86
 add hl,de
 ex de,hl
 pop hl

 pop bc
 djnz drawmapv

 ld hl,Enemies_On_Screen
 ld c,223
 call OTH_CLEAR  ;clear this

 ld a,-11
 ld (map_offset),a
 xor a
 ld (initx),a
 ld b,11
CheckFirstLoop:
 ld a,c
 ld (initx),a
 add a,8
 ld c,a
 push bc
 call TestForNewEnemies
 pop bc
 ld hl,map_offset
 inc (hl)
 djnz CheckFirstLoop
 ret

UpdateAnimatedTiles:
 ld d,a
 ld b,a
 ld c,a
 ld hl,map
 call AddMapOffsetHL
 ld a,8    ;8 rows
UpdateRow:
 push af
 ld a,11   ;11 columns
UpdateCol:
 push af
 ld a,(hl)
 sub 17
 jr z,animateblock
 dec a
 jr z,animateblock
 dec a
 jr z,animateblock
 dec a
 jr z,animateblock
DontAnimate:
 inc hl
 ld a,b
 add a,8
 ld b,a
 pop af
 dec a
 jr nz,UpdateCol

 ld b,a ; b = 0
 ld d,a ; d = 0
 ld a,c
 add a,8
 ld c,a

 ld a,(maplength)
 cp 10
 jr z,nomod
 sub 10
 ld e,a
 add hl,de
nomod:
 dec hl
 pop af
 dec a
 jr nz,UpdateRow
 ret

animateblock:
 push hl
 push bc
 ld a,(scroll_x)
 ld e,a
 ld a,b
 sub e
 ld b,a

 ld hl,questionblocks-8
 ld a,(blockcounter)
 srl a
 srl a
 inc a
 add a,a
 add a,a
 add a,a ;x8
 ld e,a
 add hl,de

 push bc
 ld de,QuestionBlock+8
 ld bc,8
 ldir
 ld hl,QuestionBlock
 pop bc
 call MASK8x8
 pop bc
 pop hl
 jr DontAnimate

LoadClip:
 ld (rightclipx),a
 sub 7
 ld (rightclipx2),a
 ret

FindSpriteHL:
 ld l,(hl)
 ld h,0
 add hl,hl
 add hl,hl
 add hl,hl
 ld de,bgtiles
 add hl,de
 ret

PutSpriteHL:
 call FindSpriteHL
PutSprite:
 push hl
 push bc
 call PutSprClpXOR
 pop bc
 pop hl
 ret

PutSprClpXOR:
			    XOR  A
__XChange_1:    LD   DE, 8     ; D = 0, E = Height

                OR   C                            ; If C < 0
                JP   M, _SCX_NoBotClp             ; No bottom clip.

                LD   A, $3F                       ; Is C is offscreen?
                SUB  C
                RET  C

__XChange_2:    CP   8-1       ; If C + 7 < 64
                JR   NC, _SCX_NoVertClp           ; No vertical clip.
                INC  A
                LD   E, A
                JR   _SCX_NoVertClp               ; Height = 64 - C

_SCX_NoBotClp:
__XChange_3:    CP   -(8-1)    ; Is C is offscreen?
                RET  C

                ADD  A, E                         ; Find how many lines
                LD   C, A                         ; to actually draw
                SUB  E

                NEG
                LD   E, A
                ADD  HL, DE                       ; Move HL down
                LD   E, C                         ; by -C lines
                LD   C, D

_SCX_NoVertClp: PUSH HL                           ; IX -> Sprite
                POP  IX

                LD   A, $77                       ; OP code for
                LD   (_SCX_OPchg_1), A            ;   LD   (HL), A
                LD   (_SCX_OPchg_2), A

                XOR  A                            ; Is B > 0?
                OR   B
                JP   M, _SCX_NoRightClp

                CP   80-7                           ; Is B < 89?
rightclipx2 = $-1
                JR   C, _SCX_ClpDone
                CP   80
rightclipx = $-1
                RET  NC

                LD   HL, _SCX_OPchg_1             ; Modify LD to NOP
                JR   _SCX_ClpModify

_SCX_NoRightClp:CP   -7                           ; Is B is offscreen?
                RET  C

                LD   HL, _SCX_OPchg_2             ; Modify LD to NOP
_SCX_ClpModify: LD   (HL), D

_SCX_ClpDone:   LD   B, D
                LD   H, B
                LD   L, C
                ADD  HL, BC                       ; HL = Y * 12
                ADD  HL, BC
                ADD  HL, HL
                ADD  HL, HL

                LD   C, A                         ; HL = Y*12 + X/8
                SRA  C
                SRA  C
                SRA  C
                INC  C

                ADD  HL, BC
                LD   BC, GRAPH_MEM
                ADD  HL, BC

                LD   B, E                         ; B = number of rows

                CPL
                AND  %00000111                    ; find number of
                LD   E, A                         ; instructions to jump
                ADD  A, E
                ADD  A, E
                LD   (_SCX_OPchg_3 + 1), A        ; 3 * (7 - number)

                LD   DE, 13

_SCX_LineLoop:  LD   C, (IX)
                XOR  A
_SCX_OPchg_3:   JR   _SCX_OPchg_3                 ; modify

                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA
                RR   C
                RRA

OPCHANGE4:
                OR  (HL)                         ; XOR with background
_SCX_OPchg_1:   LD   (HL), A                      ; Write
                DEC  HL                           ; HL -> next 8 pixels

                LD   A, C
OPCHANGE5:
                OR  (HL)                         ; XOR with background
_SCX_OPchg_2:   LD   (HL), A                      ; Write
                ADD  HL, DE                       ; HL -> next row

                INC  IX                           ; Increment to next data
                DJNZ _SCX_LineLoop
                RET

DnEMario:
 ld hl,blockcounter
 inc (hl)
 ld a,(hl)
 sub 20
 jr nz,noreset
 ld (hl),a
noreset:
 and %11
 call z,UpdateAnimatedTiles

 call CopyGRAPHtoAPD

 ld a,0
invincibility = $-1
 or a
 jr z,DrawSprite

 ld a,(animcounter)
 ld hl,invincibility
 and %1
 jr z,NoDecInv
 dec (hl)
NoDecInv
 ld a,(hl)
 and %1
 jr z,NoNoRevSprite
DrawSprite:
 ld hl,MarioSprites-72

 ld a,(bigflag)
 ld b,a
 inc b
BigMario:
 ld de,72
 add hl,de
 djnz BigMario

 ld a,(runflag)
 or a
 jr z,NotRunning
 ld e,24
 add hl,de
 ld a,0
animcounter = $-1
 and %1
 jr z,NotRunning
 add hl,de
NotRunning:
 xor a
 ld (runflag),a

 ld a,(direction)
 or a
 jr z,NoRevSprite
 ld a,24
 call RevSprite
NoRevSprite:
 ld a,12
 ld bc,(ypos)
 call PutSpriteMask
NoNoRevSprite:
 ld hl,animcounter
 inc (hl)
 call DrawEnemies
 ld hl,updateflag
 ld a,(hl)
 ld (hl),0
 or a
 jr z,SpecialCopy
 call fastCopy
DoneCopy:
 ld de,GRAPH_MEM
 ld hl,APD_BUF
 jr Copy768

CopyGRAPHtoAPD:
 ld hl,GRAPH_MEM
 ld de,APD_BUF
Copy768:
 ld bc,768
FastLDI_Loop:
 ldi \ ldi \ ldi \ ldi
 ldi \ ldi \ ldi \ ldi
 ldi \ ldi \ ldi \ ldi
 ldi \ ldi \ ldi \ ldi
 jp pe,FastLDI_Loop
 ret

SpecialCopy:
	di
	ld	a,$80				; 7
	out	($10),a				; 11
	ld	hl,GRAPH_MEM-12-(-(12*64)+1)		; 10
	ld	a,$20				; 7
	ld	c,a				; 4
	inc	hl				; 6 waste
	dec	hl				; 6 waste
fastCopyAgain:
	ld	b,64				; 7
	inc	c				; 4
	ld	de,-(12*64)+1			; 10
	out	($10),a				; 11
	add	hl,de				; 11
	ld	de,10				; 10
fastCopyLoop:
	add	hl,de				; 11
	inc	hl				; 6 waste
	inc	hl				; 6 waste
	inc	de				; 6
	ld	a,(hl)				; 7
	out	($11),a				; 11
	dec	de				; 6
	djnz	fastCopyLoop			; 13/8
	ld	a,c				; 4
	cp	$2A				; 7
	jr	nz,fastCopyAgain		; 10/1
    jr DoneCopy

MASK8x8:
 ld a,8
PutSpriteMask:
                LD   (__Change_4+2), A
PutSpriteMask2:
				LD   (__Change_1+1), A
                DEC  A
                LD   (__Change_2+1), A
                NEG
                LD   (__Change_3+1), A

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
ChangeWidth4 = $-1
                nop
ChangeWidth3 = $-1
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

                CP   80-7                           ; Is B < 89?
                JR   C, _PSC_ClpDone
                CP   80
                RET  NC

                LD   HL, _PSC_OPchg_1             ; Modify LD to NOP
                JR   _PSC_ClpModify

_PSC_NoRightClp: CP   -7                           ; Is B is offscreen?
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
__Change_4:     LD   A, (IX+8)
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

                LD   A, (IX+0)
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
ChangeWidth2 = $-2
                nop \ nop
ChangeWidth1 = $-2
                DJNZ _PSC_LineLoop
                RET

PutSpriteMaskW:
 push af
 push hl
 push bc

 push hl
 push af
 ld hl,(ChangeWidth2)
 ld (ChangeWidth1),hl

 push af
 ld a,(ChangeWidth4)
 ld (ChangeWidth3),a
 pop af

 add a,a
 ld (__Change_4+2), A
 pop af
 pop hl

 call PutSpriteMask2
 pop bc
 ld a,8
 add a,b
 ld b,a
 pop hl
 inc hl
 pop af
 call PutSpriteMask2

 xor a
 ld l,a
 ld h,a
 ld (ChangeWidth1),hl
 ld (ChangeWidth3),a
 ret

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

updatestats:
 ld de,$1253
 ld hl,lives
 call DM_HL_DECI

 ld d,$32
 ld hl,(coins)
 call DM_HL_DECI2

 ld d,$22
 ld hl,level

DM_HL_DECI:           ; Display HL in menu style with leading zeros
 ld l,(hl)            ;Stuff added to save bytes
 ld h,0
DM_HL_DECI2:
 ld (pencol),de
 ld b,3
DM_HL_DECI3:
 push de
;Routine Starts here
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
 ret



DMX:
 ld (pencol),de
DMX2:
 bcall(_vputs)
 ret

DTX:
 ld (currow),de
 bcall(_puts)
 ret

ID: 
	.db $AA,$BB,0

Loadlevel:
 bcall(_cleargbuf)
 ld de,$0015
 ld hl,TitleTxt
 call DMX
 ld de,$3901
 ld hl,AuthorTxt
 call DMX
 ld hl,GRAPH_MEM
 call Invert
 ld hl,GRAPH_MEM+684
 call Invert

 ld hl,(progptr)
SearchLoop:
 ld ix,ID
 call Detect
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
 call fastCopy
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
 jr c,SetMax
 ld a,8
SetMax:
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
 ld de,28
 add hl, de		; skip length of program + header bytes + program desc + password
 ld (compressedmaps),hl
 ret			; return hl = pointer to the program.

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
 jp fastCopy

LoadLevelA:
 ld b,a
 ld de,(progptr)
LoadLevelB2:
 ex de,hl
 ld ix,ID
 push bc
 call Detect
 pop bc
 djnz LoadLevelB2
 ret

OTH_ARROW:      ;Altered version of the Usgard routine
                ;ALPHA will be bit 4, instead of Y=
 ld a,$DF
 out (1),a
 ld c,%11111111 ;slight delay
 in a,(1)
 cp 127
 jr nz,nosetbit4
 res 4,c
nosetbit4:
 LD A,$fe
 OUT ($01),A ; Port 1 Keypad
 NOP \ NOP
 IN A,($01) ; Port 1 Keypad
 LD B,A
 LD A,$ff
 IN A,($01) ; Port 1 Keypad
 LD A,B
 LD A,$bf
 OUT ($01),A ; Port 1 Keypad
 IN A,($01) ; Port 1 Keypad
 OR $0f
 AND B
 AND C
 ret

RevSprite:
 ld ix,tempsprite
 push ix
 ld b,a
revBYTE:
 ld e,(hl)
 ld a,8
 ld d,0
revBYTEloop:
 srl e		; destroy a bit // set carry if it was a ONE
 rl d		; place the bit ONTO a new byte
 dec a
 jr nz,revBYTEloop
 ld (ix),d
 inc ix
 inc hl
 djnz revBYTE
 pop hl
 ret

OTH_CLEAR2:
 ld bc,767
OTH_CLEAR:
 ld e,l
 ld d,h
 inc de
 ld (hl),0
 ldir
 ret


;
;Mario Collision Detection Routines
;
FindJumpValue2:
 ld e,(hl)
FindJumpValue:
 ld d,0
 ld hl,JumpFallTable
 add hl,de
 ld b,(hl)
 inc b
 ret

CheckBelowCollision:
 xor a
 ld (jumpflag),a
 call CheckDownM
 jr z,fallthrough
clearfall:
 call CheckEwithM
clearfall2:
 xor a
 ld (fallflag),a
 jr fallthroughdone

fallthrough:
 ld a,(fallflag)
 ld e,a
 call FindJumpValue
 ld a,b
 ld (fallcount),a
 ld hl,ypos
 dec (hl)
fallloop:
 ld hl,ypos
 inc (hl)
 xor a
 ld (eflag),a
 call CheckEwithM
 ld a,0
eflag = $-1
 or a
 jr nz,clearfall2

 call CheckDownM
 jr nz,clearfall2

 ld a,0
fallcount = $-1
 dec a
 ld (fallcount),a
 jr nz,fallloop

 ld hl,fallflag
 ld a,(hl)
 cp 21
 jr z,fallthroughdone
 inc (hl)
fallthroughdone:
 jp CheckDelay

;
; Collision Detection Routines For All
;

CheckShellswithE:
 ld hl,Enemies_On_Screen
 ld b,32
ShellLoop:
 push bc
 push hl
 ld a,(hl)
 cp 21
 jr nc,SortofShell
 cp 3
 jr nz,SkipNonShell
SortofShell:
 ld (ShellTemp),hl
 inc hl
 ld a,(hl)
 or a
 jr z,SkipNonShell ;not moving
 ld a,5
 ld (h1),a
 inc hl
 ld a,(hl)
 ld (x1),a
 inc hl
 ld a,(hl)
 call CheckEnemies
SkipNonShell:
 pop hl
 pop bc
 ld de,7
 add hl,de
 djnz ShellLoop
 ret

LoadEHeight:
 ld e,(hl)
 dec e
 ld d,0
LoadEHeight2:
 ld hl,enemy_sprites
 add hl,de
 add hl,de
 bcall(_ldhlind) ;find height
 ret

seta4:
 ld a,4
 ret

CheckEwithM:
 xor a
 ld (mover),a
 ld a,(xpos)
 ld (x1),a
 ld a,(bigflag)
 or a
 call nz,seta4
 ld b,a
 add a,8
 ld (h1),a  ;height of Mario
 ld a,b
 sub 4
 neg
 ld b,a
 ld a,(ypos)
 add a,b

CheckEnemies:
 ld (y1),a
 ld a,8
 ld (w1),a
 ld hl,Enemies_On_Screen
 ld b,32
checkcloop:
 push bc
 push hl
 ld a,(hl)
 or a
 jr z,SkipC
 push hl
 pop ix
 cp 4
 jr nz,NotaPlant
 ld a,(ix+6)
 dec a
 cp 20
 jr nc,SkipC
NotaPlant:
 push hl
 call LoadEHeight
 ld e,(hl)      ;put it in A
 ld a,(ix)
 cp 4
 jr nz,nofixplant
 ld a,(ix+6)
 ld e,a
 cp 12
 jr c,nofixplant
 ld e,12
nofixplant:
 ld a,e
 ld (h2),a   ;store enemy height
 inc hl
 ld a,(hl)
 ld (w2),a   ;store enemy width
 pop hl

 inc hl
 inc hl

 ld a,(hl)
 ld (xadj),a
 add a,16
 cp 89
 jr nc,SkipC  ;offscreen

 ld de,(w1)

 ld a,(x1)
 sub (hl)
 cp d
 jr c,testY
 neg
 cp e 
 jr c,testY
SkipC:
 pop hl
 ld de,7
 add hl,de
 pop bc
 djnz checkcloop
 ret

ShellKill:
 ld a,(hl)
 cp 3
 jr c,GoodEnemy
 cp 4
 jr z,GoodEnemy
 cp 7
 jr z,GoodEnemy
 cp 11
 jr nz,SkipC
GoodEnemy:
 ld (hl),0
 ld hl,0
ShellTemp = $-2
 ld a,(hl)
 cp 21
 jr c,SkipC
 ld (hl),0
 jr SkipC
 
testY:
 ld de,(h1)
 inc hl
 ld a,(hl)
 add a,6
 ld (yadj),a

 ld a,(y1)
 sub (hl)
 cp d
 jr c,hitE
 neg
 cp e
 jr nc,SkipC
hitE:
 dec hl
 dec hl
 dec hl
 ld a,(mover)
 or a
 jr nz,ShellKill

 ld a,(fallflag)
 or a
 jp nz,killE
NoKillE:
 ld a,(hl)
 sub 3
 jp z,PushShell
 sub 5
 jp z,BulletTest2
 dec a
 jp z,BulletTest2
 dec a
 jp z,BulletTest2 
 sub 3
 jp z,endlevel
 dec a
 jp z,PaddleV_Hit
 dec a
 jp z,PaddleH_Hit
 dec a
 jr z,MakeBig
 dec a
 jr z,GainLife
 dec a
 jr z,Invincible
 dec a
 jr z,MakeFlower
 sub 2
 jr z,SkipC
 dec a
 jp z,SkipC
DoDie:
 ld a,(invincibility)
 or a
 jp nz,SkipC

 ld hl,bigflag
 ld a,(hl)
 or a
 jr nz,NoDie

 pop hl
 pop bc
 inc sp
 inc sp
 jp YouDie

NoDie:
 dec (hl)
 ld a,36
 jr SetInvincible

Invincible:
 ld a,175
 ld (hl),0
SetInvincible:
 ld (invincibility),a
 jp SkipC

GainLife:
 ld (hl),0
 ld hl,lives
 inc (hl)
 jp SkipC

MakeFlower:
 inc a
MakeBig:
 inc a
 ld (bigflag),a

 ld (hl),0
 ld b,8
FlashLoop2:
 push bc
 ld hl,GRAPH_MEM
 ld bc,768
 call InvLoop ;invert screen
 call FlashDelay
 pop bc
 djnz FlashLoop2
 jp SkipC

PushShell:
 inc hl
 ld a,(hl)
 or a
 jr nz,DoDie

 ld a,(direction)
 xor %1
 inc a
 ld (hl),a
 inc hl
 dec a
 jr nz,MoveSRight
 ld a,(hl)
 sub 8
 ld (hl),a
MoveSRight:
 ld a,(hl)
 add a,4
 ld (hl),a
 jp SkipC

BulletTest2:
 inc hl
 ld a,(hl)
 or a   ;Bullets are always moving
 jr nz,DoDie
 jp SkipC  ;if not, then it's dead

endlevel:
 res 7,(iy+$14)
 push hl
 pop ix

 ld b,(ix+5)
 ld a,(ix+3)
 sub b
 sub 41
 neg
 push af
 push af
 ld hl,Bonus
 ld de,$3A00
 call DMX
 pop af
 ld l,a
 ld h,e ; h = 0
 ld b,2 
 call DM_HL_DECI3
 pop af

 ld b,a
 ld (updateflag),a
AddCoinLoop:
 push bc
 call AddCoin
 call updatestats
 call FlashDelay
 pop bc
 djnz AddCoinLoop
 call OTH_PAUSE
 set 7,(iy+$14)

 pop hl
 pop bc
 inc sp
 inc sp  ;restore stack
 ld hl,level
 inc (hl)
 jp NewLevel

AddCoin:
 ld hl,(coins)
 inc hl
 ld (coins),hl
 ld a,100
 bcall(_divhlbya)
 or a
 ret nz
 ld hl,lives
 inc (hl)
 ret

killE:
 ld a,(hl)
 dec a
 jr z,ShowRemains
 dec a
 jr z,MakeShell
 dec a
 jr z,AlreadyShell
 sub 4
 jr z,MakeTurtle
 dec a
 jp z,BulletTest3
 dec a
 jp z,BulletTest3
 dec a
 jr z,BulletTest3 
 sub 3
 jp z,endlevel
 dec a
 jr z,PaddleV_Hit
 dec a
 jr z,PaddleH_Hit
 dec a
 jp z,MakeBig
 dec a
 jp z,GainLife
 dec a
 jp z,Invincible
 dec a
 jp z,MakeFlower
 jp NoKillE

AlreadyShell:
 inc hl
 ld (hl),a   ;not moving
 jr AlreadyShell2

MakeTurtle:
 ld (hl),2 ;2 = turtle
 inc hl
 ld (hl),a ;not moving
 jr AlreadyShell2

MakeShell:
 ld (hl),3   ;3 = shell
 inc hl
 ld (hl),a   ;not moving
 inc hl
 inc hl ;Y coord
 inc (hl)
 inc (hl)
 inc (hl)
AlreadyShell2:
 call BounceM2
 jp SkipC

ShowRemains:
 call BounceM

 ld b,0
xadj = $-1

 ld c,0
yadj = $-1

 ld hl,DeadG
 ld a,2
 call PutSpriteMask
 jp SkipC

BulletTest3:
 inc hl
 ld a,(hl)
 or a
 jp z,SkipC
 xor a
 call BounceM
 jp SkipC

PaddleV_Hit:
 call CheckBelow
 inc hl ;go to direction
 ld a,(hl)
 cp 3
 jp nz,SkipC
 ld b,2
 call MoveUp
 jp SkipC

PaddleH_Hit:
 call CheckBelow
 ld a,(direction)
 push af
 inc hl ;go to direction
 ld a,(hl)
 dec a
 push af
 call z,MoveLeft
 pop af
 dec a
 call z,MoveRight
 pop af
 ld (direction),a
 jp SkipC

BounceM:
 ld (hl),a
BounceM2:
 call OTH_ARROW
 bit 5,a
 call nz,notp
 ld (eflag),a
 ret

CheckBelow:
 push hl
 inc hl
 inc hl
 inc hl ;points to Y coord
 ld a,(ypos)
 add a,11
 ld b,(hl)
 pop hl
 cp b
 jr nz,SkipC2  ;needs to be directly below
 inc a
 ld (eflag),a  ;set eflag so you don't fall
 ld a,(animcounter)
 and %11
 ret z
SkipC2:
 inc sp \ inc sp
 jp SkipC

notp:
 ld a,8
 ld (jumpcounter),a
 ld (jumpflag),a
 ret

LoadMarioStats:
 ld hl,ypos
 ld (y1),hl
 ld hl,xpos
 ld (x1),hl
 ld a,8
 ld (w1),a
 ld a,12
 ld (h1),a
 xor a
 ld (mover),a
 ret

LoadCollision:
 ex af,af'
 inc a
 ex af,af'
 ret

InitCollision:
 di
 xor a      ;collision counter
 ex af,af' ;load it to af'
 ld hl,map
 call LoadMapLenghDE
 sbc hl,de
AddMapOffsetHL:
 ld a,(map_offset)
 ld e,a
 add hl,de
 ret

FindRightLevel2:
 srl b
 srl b
 srl b
FindRightLevel:
 inc b
 call LoadMapLenghDE
findrightlevel3:
 add hl,de
 djnz findrightlevel3

 ld a,(scroll_x)    ;all this is added-on to save space
 push hl
 ld hl,(x1)
 ld b,(hl)
 pop hl
 add a,b  ;this is an add-on
 ld e,a
 srl e
 srl e
 srl e
 ret

LoadMapLenghDE:
 ld a,(maplength)
 ld e,a
 xor a
 ld d,a
 ret 

;
;Up Collision
;

testmario:
 ld a,(mover)
 or a
 ret nz
 ld a,(bigflag)
 or a
 ret z
 ld b,8
 ret

CheckUpE:
 call LoadEnemyStats2
 jr CheckUpCollision

CheckUpM:
 call LoadMarioStats
CheckUpCollision:
 call InitCollision
 ld a,(h1)
 sub 8
 ld b,a
 call nz,testmario

 push hl
 ld hl,(y1)
 ld a,(hl)
 pop hl
 sub b
 jp m,CheckCollisionDone

 ld b,a
 and %111
 jr nz,CheckCollisionDone ;unaligned, so we don't care

 call FindRightLevel2
 ld (ux),a
 add hl,de

 call CheckUpC

 ld a,(w1)
 cp 9
 jr nc,checkrightU

 ld a,0
ux = $-1
 and %111
 jr z,CheckCollisionDone
checkrightU:
 inc hl
 call CheckUpC
CheckCollisionDone:
 ex af,af'
 or a         ;set Z or NZ
 ret

FlashDelay:
 ld b,8
FlashDelay2:
 ei \ halt
 djnz FlashDelay2
 ret

start1up:
 ld d,2
 ld e,17
 jr StartItem

startstar:
 ld d,0
 ld e,18
 jr StartItem

startflower:
 ld d,0
 ld e,19
 jr StartItem

startmushroom:
 ld a,(bigflag)
 or a
 jr nz,startflower
 ld d,2
 ld e,16
StartItem:
 push hl
 push de
 call FindESlot
 pop de
 jr z,FoundM
 pop hl
 jr foundcoin

FoundM:
 ld (hl),e  ;E = item
 inc hl
 ld (hl),d  ;D = starting direction
 push hl
 pop ix
 pop hl
 push hl
 call LoadBlockCoords
 pop hl
 ld (ix+1),b
 ld a,c
 sub 8
 ld (ix+2),a
 ld (hl),1
 jr UpdateBlock2

CheckUpC:
 ld a,(hl)
 dec a
 cp 37
 call c,LoadCollision
 inc a
 cp 69
 call nc,LoadCollision
 cp 21
 jr z,bustblock
 cp 22
 jr z,bustblock
 cp 17
 jr z,foundcoin
 cp 18
 jr z,startmushroom
 cp 19
 jr z,start1up
 cp 20
 jr z,startstar
CheckUpC2:
 cp 46
 jp z,YouDie2
 cp 47
 jp z,foundcoin2
 ret

foundcoin: 
 ld b,1
 ld (hl),b
 jr foundcoin3

foundcoin2:
 ld a,(scroll_x)
 cp 8
 ret z
 ld a,(mover)
 or a
 ret nz
 ld b,a
 ld (hl),a ;1 byte saved
foundcoin3:
 push hl
 call AddCoin
 pop hl
 jr UpdateBlock

bustblock3:
 ld a,(mover)
 or a
 ret z

 push hl
 ld hl,(cur_enemy)
 ld a,(hl)
 pop hl
 sub 12
 ret nz
 ld (hl),a
 ld b,a
 jr UpdateBlock

bustblock:
 ld a,(bigflag)
 or a
 ret z
bustblock2:
 ld (hl),0
UpdateBlock2:
 ld b,1
UpdateBlock:
 push bc

 push hl
 call LoadBlockCoords
 push bc
 ld hl,BlankBlock
 call MASK8x8
 pop bc
 pop hl ;restore HL value

 push hl
 call PutSpriteHL
 pop hl

 pop bc
 ld a,b
 or a
 ret z
 inc sp
 inc sp
 jp CheckCollisionDone

LoadBlockCoords:
 ld de,map
 or a
 sbc hl,de
 ld a,(maplength)
 ld (updateflag),a
 bcall(_divhlbya)

 push af
 ld a,l
 add a,a
 add a,a
 add a,a
 ld c,a
 ld a,(map_offset)
 ld b,a
 pop af
 sub b
 add a,a
 add a,a
 add a,a  ;multiply by 8
 push af
 ld a,(scroll_x)
 ld b,a
 pop af
 sub b
 ld b,a
 ret

;
;Down Collision
;

CheckDownE:
 call LoadEnemyStats
 jr CheckDownCollision

CheckDownM:
 call LoadMarioStats
CheckDownCollision:
 call InitCollision

 push hl
 ld hl,(y1)
 ld b,(hl)
 pop hl
 ld a,(h1)
 add a,b
 jp m,CheckCollisionDone
 ld b,a
 srl b
 srl b
 srl b
 and %111
 jp nz,CheckBelowUnaligned

 ld a,b
 cp 8
 jr z,FallBottom

 call FindRightLevel
 ld (ud),a
 ld d,0
 ld a,e
 cp 21
 call nc,makeneg
 add hl,de

 call CheckDownC

 push hl
 call CheckDSp2
 pop hl

 ld a,(w1)
 cp 9
 jr nc,checkrightD

 ld a,0
ud = $-1
 and %111
 jp z,CheckCollisionDone
checkrightD:
 inc hl
 call CheckDownC
 call CheckDSp
 jp CheckCollisionDone

CheckDownC:
 ld a,(hl)
 dec a
 cp 37
 call z,LoadCollision
 cp 38
 call z,LoadCollision
 cp 39
 call z,LoadCollision
 cp 22
 jp z,bounceblock
 cp 31
 call c,LoadCollision
 inc a
 cp 69
 call nc,LoadCollision
 cp 21
 jp z,bustblock3
 cp 22
 jp z,bustblock3
 jp CheckUpC2

FallBottom:
 ld a,(mover)
 or a
 jr z,YouDie3
 ld hl,(cur_enemy)
 ld a,(hl)
 cp 12
 jp z,endgame
 ld (hl),0
 scf
 ret

YouDie2:
 ld a,(mover)
 or a
 ret nz
 inc sp
 inc sp
 inc sp
 inc sp
YouDie3:
 inc sp
 inc sp
YouDie:
 ld hl,lives
 ld a,(hl)
 dec a
 ret z
 dec (hl)
 xor a
 ld (bigflag),a
 jp NewLevel 

CheckBelowUnaligned:
 call FindRightLevel
 ld (ub),a
 add hl,de

 ld a,(hl)
 push hl
 call CheckDSp2
 pop hl

 ld a,0
ub = $-1
 and %111
 jp z,CheckCollisionDone
 inc hl
 ld a,(hl)
 call CheckDSp
 jp CheckCollisionDone

nochangeY1:
 pop af
 jp LoadCollision

CheckDSp2:
 push af
 ld a,(scroll_x)

 ld b,a
 push hl
 ld hl,(x1)
 ld a,(hl)
 pop hl
 add a,b
 and %111
 add a,8
 ld b,a
 and %111
 jr z,cont
 inc hl
 ld a,(hl)
 dec hl
 dec a
 cp 22
 jr z,cont
 cp 31
 jr c,nochangeY1
 inc a
 cp 69
 jr nc,nochangeY1
cont:
 jr CheckDownSpecials

CheckDSp:
 push af
 ld a,(scroll_x)
 ld b,a
 push hl
 ld hl,(x1)
 ld a,(hl)
 pop hl
 add a,b
 and %111
 ld b,a
CheckDownSpecials:
 pop af
 cp 32
 jr z,bgrad2
 cp 33
 jr z,bgrad4
 cp 34
 jr z,steep2
 cp 35
 jr z,bgrad3
 cp 36
 jr z,bgrad1
 cp 37
 jr z,steep1
 cp 23
 jp z,bounceblock
 ret

setb2:
 xor a
 ret

steep1:
 ld a,-8
 add a,b
 cp 9
 call nc,setb2
 ld b,a
 jr moveM

bgrad1:
 ld a,-8
 add a,b
 cp 9
 call nc,setb2
 ld b,a
 srl b
 jr moveM

bgrad3:
 srl b
moveM:
 ld a,(h1)
 sub 8
 ld c,a

 ld hl,(y1)
 ld a,(hl)
 push af
 add a,c
 and %11111000
 add a,b
 sub c
 ld b,a
 pop af
 cp b
 ret nz
 jp LoadCollision


setb1:
 ld a,8
 ret

bgrad4:
 ld a,b
 cp 9
 call nc,setb1
 sub 8 
 neg
 ld b,a
 srl b
 jr moveM

bgrad2:
 ld a,b
 cp 9
 call nc,setb1
 sub 8 
 neg
 ld b,a
 srl b
 ld a,4
 add a,b
 ld b,a
 jr moveM

steep2:
 ld a,b
 sub 8 
 neg
 cp 9
 call nc,setb2
 ld b,a
 jr moveM

bounceblock:
 ld a,(mover)
 or a
 jp nz,LoadCollision

 ld a,(ypos)
 and %111
 cp 4
 ret nc

 ld a,18
 ld (jumpcounter),a
 ld (jumpflag),a
 ld (superjumpflag),a
 jp LoadCollision

testend:
 call CheckEndLevel
 ret nz
 jp LoadCollision

CheckEndLevel:
 ld a,(maplength)
 sub 10
 ld b,a
 ld a,(map_offset)
 sub b
 ld (endlevelflag),a
 ret

CheckRightE:
 call LoadEnemyStats
CheckRightCollision:
 di
 xor a
 ex af,af'
 ld hl,(x1)
 ld a,(hl)

 add a,8
 call XCollisionPart1
 cp 10
 call nc,testend
 call XCollisionPart2
 jr c,skiptop2
 ld a,(hl)
 call TestSBlock
 cp 35
 call z,LoadCollision
 cp 36
 call z,LoadCollision
 cp 37
 call z,LoadCollision
 dec a
 cp 31
 call c,LoadCollision
 inc a
 cp 23
 call z,bounceblockH
 cp 69
 call nc,LoadCollision
 cp 46
 call z,YouDie2
 cp 47
 call z,foundcoin2

 call testshell
 ld a,(y)
 and %111
 jr z,testifbig
skiptop2:
 call LoadMapLenghDE
 add hl,de
 ld a,(hl)
 call TestSBlock
 cp 37
 call z,LoadCollision
 dec a
 cp 31
 call c,LoadCollision
 inc a
 cp 23
 call z,bounceblockH
 cp 69
 call nc,LoadCollision
 cp 46
 call z,YouDie2
 cp 47
 call z,foundcoin2
 call testshell
 jp CheckCollisionDone


testifbig:
 ld a,(y)
 cp 12
 jp c,CheckCollisionDone
 ld a,(h1)
 cp 9
 jp c,CheckCollisionDone
 ld a,(mover)
 or a
 jr nz,sizebig
 ld a,(bigflag)
 or a
 jp z,CheckCollisionDone
sizebig:
 call LoadMapLenghDE
 sbc hl,de
 ld a,(hl)
 dec a
 cp 31
 call c,LoadCollision
 inc a
 cp 69
 call nc,LoadCollision
 cp 46
 call z,YouDie2
 cp 47
 call z,foundcoin2
 jp CheckCollisionDone

testifbeg:
 ld a,(map_offset)
 or a
 ret nz
 jp LoadCollision

deca:
 ld hl,(x1)
 ld a,(hl)
 dec a
 jr nodeca

CheckLeftE:
 call LoadEnemyStats
CheckLeftCollision:
 di
 xor a
 ex af,af'

 ld a,(scroll_x)
 ld b,a
 ld hl,(x1)
 ld a,(hl)
 add a,b
 and %111
 jr nz,deca

 ld hl,(x1)
 ld a,(hl)
 sub 8
nodeca:
 call XCollisionPart1
 cp 21
 call nc,testifbeg
 ld a,e
 cp 21
 call nc,makeneg
 call XCollisionPart2
 jr c,skiptop

 ld a,(hl)
 dec a
 cp 34
 call c,LoadCollision
 sub 2
 call TestSBlock
 add a,3
 cp 23
 call z,bounceblockH
 cp 69
 call nc,LoadCollision
 cp 46
 call z,YouDie2
 cp 47
 call z,foundcoin2
 call testshell
 
 ld a,(y)
 and %111
 jp z,testifbig
skiptop:
 call LoadMapLenghDE
 add hl,de
 ld a,(hl)
 dec a
 cp 31
 call c,LoadCollision
 cp 33
 call z,LoadCollision
 sub 2
 call TestSBlock
 add a,3
 cp 23
 call z,bounceblockH
 cp 69
 call nc,LoadCollision
 cp 46
 call z,YouDie2
 cp 47
 call z,foundcoin2
 call testshell
 jp CheckCollisionDone

bounceblockH:
 push af
 ld a,(mover)
 or a
 jr z,doneblockH
 ld a,(ypos)
 and %111
 cp 4
 jr c,doneblockH
 ex af,af'
 dec a
 ex af,af'
doneblockH:
 pop af
 ret

XCollisionPart1:
 ld (x),a
 ld hl,(y1)
 ld b,(hl)
 ld a,(h1)
 add a,b
 dec a
 jp m,CheckCollisionDone2
 inc a
 ld (y),a
 ld a,(scroll_x)
 ld b,a
 ld a,(x)
 add a,b
 ld e,a
 srl e
 srl e
 srl e
 ld a,e
 ld d,0
 ret

CheckCollisionDone2:
 inc sp
 inc sp
 jp CheckCollisionDone

XCollisionPart2:
 ld a,e
 cp 21
 call nc,makeneg
 ld hl,map
 add hl,de
 call LoadMapLenghDE
 sbc hl,de
 sbc hl,de
 call AddMapOffsetHL
 ld a,(y)
 push af
 ld b,a
 call FindRightLevel2 
 pop af
 cp 8
 ret


testshell:
 ld a,(mover)
 or a
 ret z
 push hl
 ld hl,(cur_enemy)
 ld a,(hl)
 pop hl
 cp 3
 ret nz

 ld a,(hl)
 sub 17
 jp z,foundcoin
 dec a
 jp z,startmushroom
 dec a
 jp z,start1up
 dec a
 jp z,startstar
 dec a
 jp z,bustblock2
 dec a
 jp z,bustblock2
 ret

TestSBlock:
 push af
 push hl
 sub 32
 jr z,SBlock
 dec a
 jr z,SBlock
 dec a
 jr z,SBlock
DoneSBlock:
 pop hl
 pop af
 ret

SBlock:
 ex af,af'
 ld l,a

 push hl
 ld a,(mover)
 or a
 jr nz,itse
 call CheckUpM
 jr testU
itse:
 call CheckUpE
testU
 pop hl
 ex af,af'
 ld a,l
 ex af,af'  ;restored AF'
 ld hl,(y1)
 or a
 jr z,NoLoadCollisionn
 call LoadCollision
 inc (hl)
NoLoadCollisionn:
 dec (hl)
 jr DoneSBlock

LoadEnemyStats:
 ld (cur_enemy),hl
LoadEnemyStats2:
 ld hl,0
cur_enemy = $-2

 push hl
 call LoadEHeight
 ld a,(hl)      ;put it in A
 ld (h1),a   ;height stored
 ld (mover),a
 inc hl
 ld a,(hl)
 ld (w1),a 
 pop hl
 inc hl ;skip type
 inc hl ;skip direction
 ld (x1),hl
 inc hl
 ld (y1),hl
 ret

makeneg:
 push hl
 ld a,32
 sub e
 ld e,a
 xor a
 ld d,a
 ld h,a
 ld l,a
 sbc hl,de
 ex de,hl
 pop hl
 ret

endgame:
 ld hl,enemy_table 
 ld bc,299
 call OTH_CLEAR

 ld a,10
 ld (maplength),a

 ld hl,FinalScreen
 ld de,map
 ld c,80
 call Decompress
 call DrawingInitMap

 ld hl,Enemies_On_Screen
 ld (hl),4
 inc hl
 ld (hl),4
 inc hl
 ld (hl),20
 inc hl
 ld (hl),40
 inc hl
 ld (hl),20
 inc hl
 ld (hl),28
 
 ld a,44
 ld (ypos),a
 ld a,40
 ld (xpos),a

 xor a
 ld (direction),a
 ld e,a
 inc a
 ld (bigflag),a

 ld hl,EndingText
 ld d,$09
 call DMX
 ld d,$10
 call DMX
FinalLoop:
 call MoveEnemies
 call DnEMario
 call OTH_ARROW
 bit 5,a
 jr z,DoneFinal
 ld a,%01111101
 out (1),a
 nop \ nop         
 in a,(1)
 bit 0,a
 jr nz,FinalLoop
DoneFinal:
 ld b,160
FinalLoop2:
 push bc
 ld a,b
 and %11
 dec a
 jr nz,NoMarioMove
 ld hl,xpos
 inc (hl)
NoMarioMove:
 call MoveEnemies
 call DnEMario
 pop bc
 djnz FinalLoop2
 ld b,50
 call FlashDelay2
QuickExit:
 ld sp,0
INITSP = $-2
 ret

OTH_PAUSE:
 call ClearKeys
OTH_PAUSE2:
 bcall(_getcsc)
 cp G_CLEAR
 jr z,QuickExit
 cp G_2ND
 jr z,ClearKeys
 cp 9
 jr nz,OTH_PAUSE2
ClearKeys:
 bcall(_getcsc)
 or a
 jr nz,ClearKeys 
 ret

DecodeZCP: 
		        LD   C, %10000000                 ; Graphics mask
                LD   A, (DE)                      ; A = byte of screen
_ZCP_MainLoop:  LD   B, (HL)                      ; B = byte of stream
                RLC  (HL)
                JR   C, _ZCP_Copy                 ; If MSB set, copy
                JR   Z, _ZCP_EndDecode            ; If zero, end
                LD   (HL), B
                INC  HL
                BIT  6, B                         ; Is Clear Bit set?
                JR   Z, _ZCP_WhiteBar             ; No, stream clear pixels
_ZCP_BlackBar:  RES  6, B                         ; AND B, 63
_ZCP_BBLoop:    XOR  C                            ; Set Bit
                RRC  C                            ; Shift the Mask
                JR   NC, _ZCP_BBSameByte
_ZCP_BBNextByte:LD   (DE), A                      ; Save Screen data
                INC  DE
                LD   A, (DE)                      ; Find next one
_ZCP_BBSameByte:DJNZ _ZCP_BBLoop
                JR   _ZCP_MainLoop
_ZCP_WhiteBar:  LD   (DE), A                      ; write data
_ZCP_WBLoop:    RRC  C
                JR   NC, _ZCP_WBSameByte
_ZCP_WBNextByte:INC  DE
_ZCP_WBSameByte:DJNZ _ZCP_WBLoop
                LD   A, (DE)                      ; read next - useful when stream <= 7
                JR   _ZCP_MainLoop
_ZCP_Copy:      LD   B, 7
_ZCP_CLoop:     RLC  (HL)
                JR   NC, _ZCP_CNoCopy
_ZCP_CCopy:     XOR  C
_ZCP_CNoCopy:   RRC  C
                JR   NC, _ZCP_CSameByte
_ZCP_CNextByte: LD   (DE), A
                INC  DE
                LD   A, (DE)
_ZCP_CSameByte: DJNZ _ZCP_CLoop
                INC  HL
                JR   _ZCP_MainLoop
_ZCP_EndDecode: LD   (DE), A                      ; Save last screen byte
                RET

fastCopy:
	di
	ld	a,$80				; 7
	out	($10),a				; 11
	ld	hl,GRAPH_MEM-12-(-(12*64)+1)		; 10
	ld	a,$20				; 7
	ld	c,a				; 4
	inc	hl				; 6 waste
	dec	hl				; 6 waste
fastCopyAgainI:
	ld	b,64				; 7
	inc	c				; 4
	ld	de,-(12*64)+1			; 10
	out	($10),a				; 11
	add	hl,de				; 11
	ld	de,10				; 10
fastCopyLoopI:
	add	hl,de				; 11
	inc	hl				; 6 waste
	inc	hl				; 6 waste
	inc	de				; 6
	ld	a,(hl)				; 7
	out	($11),a				; 11
	dec	de				; 6
	djnz	fastCopyLoopI			; 13/8
	ld	a,c				; 4
	cp	$2B+1				; 7
	jr	nz,fastCopyAgainI		; 10/1
	ret					; 10

Detect:
    ld	de,(ptemp)
	bcall(_cphlde)
	ld	a,(hl)
	jr	nz,detectContinue
	inc	a
	ret
detectContinue:
	push	hl
	and	$01
	jr	nz,detectSkip

#ifdef TI83P
	dec	hl
	dec	hl
	dec	hl	; hl->lsb ptr
	ld	e,(hl)
	dec	hl
	ld	d,(hl)
	dec	hl	; hl->page
	ld	a,(hl)
	or	a
	push	af
	ld	h,d
	ld	l,e	; hl & de->program
	jr	z,detectNoMove
	push	hl
	bcall(_memfree)
	ld	bc,64
	sbc	hl,bc
	pop	hl
	jr	c,detectNotEnough
	ld	de,($9820)
	push	ix
	push	hl
	push	de
	bcall(5017h)
	pop	hl
	push	hl
	pop	ix
	ld	a,10
	add	a,(ix+9)
	ld	e,a
	ld	d,0	; de=flash offset
	add	hl,de
	ex	(sp),hl
	add	hl,de
	pop	de
	ex	de,hl	; hl-temp, de-perm
	pop	ix
detectNoMove:
	inc	de
	inc	de
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	inc	hl	; hl->data in ram
	push	bc
	push	ix
	pop	bc
detectCheck:
	ld	a,(bc)
	or	a
	jr	z,detectFound
	cp	(hl)
	inc	bc
	inc	de
	inc	hl
	jr	z,detectCheck
detectBad:
	pop	bc
detectNotEnough:
	pop	af
detectSkip:
	pop	hl
	ld	bc,-6
	add	hl,bc
	ld	b,(hl)
	dec	hl
detectNameLoop2:
	dec	hl
	djnz	detectNameLoop2
	jr	detect
detectFound:
	pop	hl
	; hl=size, de->data
	pop	af	; a=page, f=(or a)
	jr	z,detectInRam
	push	de	; data
	push	af
	push	hl
	bcall(_enoughRam)
	pop	bc
	jr	c,detectBad
	pop	af
	pop	hl
	ld	de,($9820)	; tempMem
	push	de
	bcall(5017h)
	pop	de
detectInRam:	; de->data in RAM
	pop	hl	; hl->vat location
	ld	bc,-6
	add	hl,bc
	ld	b,(hl)
	inc	b
detectNameLoop1:
	dec	hl
	djnz	detectNameLoop1
	ex	de,hl
	xor	a
	ret
#else
	dec	hl		; move to ptr
	ld	b,(hl)
	dec	hl
	push	hl
	ld	h,(hl)
	ld	l,b	; now we are at the program
	inc	hl	; skip file length
	inc	hl
	push	ix
	pop	de
detectCheck:
	ld	a,(de)
	or	a
	jr	z,detectFound
	cp	(hl)
	inc	de
	inc	hl
	jr	z,detectCheck
detectNoGood:
	pop	hl
detectSkip:
	pop	hl
	dec	hl
	dec	hl
	dec	hl
	ld	b,(hl)
	dec	hl
detectNameLoop2:
	dec	hl
	djnz	detectNameLoop2
	jr	detect
detectFound:
	ex	de,hl
	pop	hl
	pop	af
	dec	hl
	ld	b,(hl)
	inc	b
detectNameLoop1:
	dec	hl
	djnz	detectNameLoop1
	ex	de,hl
	xor	a
	ret
#endif

MarioSprites:
 .db $FF,$FF,$E7,$C1,$83,$01,$01,$C3,$81,$00,$81,$18
 .db $00,$00,$18,$3E,$64,$D0,$C6,$3C,$7E,$95,$7E,$E7
 .db $FF,$FF,$E7,$C1,$83,$01,$01,$C3,$81,$81,$03,$F1
 .db $00,$00,$18,$3E,$64,$D0,$C6,$3C,$72,$7E,$FC,$0E
 .db $FF,$FF,$E7,$C1,$83,$01,$01,$C3,$81,$01,$81,$1F
 .db $00,$00,$18,$3E,$64,$D0,$C6,$3C,$7E,$9A,$7E,$E0

MarioBigSprites:
 .db $E7,$C1,$83,$01,$01,$E3,$C3,$81,$00,$81,$81,$18
 .db $18,$3E,$64,$D0,$C6,$00,$3C,$7E,$95,$7E,$7E,$E7
 .db $E7,$C1,$83,$01,$01,$E3,$C1,$C0,$81,$81,$01,$F8
 .db $18,$3E,$64,$D0,$C6,$00,$3E,$39,$7E,$7E,$FE,$07
 .db $E7,$C1,$83,$01,$01,$E3,$C3,$81,$80,$01,$80,$1F
 .db $18,$3E,$64,$D0,$C6,$00,$3C,$7E,$75,$9E,$7F,$E0

MarioFlowerSprites:
 .db $E7,$C1,$83,$01,$01,$E3,$C3,$81,$00,$81,$81,$18
 .db $18,$3E,$64,$D0,$C6,$00,$3C,$42,$EB,$42,$5A,$E7
 .db $E7,$C1,$83,$01,$01,$E3,$C1,$C0,$81,$81,$01,$F8
 .db $18,$3E,$64,$D0,$C6,$00,$3E,$27,$4A,$42,$FE,$07
 .db $E7,$C1,$83,$01,$01,$E3,$C3,$81,$80,$01,$80,$1F
 .db $18,$3E,$64,$D0,$C6,$00,$3C,$42,$57,$C2,$7F,$E0

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

questionblocks:
 .db $7E,$99,$A5,$8D,$99,$81,$99,$7E
 .db $7E,$8D,$D3,$C7,$8D,$81,$8D,$7E
 .db $7E,$C7,$A9,$E3,$C7,$81,$C7,$7E
 .db $7E,$E3,$95,$B1,$E3,$81,$E3,$7E
 .db $7E,$B1,$CB,$99,$B1,$81,$B1,$7E

enemy_sprites:
 .dw goomba   ;#1 = goomba
 .dw turtle   ;#2 = turtle
 .dw shell    ;#3 = shell
 .dw plant    ;#4 = plant
 .dw fireballs ;#5 = lava ball
 .dw pound    ;#6 = big pounder
 .dw flyingturtle  ;#7 = flying turtle
 .dw bullet        ;#8 = bullet
 .dw cannonball ;9  = ball diagonal up-left
 .dw cannonball ;10 = ball diagonal down-left
 .dw Fish     	;#11 = Fish
 .dw Bowser 	;#12 = Bowser Left
 .dw endbar    	;#13 = end level bar
 .dw Paddle    	;#14 = Paddle V
 .dw Paddle    	;#15 = Paddle H
 .dw mushroom  	;#16 = mushroom
 .dw mushroom2 	;#17 = 1 up
 .dw star      	;#18 = star
 .dw flowerpower ;19 = fireball flower
 .dw plantfire   ;20
 .dw mfire       ;21 ;mario fire left
 .dw mfire       ;22 ;mario fire right

mfire:
 .db 5,5
 .db %11011111
 .db %10001111
 .db %00000111
 .db %10001111
 .db %11011111

 .db 0
 .db %00100000
 .db %01110000
 .db %00100000
 .db 0

Bowser:
 .db 20,16
 .db $EF,$F7,$CF,$F3,$88,$11,$80,$01,$80,$01,$C0,$03,$C0,$03,$80,$01
 .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$01,$80,$01,$80,$01
 .db $C0,$03,$80,$01,$81,$81,$C3,$C3

 .db $10,$08,$30,$0C,$57,$EA,$5B,$DA,$5D,$BA,$39,$9C,$3B,$DC,$5E,$7A
 .db $F8,$1F,$B7,$ED,$CA,$53,$D7,$EB,$B8,$1D,$67,$E6,$70,$0E,$6F,$F6
 .db $30,$0C,$79,$9E,$7E,$7E,$3C,$3C


plantfire:
 .db 3,3
 .db %10111111,%00011111,%10111111
 .db $A0,$40,$A0

flowerpower:
 .db 8,8
 .db $81,$00,$00,$00,$81,$66,$24,$81
 .db $7E,$81,$99,$81,$7E,$99,$DB,$7E
 .db $81,$00,$00,$00,$81,$66,$24,$81
 .db $7E,$FF,$E7,$FF,$7E,$99,$DB,$7E

cannonball:
 .db 7,7
 .db %11000111
 .db %10000011
 .db %00000001
 .db %00000001
 .db %00000001
 .db %10000011
 .db %11000111

 .db $38,$64,$FA,$FE,$FE,$7C,$38

Paddle:
 .db 3,8
 .db $00,$00,$81
 .db $FF,$BF,$7E

 ;height,width
 ;mask
 ;sprite
goomba:
 .db 8,8   ;goomba 1
 .db $C3,$81,$00,$00,$00,$81,$C1,$8F
 .db $3C,$7E,$DB,$FF,$E7,$7E,$3E,$70

 .db $C3,$81,$00,$00,$00,$81,$83,$F1
 .db $3C,$7E,$DB,$FF,$E7,$7E,$7C,$0E

bullet:
 .db 8,8
 .db $F0,$C1,$80,$01,$01,$80,$C1,$F0
 .db $0F,$3E,$5F,$FE,$F6,$4F,$3E,$0F

Fish:
 .db 8,8
 .db $E3,$C6,$84,$01,$81,$80,$0C,$C7
 .db $1C,$39,$7B,$BE,$7E,$7F,$F3,$38
 .db $FF,$C7,$8C,$00,$03,$00,$9C,$CF
 .db $00,$38,$73,$BF,$FC,$FF,$63,$30

turtle:
 .db 14,8
 .db $9F,$8F,$CF,$4F,$0F,$0F,$4F,$C3,$C3,$80,$80,$C0,$C1,$8F
 .db $60,$70,$30,$B0,$F0,$F0,$B8,$3E,$3A,$6F,$7D,$3F,$3E,$70

 .db $9F,$8F,$CF,$4F,$0F,$0F,$47,$C1,$C1,$80,$80,$C0,$89,$F1
 .db $60,$70,$30,$B0,$F0,$F0,$B8,$3E,$3A,$6F,$7D,$3F,$76,$0E

flyingturtle:
 .db 14,8
 .db $9F,$8F,$CF,$4F,$0F,$0F,$47,$C1,$C1,$80,$80,$C0,$89,$F1
 .db $60,$70,$30,$B0,$F0,$F0,$B8,$3E,$36,$67,$63,$3F,$76,$0E
 .db $9F,$8F,$CF,$4F,$0F,$0F,$47,$C1,$C1,$80,$80,$C0,$89,$F1
 .db $60,$70,$30,$B0,$F0,$F0,$B8,$3E,$36,$71,$7F,$3F,$76,$0E

shell:
 .db 5,8
 .db $C3,$81,$00,$00,$81
 .db $3C,$7A,$DF,$F7,$7E

endbar:
 .db 4,12
 .db $80,$0F,$00,$0F,$00,$0F,$80,$0F
 .db $7F,$F0,$80,$00,$80,$00,$7F,$F0

mushroom:
 .db 8,8
 .db $C3,$81,$00,$00,$00,$81,$81,$C3
 .db $3C,$66,$CB,$9D,$FF,$56,$42,$3C

mushroom2:
 .db 8,8
 .db $C3,$81,$00,$00,$00,$81,$81,$C3
 .db $3C,$7E,$FF,$FF,$FF,$56,$42,$3C

star:
 .db 8,8
 .db $FF,$E7,$C3,$00,$00,$81,$00,$18
 .db $00,$18,$24,$C3,$95,$42,$99,$E7
 .db $FF,$E7,$C3,$00,$00,$81,$00,$18
 .db $00,$18,$3C,$FF,$FF,$7E,$FF,$E7
 
fireballs:
 .db 8,8
 .db $C3,$81,$00,$00,$00,$81,$81,$C3
 .db $00,$3C,$5A,$66,$3C,$18,$24,$00
 .db $C3,$81,$81,$00,$00,$00,$81,$C3
 .db $00,$24,$18,$3C,$66,$5A,$3C,$00

plant:
 .db 12,8
 .db $3C,$18,$81,$C3,$66,$81,$C3,$66,$81,$C3,$66,$81
 .db $C3,$E7,$7E,$3C,$99,$7E,$3C,$99,$7E,$3C,$99,$7E

pound:
 .db 16,16
 .db $84,$21,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
 .db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$84,$21
 .db $7B,$DE,$A6,$65,$94,$29,$8C,$31,$CF,$F3,$F9,$9F,$AE,$75,$A6,$65
 .db $E0,$07,$A3,$C5,$A4,$25,$F0,$0F,$BF,$FD,$94,$29,$A6,$65,$7B,$DE

DeadG:
 .db %10000001
 .db %00000000

 .db %01111110
 .db %11111111

JumpFallTable:
 .db 1,0,0,1,0,1,1,2,1,2,1,2,2,2,3,2,3,3,2,3,2,3,3,4

InitEStatusTable:
 .db 1,1,0,3,3,0,1,1,1,1,1,1,3,3,1,0,2,2  ;,0

MaskTable:      .DB   %00000000
                .DB   %10000000
                .DB   %11000000
                .DB   %11100000
                .DB   %11110000
                .DB   %11111000
                .DB   %11111100
                .DB   %11111110

TitlePic:
 .db $12,$F9,$E0,$FE,$08,$FF,$0E,$F7,$E0,$1E,$F3,$BF,$F8,$EF,$EC,$98,$09,$FE
 .db $AC,$1E,$E7,$DB,$B8,$9C,$B4,$F6,$0A,$FD,$92,$1E,$E5,$92,$99,$C8,$B4,$D2
 .db $0A,$FF,$BA,$1F,$EC,$E4,$B3,$93,$C9,$E6,$0A,$4B,$A0,$1B,$EF,$C8,$E6,$A1
 .db $F3,$DE,$0A,$4A,$A0,$1C,$CF,$91,$C9,$C3,$E0,$BE,$0A,$DF,$E8,$1E,$C7,$81
 .db $C3,$CF,$E0,$A7,$87,$F1,$FF,$1F,$F3,$E0,$F3,$F3,$F9,$E9,$C3,$82,$BF,$F0
 .db $19,$E3,$F0,$FD,$F8,$8C,$F3,$E1,$81,$C7,$FC,$1A,$E6,$53,$8F,$FC,$F8,$B0
 .db $80,$FF,$1D,$F8,$64,$83,$C0,$8F,$F0,$1A,$E3,$E7,$4B,$BD,$DE,$F0,$9F,$E3
 .db $FC,$21,$FC,$4D,$8E,$EF,$BB,$8F,$FB,$E6,$21,$FC,$BF,$9E,$8F,$FF,$94,$C7
 .db $F3,$C4,$22,$E0,$B0,$98,$8E,$8C,$82,$CA,$80,$C8,$3F,$81,$92,$AF,$08,$C0
 .db $18,$F9,$E0,$80,$FE,$0E,$F1,$8F,$C0,$A0,$1A,$ED,$F0,$FC,$E3,$CF,$C0,$C2
 .db $EF,$83,$C0,$1A,$C7,$99,$CC,$C0,$ED,$C0,$A5,$FC,$BE,$21,$C0,$89,$84,$C0
 .db $A8,$C0,$9B,$FE,$FF,$21,$C0,$8B,$86,$CF,$B8,$C7,$EF,$4D,$A0,$1B,$C0,$8A
 .db $82,$CB,$98,$DE,$B1,$4D,$90,$1A,$E0,$87,$81,$A5,$CC,$B8,$8C,$AF,$DF,$D0
 .db $1A,$C0,$F2,$99,$A7,$CC,$B0,$86,$AF,$DF,$F0,$1A,$C0,$F2,$99,$E7,$DC,$B1
 .db $E3,$BF,$4A,$1E,$C0,$FB,$B8,$E0,$94,$A1,$B1,$BF,$4A,$1E,$CF,$FB,$98,$E0
 .db $94,$B1,$91,$FE,$BD,$F0,$1A,$CF,$FD,$80,$E0,$9C,$B1,$B1,$FE,$9E,$88,$19
 .db $E7,$FA,$C0,$91,$E6,$9C,$F0,$48,$9C,$88,$19,$C7,$FB,$FE,$91,$E6,$9E,$81
 .db $4C,$81,$1C,$C7,$FD,$FE,$9B,$EF,$BF,$87,$FB,$FC,$84,$1A,$EF,$EF,$FF,$BF
 .db $49,$BE,$F1,$EF,$F8,$90,$18,$49,$AB,$50,$BD,$49,$BD,$48,$B8,$1E,$FC,$87
 .db $4E,$9D,$4E,$BF,$F8,$21,$FC,$87,$C1,$FF,$98,$FB,$FE,$BF,$C8,$21,$FC,$83
 .db $83,$FE,$0E,$FC,$BF,$C8,$22,$F0,$0C,$F8,$16,$48,$90,$3F,$11,$4B,$A0,$3F
 .db $0E,$4F,$3F,$12,$C3,$BF,$C0,$3F,$0D,$4E,$3F,$17,$C1,$B0,$3F,$14,$48,$80
; .db $00 ;zero below

FinalScreen:   ;rle compressed
	.db $00,$38,$39,$80,$0E,$38,$39,$80,$1B,$11
	.db $00,$11,$80,$03,$0D,$0F,$80,$08,$0E,$10
	.db $80,$03,$B2,$03,$06,$87,$08,$08

SideBar:
 .db $7F,$FE,$B1,$75,$EF,$27,$F3,$57,$FD,$77,$FD,$77,$E3,$77,$BF,$FD
 .db $80,$01,$D5,$55,$AA,$AB,$C4,$45,$80,$01,$A2,$81,$B6,$B1,$AA,$21
 .db $A2,$11,$A2,$31,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01
 .db $80,$01,$D5,$55,$AA,$AB,$C4,$45,$80,$01,$A0,$61,$A5,$21,$A5,$21
 .db $A5,$21,$B2,$21,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01
 .db $80,$01,$D5,$55,$AA,$AB,$C4,$45,$80,$01,$90,$41,$A2,$19,$A5,$55
 .db $A5,$55,$92,$55,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01,$80,$01
 .db $80,$01,$A2,$23,$D5,$55,$AA,$AB,$D5,$55,$AA,$AB,$D5,$55,$7F,$FE

map:
 .ds 1024

Enemies_On_Screen:
 .ds 225   ;32 enemies
           ;7 bytes each
       			;1 = type
				;2 = direction
				;3 = x
				;4 = y
				;5 = init x
				;6 = init y
				;7 = timer

enemy_table:  ;where enemies are initially ;100 max
 .ds 300


LevelTxt:
 .db "Level:",0

TitleScreenText:
 .db "Void Productions",0
 .db "(http://void.calc.org)",0
 .db "presents",0
AuthorTxt:
 .db "Written by Sam Heald",0
 .db "Graphics by Bill Nagel",0
 .db "Press 2nd Key",0

EndingText:
 .db "Thank you, Mario!",0
 .db "Your Quest Is Over...",0

Bonus:
 .db " Bonus!: ",0

TEXT_MEM3:
 .ds 129

NoLevel:
 .db "No Levels Found!",0

scroll_x = TEXT_MEM3
map_offset = TEXT_MEM3+1
ypos = TEXT_MEM3+2
xpos = TEXT_MEM3+3
direction = TEXT_MEM3+4
runflag = TEXT_MEM3+5
bigflag = TEXT_MEM3+6
x = TEXT_MEM3+7
y = TEXT_MEM3+8
blockcounter = TEXT_MEM3+9
updateflag = TEXT_MEM3+10
maplength = TEXT_MEM3+11
level = TEXT_MEM3+12
jumpflag = TEXT_MEM3+13
jumpcounter = TEXT_MEM3+14
superjumpflag = TEXT_MEM3+15
lives	= TEXT_MEM3+16
coins = TEXT_MEM3+17 ;2 bytes
mover = TEXT_MEM3+20  ;which is moving, 0 = Mario, 1 = Enemy
y1 = TEXT_MEM3+21 ;Y coord
x1 = TEXT_MEM3+23 ;X coord
h1 = TEXT_MEM3+25 ;height
h2 = TEXT_MEM3+26
w1 = TEXT_MEM3+27
w2 = TEXT_MEM3+28
fallflag = TEXT_MEM3+32
KEYPRESS = TEXT_MEM3+34
alphapressed = TEXT_MEM3+36
string = TEXT_MEM3+38
tempsprite = TEXT_MEM3+44 ;24 byte buffer for flipping left to right.

Top		= TEXT_MEM3+103
LvlNo	= TEXT_MEM3+104
BlankBlock = TEXT_MEM3+105 ;blank sprite
QuestionBlock = BlankBlock+8

.end
