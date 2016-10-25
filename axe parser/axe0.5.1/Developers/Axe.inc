;########################
;#    Axiom Helpers     #
;########################

AXM_HEADER	=$C0DE
AXM_END         =$0000

AXM_NOSTUB	=%00000001
AXM_ION		=%00000010
AXM_MOS		=%00000100
AXM_DCS		=%00001000
AXM_APP		=%00010000
AXM_SHELLS      =%00001110
AXM_NONAPP	=%00001111
AXM_ALL		=%00011111

AXM_INLINE	=%00000000
AXM_SUB		=%00000001
AXM_RMOD	=%00000010
AXM_RRMOD	=%00000100
AXM_STORED	=%00001000
AXM_DATA	=%00010000

AXM_0ARGS	=0
AXM_1ARG	=1
AXM_2ARGS	=2
AXM_3ARGS	=3
AXM_4ARGS	=4
AXM_5ARGS	=5
AXM_6ARGS	=6

#define	REP_NEXT  ld a,a \ .org $-1

;#######################
;#    Axiom Tokens     #
;#######################

tok_None	=$0000
tok_Load	=$1263
tok_Next	=$1363
tok_Render	=$0063
tok_DrawL	=$1463
tok_DrawR	=$1563
tok_DrawS	=$0163
tok_Print	=$3763
tok_Up		=$1863
tok_Down	=$1963
tok_Left	=$2463
tok_Right	=$1663
tok_New		=$1763
tok_Delete	=$2563
tok_Data	=$FFFF

;############################
;#     Axe Subroutines      #
;############################

sub_Mul		=$3F00+1
sub_Div		=$3F00+2
sub_Mod		=$3F00+3
sub_Pix		=$3F00+4
sub_Rand	=$3F00+5
sub_FastCpy	=$3F00+6
sub_OrSprite	=$3F00+7
sub_XorSprite	=$3F00+8
sub_OffSprite	=$3F00+9
sub_DrawInv	=$3F00+10
sub_ShiftLeft	=$3F00+11
sub_ShiftRight	=$3F00+12
sub_ShiftUp	=$3F00+13
sub_ShiftDown	=$3F00+14
sub_DirectKey	=$3F00+15
sub_FreqOut	=$3F00+16
sub_Sqrt	=$3F00+17
sub_GetCalc	=$3F00+18
sub_NewVar	=$3F00+19
sub_Unarc	=$3F00+20
sub_Arc		=$3F00+21
sub_Sin		=$3F00+22
sub_DispGS	=$3F00+23
sub_Line	=$3F00+24
sub_SDiv	=$3F00+25
sub_TextInt	=$3F00+26
sub_4Lvl	=$3F00+27
sub_SortD	=$3F00+28
sub_EquStr	=$3F00+29
sub_88Mul	=$3F00+30
sub_Rect	=$3F00+31
sub_Circle	=$3F00+32
sub_RectInv	=$3F00+33
sub_GetArc	=$3F00+34
sub_ReadArc	=$3F00+35
sub_CopyArc	=$3F00+36
sub_MskSprite	=$3F00+37
sub_GetByte	=$3F00+38
sub_SendByte	=$3F00+39
sub_RotC	=$3F00+40
sub_RotCC	=$3F00+41
sub_FlipH	=$3F00+42
sub_FlipV	=$3F00+43
sub_InData	=$3F00+44
sub_Input	=$3F00+45
sub_Nib1	=$3F00+46
sub_Nib2	=$3F00+47
sub_NibSto	=$3F00+48
sub_Length	=$3F00+49
sub_GetKeyVar   =$3F00+50
sub_DrawClr	=$3F00+51
sub_ToHex	=$3F00+52

sub_Axiom	=$3F00+96

sub_Axiom1	=sub_Axiom+0
sub_Axiom2	=sub_Axiom+1
sub_Axiom3	=sub_Axiom+2
sub_Axiom4	=sub_Axiom+3
sub_Axiom5	=sub_Axiom+4
sub_Axiom6	=sub_Axiom+5
sub_Axiom7	=sub_Axiom+6
sub_Axiom8	=sub_Axiom+7
sub_Axiom9	=sub_Axiom+8
sub_Axiom10	=sub_Axiom+9
sub_Axiom11	=sub_Axiom+10
sub_Axiom12	=sub_Axiom+11
sub_Axiom13	=sub_Axiom+12
sub_Axiom14	=sub_Axiom+13
sub_Axiom15	=sub_Axiom+14
sub_Axiom16	=sub_Axiom+15
sub_Axiom17	=sub_Axiom+16
sub_Axiom18	=sub_Axiom+17
sub_Axiom19	=sub_Axiom+18
sub_Axiom20	=sub_Axiom+19
sub_Axiom21	=sub_Axiom+20
sub_Axiom22	=sub_Axiom+21
sub_Axiom23	=sub_Axiom+22
sub_Axiom24	=sub_Axiom+23
sub_Axiom25	=sub_Axiom+24
sub_Axiom26	=sub_Axiom+25
sub_Axiom27	=sub_Axiom+26
sub_Axiom28	=sub_Axiom+27
sub_Axiom29	=sub_Axiom+28
sub_Axiom30	=sub_Axiom+29
sub_Axiom31	=sub_Axiom+30
sub_Axiom32	=sub_Axiom+31

;############################
;#      Axe Variables       #
;############################

axv_A		=$89B6+0
axv_B		=$89B6+2
axv_C		=$89B6+4
axv_D		=$89B6+6
axv_E		=$89B6+8
axv_F		=$89B6+10
axv_G		=$89B6+12
axv_H		=$89B6+14
axv_I		=$89B6+16
axv_J		=$89B6+18
axv_K		=$89B6+20
axv_L		=$89B6+22
axv_M		=$89B6+24
axv_N		=$89B6+26
axv_O		=$89B6+28
axv_P		=$89B6+30
axv_Q		=$89B6+32
axv_R		=$89B6+34
axv_S		=$89B6+36
axv_T		=$89B6+38
axv_U		=$89B6+40
axv_V		=$89B6+42
axv_W		=$89B6+44
axv_X		=$89B6+46
axv_Y		=$89B6+48
axv_Z		=$89B6+50
axv_Theta	=$89B6+52

axv_R1		=$83A5+0
axv_R2		=$83A5+2
axv_R3		=$83A5+4
axv_R4		=$83A5+6
axv_R5		=$83A5+8
axv_R6		=$83A5+10

axv_Y1		=$83A5+12
axv_Y1Page	=$83A5+14
axv_Y2		=$83A5+15
axv_Y2Page	=$83A5+17
axv_Y3		=$83A5+18
axv_Y3Page	=$83A5+20
axv_Y4		=$83A5+21
axv_Y4Page	=$83A5+23
axv_Y5		=$83A5+24
axv_Y5Page	=$83A5+26
axv_Y6		=$83A5+27
axv_Y6Page	=$83A5+29
axv_Y7		=$83A5+30
axv_Y7Page	=$83A5+32
axv_Y8		=$83A5+33
axv_Y8Page	=$83A5+35
axv_Y9		=$83A5+36
axv_Y9Page	=$83A5+38
axv_Y0		=$83A5+39
axv_Y0Page	=$83A5+41

axv_SpriteBuff	=$83A5+42

axv_RandSeed	=$83A5+50

axv_X1t		=$83A5+52
axv_Y1t         =$83A5+54
axv_X2t		=$83A5+56
axv_Y2t         =$83A5+58
axv_X3t		=$83A5+60
axv_Y3t         =$83A5+62

;############################
;#    Axe RAM Locations     #
;############################

axr_L1		=$86EC		;714 bytes
axr_L2		=$8A3A		;531 bytes
axr_L3		=$9872		;768 bytes
axr_L4		=$82A5		;323 bytes
axr_L5		=$8508		;128 bytes
axr_L6		=$9340		;768 bytes