	processor 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"

row_height_bits = 2
row_height_scanlines = 1 << row_height_bits

	seg.u variables
	org $80
temp0 ds 1

position_lo	ds 1
position_hi	ds 1
avatar_x ds 1

; Used in kernel
scanline_count ds 1
scanlines_left_in_row ds 1

	seg code
	org  $f000

; NTSC timing:
; 40 scanlines vblank (3 with vsync)
; 192 scanlines kernel
; 30 scanlines overscan
; Total: 262 scanlines

main_start:
	CLEAN_START

	.if 0
	lda #$02    ; SCORE = different colors for left and right
	sta CTRLPF
	lda #$18
	sta COLUP0
	lda #$88
	sta COLUP1
	.endif

	lda #40
	sta avatar_x

game_frame:
	lsr SWCHB ; test reset switch
	bcc main_start

	TIMER_SETUP 40   ;NTSC: 40 lines vblank

	lda #2
	sta VBLANK
	sta VSYNC
	sta WSYNC  ; Keep VSYNC on for 3 scanlines
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC

	lda #$2f
	sta COLUPF
	lda #$04
	sta COLUBK

	lda #$00
	sta PF0
	sta PF1
	sta PF2

	; Check input

	ldy #0    ; delta
	sty temp0  ; delta high byte

	lda #$20  ; Down
	bit SWCHA
	bne .not_down
	ldy #$ff
	sty temp0
.not_down:

	lda #$10    ; Up
	bit SWCHA
	bne .not_up
	iny
.not_up:
	tya
	clc
	adc position_lo
	sta position_lo
	lda position_hi
	adc temp0  ; high byte
	sta position_hi

	lda INPT4
	bmi .not_fire
	lda #0
	sta position_hi
	sta position_lo
.not_fire:

	; Set avatar position
	lda avatar_x
	ldx #0		; player 0
	jsr set_x_pos
	sta WSYNC
	sta HMOVE	; apply fine offsets

	; Get level row from high resolution position

	lda position_hi
	sta temp0
	lda position_lo

	REPEAT row_height_bits
	lsr temp0
	ror
	REPEND
	tax    ; x = level row index

	lda position_lo
	and #row_height_scanlines-1
	eor #row_height_scanlines-1
	sta scanlines_left_in_row

	; Wait for end of vblank
	TIMER_WAIT
	;lda #0   ; already 0 from TIMER_WAIT
	sta VBLANK

	; Start of visible graphics
	lda #192
	sta scanline_count

	ldy #0
.each_scanline:
	sta WSYNC
	stx COLUPF

	lda avatar_sprite,y
	sta GRP0

	;lda level_pf0l,x
	;sta PF0
	lda level_pf1l,x
	sta PF1
	lda level_pf2l,x
	sta PF2

	lda level_pf0r,x
	sta PF0
	lda level_pf1r,x
	sta PF1
	lda level_pf2r,x
	sta PF2

	iny  ; next sprite pointer

	dec scanlines_left_in_row
	bpl .not_next_row
	inx
	lda #row_height_scanlines
	sta scanlines_left_in_row
.not_next_row:

	dec scanline_count
	bne .each_scanline

	TIMER_SETUP 30  ; NTSC: 30 lines overscan

	lda #$00
	sta COLUPF

	TIMER_WAIT
	jmp game_frame

; SetHorizPos routine
; Based on 8bitworkshop's SetHorizPos
; A = X coordinate
; X = player number (0 or 1)
set_x_pos:
	sta WSYNC	; start a new line
	sec		; set carry flag
DivideLoop
	sbc #15		; subtract 15
	bcs DivideLoop	; branch until negative
	eor #7		; calculate fine offset
	asl
	asl
	asl
	asl
	sta RESP0,x	; fix coarse position
	sta HMP0,x	; set fine offset
	rts		; return to caller

level_data_start:
	ALIGN $100
	.include "level.asm"
	echo "Size of levels:", *-level_data_start

	ALIGN $100
avatar_sprite:
	;.byte #0        ; zero padding, also clears register
	.byte #%00111100
	.byte #%01000010
	.byte #%11100111
	.byte #%11111111
	.byte #%10011001
	.byte #%01111110
	.byte #%11000011
	.byte #%10000001

bytes_left = $fffc-*
	echo "Bytes left:", bytes_left

	org $fffc
	.word main_start
	.word main_start

; vim: set ts=16 sw=16 tw=0 noet :
