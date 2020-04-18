	processor 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"

	seg.u variables
	org $80
position	ds 1

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

	; Check input

	lda #$20  ; Down
	bit SWCHA
	bne .not_down
	dec position
.not_down:

	lda #$10    ; Up
	bit SWCHA
	bne .not_up
	inc position
.not_up:

	lda INPT4
	bmi .not_fire
	lda #0
	sta position
.not_fire:

	; Wait for end of vblank
	TIMER_WAIT
	lda #0
	sta VBLANK

	; Start of visible graphics
	ldx #0
.each_scanline:
	stx COLUPF

	lda level_pf0l,x
	sta PF0
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

	sta WSYNC
	inx
	cpx #192
	bne .each_scanline

	TIMER_SETUP 30  ; NTSC: 30 lines overscan

	lda #$00
	sta COLUPF

	TIMER_WAIT
	jmp game_frame

	.include "level.asm"

	org $fffc
	.word main_start
	.word main_start

; vim: set ts=16 sw=16 tw=0 noet :
