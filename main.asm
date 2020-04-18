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
	lda #$00
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
	clc
.each_scanline:
	stx COLUBK

	lda level_pf0,x
	sta PF0
	lda level_pf1,x
	sta PF1
	lda level_pf2,x
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
