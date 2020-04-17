	processor 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"

	seg.u variables
	org $80

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

	TIMER_WAIT

	lda #0
	sta VBLANK

	; Start of visible graphics
	ldx #192
.each_scanline:
	stx COLUBK
	sta WSYNC
	dex
	bne .each_scanline

	TIMER_SETUP 30  ; NTSC: 30 lines overscan

	lda #$00
	sta COLUPF

	TIMER_WAIT
	jmp game_frame

	org $fffc
	.word main_start
	.word main_start

; vim: set ts=16 sw=16 tw=0 noet :
