start_frame:
	; Call at start of frame. Sets vertical sync and a timer for vblank.
	; Follow up with:
	;   TIMER_WAIT
	;   sta VBLANK

	TIMER_SETUP 40   ;NTSC: 40 lines vblank

	lda #2
	sta VBLANK
	sta VSYNC
	sta WSYNC  ; Keep VSYNC on for 3 scanlines
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC
	rts

; From 8bitworkshop 48 pixel sprite example
	seg.u variables

Temp		byte
LoopCount	byte

	seg code

display_wide_sprite:
	; Expects a timer setup for when to start rendering

	lda #wide_sprite_height
	sta LoopCount	; scanline counter
	lda #%011
	sta NUSIZ0
	sta NUSIZ1	; both players have 3 copies
	sta WSYNC
	SLEEP 20
	sta RESP0	; position 1st player
	sta RESP1	; ...and 2nd player
	lda #$10
	sta HMP1	; 1 pixel to the left
	sta WSYNC
	sta HMOVE	; apply HMOVE
	sta HMCLR
	lda #1
	sta VDELP0	; we need the VDEL registers
	sta VDELP1	; so we can do our 4-store trick
	TIMER_WAIT
	sta VBLANK

	SLEEP 40	; start near end of scanline
BigLoop
	ldy LoopCount	; counts backwards
	lda wide_sprite0,y	; load B0 (1st sprite byte)
	sta GRP0	; B0 -> [GRP0]
	lda wide_sprite1,y	; load B1 -> A
	sta GRP1	; B1 -> [GRP1], B0 -> GRP0
	sta WSYNC	; sync to next scanline
	lda wide_sprite2,y	; load B2 -> A
	sta GRP0	; B2 -> [GRP0], B1 -> GRP1
	lda wide_sprite5,y	; load B5 -> A
	sta Temp	; B5 -> temp
	ldx wide_sprite4,y	; load B4 -> X
	lda wide_sprite3,y	; load B3 -> A
	ldy Temp	; load B5 -> Y
	sta GRP1	; B3 -> [GRP1]; B2 -> GRP0
	stx GRP0	; B4 -> [GRP0]; B3 -> GRP1
	sty GRP1	; B5 -> [GRP1]; B4 -> GRP0
	sta GRP0	; ?? -> [GRP0]; B5 -> GRP1
	dec LoopCount	; go to next line
	bpl BigLoop	; repeat until < 0
	rts
