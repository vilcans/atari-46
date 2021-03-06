start_frame:
	; Call at start of frame. Sets vertical sync and a timer for vblank.
	; Follow up with:
	;   TIMER_WAIT
	;   sta VBLANK

	TIMER_SETUP vblank_scanlines

	lda #2
	sta VBLANK
	sta VSYNC
	sta WSYNC  ; Keep VSYNC on for 3 scanlines
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC
	rts

; Based on http://www.qotile.net/minidig/code/bigmove.asm
	seg.u variables

Temp		byte
LoopCount	byte

	seg code

sprite_x_offset = 35  ; in cycles

display_wide_sprite:
	; Expects a timer setup for when to start rendering

	lda #wide_sprite_height-1
	sta LoopCount	; scanline counter
	lda #%011
	sta NUSIZ0
	sta NUSIZ1	; both players have 3 copies
	lda #1
	sta VDELP0
	sta VDELP1

	sta WSYNC
	SLEEP_Y sprite_x_offset
	sta RESP0	; position 1st player
	sta RESP1	; ...and 2nd player, 3 cycles later = 9 pixels
	lda #$10
	sta HMP1	; 1 pixel to the left = 8 pixels
	sta WSYNC
	sta HMOVE	; 3c. apply HMOVE

.waittimer:
	lda INTIM
	bne .waittimer
	sta WSYNC

	SLEEP_Y sprite_x_offset + 76 - 51
BigLoop
	ldy LoopCount         ; 3c
	lda wide_sprite0,y    ; 4c load B0 (1st sprite byte)
	sta GRP0              ; 3c B0 -> [GRP0]
	lda wide_sprite1,y    ; 4c load B1 -> A
	sta GRP1              ; 3c B1 -> [GRP1], B0 -> GRP0
	; =17c

	SLEEP 76-(17+45)

	lda wide_sprite2,y    ; 4c load B2 -> A
	sta GRP0              ; 3c B2 -> [GRP0], B1 -> GRP1
	lda wide_sprite5,y    ; 4c load B5 -> A
	sta Temp              ; 3c B5 -> temp
	ldx wide_sprite4,y    ; 4c load B4 -> X
	lda wide_sprite3,y    ; 4c load B3 -> A
	ldy Temp              ; 3c load B5 -> Y
	sta GRP1              ; 3c B3 -> [GRP1]; B2 -> GRP0
	stx GRP0              ; 3c B4 -> [GRP0]; B3 -> GRP1
	sty GRP1              ; 3c B5 -> [GRP1]; B4 -> GRP0
	sta GRP0              ; 3c ?? -> [GRP0]; B5 -> GRP1
	dec LoopCount         ; 5c
	bpl BigLoop           ; 3c
	; =45c
	rts
