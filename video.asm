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

sprite_x_offset = 35  ; in cycles

display_wide_sprite:
	; Expects a timer setup for when to start rendering

	lda #wide_sprite_height-1
	sta LoopCount	; scanline counter
	lda #%011
	sta NUSIZ0
	sta NUSIZ1	; both players have 3 copies

	sta WSYNC
	SLEEP sprite_x_offset
	sta RESP0	; position 1st player
	sta RESP1	; ...and 2nd player
	lda #$10
	sta HMP1	; 1 pixel to the left
	sta WSYNC
	sta HMOVE	; apply HMOVE

.waittimer
	lda INTIM
	bne .waittimer
	sta VBLANK
	sta WSYNC

	lda #1
	sta VDELP0	; we need the VDEL registers
	sta VDELP1	; so we can do our 4-store trick

	sta WSYNC
	SLEEP sprite_x_offset + 76 - 51
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
