	processor 6502
	include "vcs.h"
	include "macro.h"
	include "xmacro.h"

row_height_bits = 3
row_height_scanlines = 1 << row_height_bits
number_of_visible_rows = (192 / row_height_scanlines) - 1

invincibility_time = 200
invincible_warning_time = 20
invincibility_time_final = 216

sprite_screen_y = 40

min_x = 32-4
max_x = min_x + 15 * 8 - 4
starting_x = (max_x + min_x) / 2

starting_health = 3

gravity = 3
collision_bounce_velocity = -2

	seg.u variables
	org $80

zp_clear_start:

temp0 ds 1

position_frac ds 1
position_lo	ds 1
position_hi	ds 1

velocity_y_frac ds 1
velocity_y_lo ds 1
velocity_y_hi ds 1

avatar_x ds 1
health ds 1
invincible_count ds 1

zp_clear_end:

sprite_ptr ds 2
sprite_ptr_hi = sprite_ptr+1
sprite_ptr_lo = sprite_ptr

; Used in kernel
level_row ds 1
rows_left ds 1
vertical_shift ds 1
level_color ds 1

row_pf1l ds 1
row_pf2l ds 1
row_pf0r ds 1
row_pf1r ds 1
row_pf2r ds 1

	ECHO "Zero page used up to", *, "=", *-$80, "bytes"

	seg code
	org  $f000

; NTSC timing:
; 40 scanlines vblank (3 with vsync)
; 192 scanlines kernel
; 30 scanlines overscan
; Total: 262 scanlines

game_start:
	lda #0
	tax
.each_zp_byte:
	sta zp_clear_start,x
	inx
	cpx #zp_clear_end-zp_clear_start
	bne .each_zp_byte

	lda #$1e
	sta COLUP0

	lda #$5-1
	sta AUDF0
	lda #$8
	sta AUDC0

	lda #0
	sta CTRLPF

	.if 0
	lda #$02    ; SCORE = different colors for left and right
	sta CTRLPF
	lda #$18
	sta COLUP0
	lda #$88
	sta COLUP1
	.endif

	lda #starting_x
	sta avatar_x

	lda #starting_health
	sta health

	lda #>avatar_sprite
	sta sprite_ptr_hi
	lda #<avatar_sprite  ; = $00, sprites start at $xx00
	sta sprite_ptr_lo

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
	sta PF0
	sta PF1
	sta PF2

	; Check input

	lda health
	beq .move_done
	lda SWCHA
	asl
	bcs .not_right
	ldx avatar_x
	cpx #max_x
	bcs .move_done
	inc avatar_x
.not_right:
	asl
	bcs .not_left
	ldx avatar_x
	cpx #min_x
	bcc .not_left
	dec avatar_x
.not_left:
.move_done:

;	asl
;	bcs .not_down
;	iny
;.not_down:
;	asl
;	bcs .not_up
;	ldy #$ff
;	sty temp0
;.not_up:

	clc
	lda position_frac
	adc velocity_y_frac
	sta position_frac

	lda position_lo
	adc velocity_y_lo
	sta position_lo

	lda position_hi
	adc velocity_y_hi
	sta position_hi

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
	sta level_row

	lda #number_of_visible_rows
	sta rows_left

	; Calculate vertical shift for scrolling
	lda position_lo
	and #row_height_scanlines-1
	eor #row_height_scanlines-1  ; how many scanlines to shift down
	sta vertical_shift

	; Invincibility and flash visuals, and calculate sprite position

	lda #$02
	sta COLUBK

	lda #>avatar_sprite
	sta sprite_ptr_hi

	lda invincible_count
	beq .not_invincible

	adc #-invincibility_time+$0f
	bcc .no_flash
	sta COLUBK
	sta AUDV0
.no_flash:

	lda invincible_count
	cmp #invincible_warning_time
	bcs .broken
	ldy health
	beq .hidden
	and #1
	beq .not_invincible
.broken:
	lda #>broken_sprite
	sta sprite_ptr_hi
	jmp .invincibility_done
.hidden:
	lda #>gameover_sprite
	sta sprite_ptr_hi
.not_invincible:
.invincibility_done

	sta CXCLR  ; clear collisions

	; Wait for end of vblank
	TIMER_WAIT
	;lda #0   ; already 0 from TIMER_WAIT
	sta VBLANK

	; Start of visible graphics

	ldy vertical_shift
	jsr shift_y_lines
	ldy vertical_shift
	jsr enter_kernel

	sta WSYNC

	lda #$00
	sta PF0
	sta PF1
	sta PF2
	sta GRP0

	lda vertical_shift
	eor #row_height_scanlines-1
	tay
	jsr shift_y_lines

	sta WSYNC
	lda #$00
	sta COLUBK

	TIMER_SETUP 29  ; NTSC: 30 lines overscan

	lsr SWCHB ; test reset switch
	bcc game_reset

	ldx invincible_count
	bne .invincible

	lda CXP0FB
	bpl .after_collision

	lda #collision_bounce_velocity
	sta velocity_y_lo
	lda #$ff
	sta velocity_y_frac
	sta velocity_y_hi

	ldx health
	dex
	stx health
	beq .on_death_collision
	lda #invincibility_time
.after_on_death:
	sta invincible_count

.after_collision:

	; Apply gravity to velocity. Skipped if game over.
	clc
	lda velocity_y_frac
	adc #gravity
	sta velocity_y_frac
	lda velocity_y_lo
	adc #0
	sta velocity_y_lo
	lda velocity_y_hi
	adc #0
	sta velocity_y_hi

	TIMER_WAIT
	jmp game_frame

.on_death_collision:
	lda #$02    ; SCORE = different colors for left and right
	sta CTRLPF
	lda #$00
	sta COLUP0
	sta COLUP1
	lda #invincibility_time_final
	jmp .after_on_death

.invincible:
	dex
	beq .invincibility_ended
	stx invincible_count
	jmp .after_collision

.invincibility_ended:
	lda health
	beq game_over
	stx invincible_count
	jmp .after_collision

game_over:
game_reset:
	TIMER_WAIT
	jmp intro_start

KERNEL SUBROUTINE
; Kernel renders an "initialization" scanline followed by 7 "graphics" scanlines

	MAC set_color_on_line
;.execute_on_line = {1}
;.current_line = {2}
;.color = {3}
	IF {1} == {2}
	lda level_color   ; 3c
	ora #{3}          ; 2c
	sta COLUPF        ; 3c
	ENDIF
	ENDM

.enter_graphics:
scanline_in_row SET 1   ; because row 0 is initialization
	REPEAT row_height_scanlines - 1  ; minus 1 because first row is initialization
	sta WSYNC

	lda #0
	sta PF0
	lda row_pf1l
	sta PF1

	lda (sprite_ptr),y
	sta GRP0

	set_color_on_line 1, scanline_in_row, 6 * 2
	set_color_on_line 2, scanline_in_row, 7 * 2
	set_color_on_line 3, scanline_in_row, 6 * 2
	set_color_on_line 4, scanline_in_row, 5 * 2
	set_color_on_line 5, scanline_in_row, 4 * 2
	set_color_on_line 6, scanline_in_row, 3 * 2
	set_color_on_line 7, scanline_in_row, 1 * 2

	lda row_pf2l
	sta PF2

	lda row_pf0r
	sta PF0
	lda row_pf1r
	sta PF1
	lda row_pf2r
	sta PF2

	iny  ; next sprite pointer

scanline_in_row SET scanline_in_row + 1
	REPEND

	dec rows_left
	beq .end

enter_kernel:
	;sta WSYNC

	lda (sprite_ptr),y
	sta GRP0

	lda #0
	sta PF0
	sta PF1
	sta PF2

	ldx level_row
	inx
	stx level_row
	lda level,x
	tax
	lda bitmap_pf1l,x
	sta row_pf1l
	lda bitmap_pf2l,x
	sta row_pf2l
	lda bitmap_pf0r,x
	sta row_pf0r
	asl
	asl
	asl
	asl
	sta level_color

	lda bitmap_pf1r,x
	sta row_pf1r
	lda bitmap_pf2r,x
	sta row_pf2r

	iny
	jmp .enter_graphics
.end:
	rts

UTILITIES SUBROUTINE

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

MAIN SUBROUTINE
main_start:
	CLEAN_START
intro_start:

	jmp game_start

; Delay Y scanlines.
; Zero flag must correspond to the value in Y before call.
shift_y_lines:
	beq .end_shift
.each_shift:
	sta WSYNC
	dey
	bne .each_shift
.end_shift:
	rts

DATA SUBROUTINE

level_data_start:
	ALIGN $100
	.include "level.asm"
	echo "Size of levels:", *-level_data_start

	ALIGN $100
avatar_sprite:
	ds sprite_screen_y,0
visible_sprite_start:
	INCBIN "whale.dat"
visible_sprite_height = *-visible_sprite_start
	ds 192 - sprite_screen_y - visible_sprite_height,0

	IF * - avatar_sprite > $100
	ECHO "Must fit on page"
	ERROR
	ENDIF

	ALIGN $100
broken_sprite:
	ds sprite_screen_y,0
	INCBIN "deadwhale.dat"
	ds 192 - sprite_screen_y - visible_sprite_height,0

	IF * - broken_sprite > $100
	ECHO "Must fit on page"
	ERROR
	ENDIF

	ALIGN $100
gameover_sprite:
	ds 192,0

bytes_left = $fffc-*
	echo "Bytes left:", bytes_left

VECTORS SUBROUTINE
	org $fffc
	.word main_start
	.word main_start

; vim: set ts=16 sw=16 tw=0 noet :
