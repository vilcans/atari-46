; Macro to delay a specific number of cycles.
; May clobber y register and zero flag but uses fewer instructions for longer delays
;
; Has to add extra instructions in certain cases to avoid page spanning jumps
;   fa  fb  fc  fd  fe  ff  00  01  02  03  04
;   ldy #   dey bne -   .                        OK
;       ldy #   dey bne -   .                    SPANS PAGE - add 3 bytes to align
;           ldy #   dey bne -   .                SPANS PAGE - add 2 bytes to align
;               ldy #   dey bne -   .            SPANS PAGE - add 1 byte to align
;                   ldy #   dey bne -   .        OK
	MAC SLEEP_Y
.cycles SET {1}
	IF (.cycles >= 8) && (<*) >= $fb) && ((<*) < $fe)
.align_nops SET ($fe - <*)
		ds .align_nops, $ea
.cycles SET .cycles - .align_nops * 2
	ENDIF
	IF .cycles >= 8
.iterations = (.cycles - 3) / 5
		ldy #.iterations  ;2c
.each:
		dey  ;2c
		bne .each  ; 3c/2c
.cycles SET .cycles - (2 + .iterations * 5 - 1)
	ENDIF
	IF .cycles & 1
		nop 0
.cycles SET .cycles - 3
	ENDIF
	REPEAT .cycles / 2
		nop
	REPEND

	ENDM
