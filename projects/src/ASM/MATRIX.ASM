;
; MATRIX LED
;
; GPIO 0 - DIN
; GPIO 1 - CS
; GPIO 2 - CLK
;

PIN_DIN		equ	1
PIN_CS		equ	2
PIN_CLK		equ	4

REG_NOOP	equ	0
REG_DIGIT_0	equ	$100
REG_DIGIT_1	equ	$200
REG_DIGIT_2	equ	$300
REG_DIGIT_3	equ	$400
REG_DIGIT_4	equ	$500
REG_DIGIT_5	equ	$600
REG_DIGIT_6	equ	$700
REG_DIGIT_7	equ	$800
REG_DECODE_MODE	equ	$900
REG_INTENSITY	equ	$A00
REG_SCAN_LIMIT	equ	$B00
REG_SHUTDOWN	equ	$C00
REG_DISP_TEST	equ	$F00

	include ../DEVMAP.INC

	include ../ROM/BOOTMEM.INC

	include ../ROM/BOOTROM.INC

	org $100

	sei

	lds	#$7ff

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	ldaa	#$07
	staa	GPIO_DIR+3
	ldaa	GPIO_DATA+3
	oraa	#PIN_CS
	staa	GPIO_DATA+3

	bsr	disp_init

	ldx	#heart
	bsr	disp_show

	jmp	F_RESET

disp_init proc
	ldaa	#$03
	bsr	disp_intensity
	bsr	disp_clean
	rts
	endp

disp_intensity proc
	pshb
	psha
	ldd	#REG_SHUTDOWN | $01
	bsr	senddata
	ldd	#REG_DECODE_MODE | $00
	bsr	senddata
	ldd	#REG_SCAN_LIMIT | $07
	bsr	senddata
	pula
	psha
	tab
	clra
	addd	#REG_INTENSITY
	bsr	senddata
	pula
	pulb
	rts
	endp

disp_clean proc
	pshx
	pshb
	psha
	ldx	#8
	ldd	#REG_DIGIT_0
loop	bsr	senddata
	addd	#$100
	dex
	bne	loop
	pula
	pulb
	pulx
	rts
	endp

disp_show proc
	psha
	pshb
	pshx
	pshy
	ldy	#8
	ldd	#REG_DIGIT_0

loop	ldab	0,x
	bsr	senddata
	adda	#1
	inx
	dey
	bne	loop

	puly
	pulx
	pulb
	pula
	rts
	endp

senddata proc
;	jsr	printhex
	pshb
	psha
	pshx
	xgdx
	ldaa	GPIO_DATA+3
	anda	#$FF^PIN_CS
	staa	GPIO_DATA+3
	xgdx
	ldx	#16
loop	;jsr	printhex
	psha
	tsta
	bmi	setbit
	ldaa	GPIO_DATA+3
	anda	#$FF^PIN_DIN
	bra	writebit
setbit	ldaa	GPIO_DATA+3
	oraa	#PIN_DIN
writebit staa	GPIO_DATA+3

	ldaa	GPIO_DATA+3
	oraa	#PIN_CLK
	staa	GPIO_DATA+3
	anda	#$FF^PIN_CLK
	staa	GPIO_DATA+3

	pula
	asld
	dex
	bne	loop

	ldaa	GPIO_DATA+3
	oraa	#PIN_CS
	staa	GPIO_DATA+3
	pulx
	pula
	pulb
	rts
	endp

printhex proc
	pshx
	psha
	pshb
	jsr	F_UART_PUTHEX
	tba
	jsr	F_UART_PUTHEX
	ldx	#crlf
	jsr	F_UART_PUTS
	pulb
	pula
	pulx
	rts
crlf	db	10, 13, 0
	endp

hello	db	'MATRIX test', 10, 13, 0

; 0 0 0 0 0 0 0 0
; 0 1 1 0 0 1 1 0
; 1 1 1 1 1 1 1 1
; 1 1 1 1 1 1 1 1
; 1 1 1 1 1 1 1 1
; 0 1 1 1 1 1 1 0
; 0 0 1 1 1 1 0 0
; 0 0 0 1 1 0 0 0

heart	db	$38, $7C, $7E, $3F, $3F, $7E, $7C, $38

; 0 0 0 0 1 0 0 0
; 0 0 0 0 1 0 0 0
; 0 0 0 1 1 1 0 0
; 0 0 1 0 1 0 1 0
; 0 1 0 0 1 0 0 1
; 0 0 0 0 1 0 0 0
; 0 0 0 0 1 0 0 0
; 0 0 0 1 0 1 0 0

;heart	db	$00, $08, $10, $21, $FE, $21, $10, $08

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71
