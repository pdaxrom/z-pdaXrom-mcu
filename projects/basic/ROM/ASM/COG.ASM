;
; HJ12864-COG-5
;
; GPIO 3 - PIN_RS
; GPIO 4 - PIN_RSTB
; GPIO 5 - PIN_CSB
; GPIO 6 - PIN_SDA
; GPIO 7 - PIN_SCLK
;

PIN_RS		equ	$08
PIN_RSTB	equ	$10
PIN_CSB		equ	$20
PIN_SDA		equ	$40
PIN_SCLK	equ	$80

	include ../DEVMAP.INC

	include ../BOOTMEM.INC

	include ../BOOTROM.INC

	org $100

	sei

	lds	#$7ff

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	ldaa	#(PIN_RS | PIN_RSTB | PIN_CSB | PIN_SDA | PIN_SCLK)
	staa	GPIO_DIR+3

	bsr	disp_init

	ldx	#$FF00
	jsr	lcd_test

	jmp	F_RESET

send_byte proc
;	jsr	printhex
	ldy	#8
loop	ldab	GPIO_DATA+3
	andb	#$FF^PIN_SCLK
	stab	GPIO_DATA+3

	ldab	GPIO_DATA+3
	asla
	bcs	setbit
	andb	#$FF^PIN_SDA
	bra	writebit
setbit	orab	#PIN_SDA
writebit stab	GPIO_DATA+3

	ldab	GPIO_DATA+3
	orab	#PIN_SCLK
	stab	GPIO_DATA+3
	dey
	bne	loop
	rts
	endp

send_cmd proc
;	jsr	printhex
	pshy
	pshb
	psha
	ldab	GPIO_DATA+3
	andb	#$FF^(PIN_CSB|PIN_RS)
	stab	GPIO_DATA+3

	bsr	send_byte

	ldab	GPIO_DATA+3
	orab	#PIN_CSB
	stab	GPIO_DATA+3
	pula
	pulb
	puly
	rts
	endp

send_data proc
;	jsr	printhex
	pshy
	pshb
	psha
	ldab	GPIO_DATA+3
	andb	#$FF^PIN_CSB
	orab	#PIN_RS
	stab	GPIO_DATA+3

	bsr	send_byte

	ldab	GPIO_DATA+3
	orab	#PIN_CSB
	stab	GPIO_DATA+3
	pula
	pulb
	puly
	rts
	endp

delay	proc
	pshb
	psha
loop	ldab	#110
loop1	decb
	bne	loop1
	deca
	bne	loop
	pula
	pulb
	rts
	endp

disp_init proc
	ldab	GPIO_DATA+3
	andb	#$FF^(PIN_CSB|PIN_RSTB)
	stab	GPIO_DATA+3
	ldaa	#20
	bsr	delay
	ldab	GPIO_DATA+3
	orab	#PIN_RSTB
	stab	GPIO_DATA+3

	ldaa	#20
	bsr	delay

	ldx	#init1
loop1	ldaa	0,x
	bsr	send_cmd

	ldaa	#50
	bsr	delay

	inx
	cpx	#init1+4
	bne	loop1

	ldx	#init2
loop2	ldaa	0,x
	bsr	send_cmd

	inx
	cpx	#init2+8
	bne	loop2

	ldab	GPIO_DATA+3
	orab	#PIN_CSB
	stab	GPIO_DATA+3
	rts
init1	db	$e2, $2c, $2e, $2f
init2	db	$24, $81, $1f, $a2, $c8, $a0, $60, $af
	endp

lcd_address proc
	psha
	pshb
	deca
	adda	#$B0
	jsr	send_cmd
	tba
	lsra
	lsra
	lsra
	lsra
	adda	#$10
	jsr	send_cmd
	tba
	anda	#$0F
	jsr	send_cmd
	pulb
	pula
	rts
	endp

lcd_test proc
	ldaa	#0
loop	psha
	adda	#$B0
	jsr	send_cmd
	ldaa	#$10
	jsr	send_cmd
	ldaa	#$01
	jsr	send_cmd

	ldaa	#66
loop1	psha
	pshb
	ldaa	#$00
	jsr	send_data
	ldaa	#$FF
	jsr	send_data
	pulb
	pula
	deca
	bne	loop1

;	jsr	F_UART_IN

	pula
	inca
	cmpa	#9
	bne	loop
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

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71
