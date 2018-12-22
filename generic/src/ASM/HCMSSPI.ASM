;
; HJ12864-COG-5
;
; GPIO 3 - PIN_RS
; GPIO 4 - PIN_RSTB
; MSS    - PIN_CSB
; MOSI   - PIN_SDA
; MSCK   - PIN_SCLK
;

PIN_RS		equ	$08
PIN_RSTB	equ	$10

	include ../DEVMAP.INC

	include ../ROM/BOOTMEM.INC

	include ../ROM/BOOTROM.INC

	org $100

	sei

	lds	#$fff

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	jsr	disp_init

	ldx	#ledhex
	jsr	send_data

	jmp	F_RESET

send_byte proc
;	jsr	printhex
busy	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy
	staa	SPI_DATA+1
	rts
	endp

send_cmd proc
;	jsr	printhex
	pshb

	ldab	GPIO_DATA+3
	orab	#PIN_RS
	stab	GPIO_DATA+3

	ldab	SPI_CONFIG
	andb	#$FF^SPI_SS0
	stab	SPI_CONFIG

	ldab	#8
loop	pshb
	bsr	send_byte
	pulb
	decb
	bne	loop

busy	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy

	ldab	SPI_CONFIG
	orab	#SPI_SS0
	stab	SPI_CONFIG

	pulb
	rts
	endp

send_data proc
;	jsr	printhex
	psha
	pshb
	pshx

	ldab	GPIO_DATA+3
	andb	#$FF^PIN_RS
	stab	GPIO_DATA+3

	ldab	SPI_CONFIG
	andb	#$FF^SPI_SS0
	stab	SPI_CONFIG

	ldab	#160
loop	pshb
	ldaa	0,x
	bsr	send_byte
	inx
	pulb
	decb
	bne	loop

busy	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy

	ldab	SPI_CONFIG
	orab	#SPI_SS0
	stab	SPI_CONFIG

	pulx
	pulb
	pula
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
	ldaa	#(PIN_RS | PIN_RSTB)
	staa	GPIO_DIR+3

	ldaa	#2
	staa	SPI_PRESCALER
	ldaa	#(SPI_SSM | SPI_SS0)
	staa	SPI_CONFIG

	ldaa	GPIO_DATA+3
	anda	#$FF^PIN_RSTB
	staa	GPIO_DATA+3

	ldaa	#20
	bsr	delay

	ldab	GPIO_DATA+3
	orab	#PIN_RSTB
	stab	GPIO_DATA+3

	ldaa	#$4c	; 01001100
	jsr	send_cmd

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

hello	db	'HCMS test', 10, 13, 0

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71

