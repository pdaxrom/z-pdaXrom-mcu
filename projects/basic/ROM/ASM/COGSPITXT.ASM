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

	include ../BOOTMEM.INC

	include ../BOOTROM.INC

	org $100

	sei

	lds	#$fff

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	bsr	disp_init

	ldaa	#0
	ldab	#0
	jsr	gotoxy

	ldaa	#0
loop	jsr	printchar

;	ldx	#screen
;	jsr	lcd_show

	inca
	cmpa	#$80
	bne	loop

	ldx	#screen
	jsr	lcd_show

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
	andb	#$FF^PIN_RS
	stab	GPIO_DATA+3
	bsr	send_byte
	pulb
	rts
	endp

send_data proc
;	jsr	printhex
	pshb
	ldab	GPIO_DATA+3
	orab	#PIN_RS
	stab	GPIO_DATA+3
	bsr	send_byte
	pulb
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
	ldaa	#SPI_SSM
	staa	SPI_CONFIG

	ldaa	GPIO_DATA+3
	anda	#$FF^PIN_RSTB
	staa	GPIO_DATA+3

	ldaa	#20
	bsr	delay

	ldab	GPIO_DATA+3
	orab	#PIN_RSTB
	stab	GPIO_DATA+3

	ldaa	#0
	staa	SPI_CONFIG

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

lcd_show proc
	pshy
	pshx
	pshb
	psha
	ldy	#ROT8x8_IO
	ldaa	#0
loop	psha
busy1	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy1
	adda	#$B0
	jsr	send_cmd
	ldaa	#$10
	jsr	send_cmd
	ldaa	#$01
	jsr	send_cmd

busy2	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy2

	ldaa	#16
loop1	psha
	ldaa	$00,x
	staa	0,y
	ldaa	$10,x
	staa	1,y
	ldaa	$20,x
	staa	2,y
	ldaa	$30,x
	staa	3,y
	ldaa	$40,x
	staa	4,y
	ldaa	$50,x
	staa	5,y
	ldaa	$60,x
	staa	6,y
	ldaa	$70,x
	staa	7,y
	ldaa	0,y
	jsr	send_data
	ldaa	1,y
	jsr	send_data
	ldaa	2,y
	jsr	send_data
	ldaa	3,y
	jsr	send_data
	ldaa	4,y
	jsr	send_data
	ldaa	5,y
	jsr	send_data
	ldaa	6,y
	jsr	send_data
	ldaa	7,y
	jsr	send_data
	inx
	pula
	deca
	bne	loop1

	ldab	#128-16
	abx

;	jsr	F_UART_IN

	pula
	inca
	cmpa	#8
	beq	exit
	jmp	loop
exit	pula
	pulb
	pulx
	puly
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

videotxt proc
	global	gotoxy, putchar, printchar
gotoxy	pshx
	psha
	pshb
	std	curx
	clra
	asld
	asld
	asld
	asld

	asld
	asld
	asld

	addd	#screen
	std	scradr
	clra
	ldab	curx
	addd	scradr
	std	scradr
	pulb
	pula
	pulx
	rts

printchar pshx
	psha
	bsr	putchar
	inc	curx
	ldaa	curx
	cmpa	#16
	blo	printc1
	clr	curx
	ldaa	cury
	cmpa	#7
	blo	printc0

printc0	inc	cury
printc1 ldd	curx
	bsr	gotoxy
	pula
	pulx
	rts

putchar	pshy
	pshx
	psha
	pshb
	tab
	clra
	asld
	asld
	asld
	addd	#chargen
	xgdx
	ldy	scradr

	ldaa	0,x
	staa	0,y
	ldaa	1,x
	staa	$10,y
	ldaa	2,x
	staa	$20,y
	ldaa	3,x
	staa	$30,y
	ldaa	4,x
	staa	$40,y
	ldaa	5,x
	staa	$50,y
	ldaa	6,x
	staa	$60,y
	ldaa	7,x
	staa	$70,y

	pulb
	pula
	pulx
	puly
	rts
curx	db	0
cury	db	0
scradr	dw	screen
	endp


hello	db	'COGSPI TEXT test', 10, 13, 0

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71

chargen
    db  $00, $00, $00, $00, $00, $00, $00, $00, $3e, $41, $55, $41, $5d, $49, $41, $3e
    db  $3e, $7f, $6b, $7f, $63, $77, $7f, $3e, $36, $7f, $7f, $7f, $3e, $1c, $08, $00
    db  $08, $1c, $3e, $7f, $3e, $1c, $08, $00, $08, $1c, $1c, $6b, $7f, $6b, $08, $1c
    db  $08, $1c, $3e, $7f, $7f, $3e, $08, $1c, $00, $00, $18, $3c, $3c, $18, $00, $00
    db  $ff, $ff, $e7, $c3, $c3, $e7, $ff, $ff, $00, $3c, $66, $42, $42, $66, $3c, $00
    db  $ff, $c3, $99, $bd, $bd, $99, $c3, $ff, $0f, $03, $05, $39, $48, $48, $30, $00
    db  $1c, $22, $22, $1c, $08, $1c, $08, $00, $0f, $09, $0f, $08, $08, $38, $30, $00
    db  $1f, $11, $1f, $11, $11, $17, $76, $60, $08, $2a, $1c, $77, $1c, $2a, $08, $00
    db  $10, $18, $1c, $1e, $1c, $18, $10, $00, $04, $0c, $1c, $3c, $1c, $0c, $04, $00
    db  $08, $1c, $3e, $08, $3e, $1c, $08, $00, $14, $14, $14, $14, $00, $00, $14, $00
    db  $3f, $49, $49, $29, $09, $09, $09, $00, $1c, $20, $1c, $22, $22, $1c, $02, $1c
    db  $00, $00, $00, $00, $ff, $ff, $ff, $ff, $08, $1c, $3e, $08, $3e, $1c, $08, $7f
    db  $08, $1c, $3e, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $3e, $1c, $08
    db  $00, $08, $0c, $fe, $0c, $08, $00, $00, $00, $10, $30, $7f, $30, $10, $00, $00
    db  $00, $40, $40, $40, $7f, $00, $00, $00, $00, $14, $36, $7f, $36, $14, $00, $00
    db  $08, $1c, $3e, $7f, $00, $00, $00, $00, $00, $00, $00, $00, $7f, $3e, $1c, $08
    db  $00, $00, $00, $00, $00, $00, $00, $00, $08, $08, $08, $08, $08, $00, $08, $00
    db  $14, $14, $00, $00, $00, $00, $00, $00, $14, $14, $3e, $14, $3e, $14, $14, $00
    db  $08, $1e, $28, $1c, $0a, $3c, $08, $00, $00, $31, $32, $04, $08, $13, $23, $00
    db  $18, $24, $24, $18, $25, $22, $1d, $00, $08, $08, $00, $00, $00, $00, $00, $00
    db  $04, $08, $10, $10, $10, $08, $04, $00, $10, $08, $04, $04, $04, $08, $10, $00
    db  $00, $08, $2a, $1c, $1c, $2a, $08, $00, $00, $08, $08, $3e, $08, $08, $00, $00
    db  $00, $00, $00, $00, $00, $08, $08, $10, $00, $00, $00, $3e, $00, $00, $00, $00
    db  $00, $00, $00, $00, $00, $00, $08, $00, $00, $01, $02, $04, $08, $10, $20, $00
    db  $1c, $22, $26, $2a, $32, $22, $1c, $00, $08, $18, $08, $08, $08, $08, $1c, $00
    db  $1c, $22, $02, $0c, $10, $20, $3e, $00, $3e, $02, $04, $0c, $02, $22, $1c, $00
    db  $04, $0c, $14, $24, $3e, $04, $04, $00, $3e, $20, $3c, $02, $02, $22, $1c, $00
    db  $0e, $10, $20, $3c, $22, $22, $1c, $00, $3e, $02, $04, $08, $10, $10, $10, $00
    db  $1c, $22, $22, $1c, $22, $22, $1c, $00, $1c, $22, $22, $1e, $02, $04, $38, $00
    db  $00, $00, $08, $00, $08, $00, $00, $00, $00, $00, $08, $00, $08, $08, $10, $00
    db  $02, $04, $08, $10, $08, $04, $02, $00, $00, $00, $3e, $00, $3e, $00, $00, $00
    db  $10, $08, $04, $02, $04, $08, $10, $00, $1c, $22, $02, $04, $08, $00, $08, $00
    db  $1c, $22, $2a, $2e, $2c, $20, $1e, $00, $08, $14, $22, $22, $3e, $22, $22, $00
    db  $3c, $22, $22, $3c, $22, $22, $3c, $00, $1c, $22, $20, $20, $20, $22, $1c, $00
    db  $3c, $22, $22, $22, $22, $22, $3c, $00, $3e, $20, $20, $3c, $20, $20, $3e, $00
    db  $3e, $20, $20, $3c, $20, $20, $20, $00, $1e, $20, $20, $26, $22, $22, $1e, $00
    db  $22, $22, $22, $3e, $22, $22, $22, $00, $1c, $08, $08, $08, $08, $08, $1c, $00
    db  $02, $02, $02, $02, $02, $22, $1c, $00, $22, $24, $28, $30, $28, $24, $22, $00
    db  $20, $20, $20, $20, $20, $20, $3e, $00, $22, $36, $2a, $2a, $22, $22, $22, $00
    db  $22, $22, $32, $2a, $26, $22, $22, $00, $1c, $22, $22, $22, $22, $22, $1c, $00
    db  $3c, $22, $22, $3c, $20, $20, $20, $00, $1c, $22, $22, $22, $2a, $24, $1a, $00
    db  $3c, $22, $22, $3c, $28, $24, $22, $00, $1c, $22, $20, $1c, $02, $22, $1c, $00
    db  $3e, $08, $08, $08, $08, $08, $08, $00, $22, $22, $22, $22, $22, $22, $1c, $00
    db  $22, $22, $22, $22, $22, $14, $08, $00, $22, $22, $22, $2a, $2a, $36, $22, $00
    db  $22, $22, $14, $08, $14, $22, $22, $00, $22, $22, $14, $08, $08, $08, $08, $00
    db  $3e, $02, $04, $08, $10, $20, $3e, $00, $3e, $30, $30, $30, $30, $30, $3e, $00
    db  $00, $20, $10, $08, $04, $02, $00, $00, $3e, $06, $06, $06, $06, $06, $3e, $00
    db  $00, $00, $08, $14, $22, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ff
    db  $10, $08, $04, $00, $00, $00, $00, $00, $00, $00, $1c, $02, $1e, $22, $1e, $00
    db  $20, $20, $3c, $22, $22, $22, $3c, $00, $00, $00, $1e, $20, $20, $20, $1e, $00
    db  $02, $02, $1e, $22, $22, $22, $1e, $00, $00, $00, $1c, $22, $3e, $20, $1e, $00
    db  $0c, $12, $10, $38, $10, $10, $10, $00, $00, $00, $1c, $22, $22, $1e, $02, $1c
    db  $20, $20, $3c, $22, $22, $22, $22, $00, $08, $00, $18, $08, $08, $08, $1c, $00
    db  $02, $00, $06, $02, $02, $02, $12, $0c, $20, $20, $22, $24, $38, $24, $22, $00
    db  $18, $08, $08, $08, $08, $08, $1c, $00, $00, $00, $36, $2a, $2a, $2a, $2a, $00
    db  $00, $00, $3c, $22, $22, $22, $22, $00, $00, $00, $1c, $22, $22, $22, $1c, $00
    db  $00, $00, $3c, $22, $22, $3c, $20, $20, $00, $00, $1e, $22, $22, $1e, $02, $02
    db  $00, $00, $2e, $30, $20, $20, $20, $00, $00, $00, $1e, $20, $1c, $02, $3c, $00
    db  $10, $10, $38, $10, $10, $12, $0c, $00, $00, $00, $22, $22, $22, $26, $1a, $00
    db  $00, $00, $22, $22, $22, $14, $08, $00, $00, $00, $22, $22, $2a, $2a, $36, $00
    db  $00, $00, $22, $14, $08, $14, $22, $00, $00, $00, $22, $22, $22, $1e, $02, $1c
    db  $00, $00, $3e, $04, $08, $10, $3e, $00, $0e, $18, $18, $30, $18, $18, $0e, $00
    db  $08, $08, $08, $00, $08, $08, $08, $00, $38, $0c, $0c, $06, $0c, $0c, $38, $00
    db  $1a, $2c, $00, $00, $00, $00, $00, $00, $08, $08, $14, $14, $22, $22, $41, $7f

screen	ds	16*64
