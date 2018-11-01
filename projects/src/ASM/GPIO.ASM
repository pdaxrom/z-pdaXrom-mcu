;
; Check GPIO
;
; GPIO 0 - LED
; GPIO 1 - LED
; GPIO 2 - BUTTON to GND
; GPIO 3 - BUTTON to GND
;

	include ../DEVMAP.INC

	include ../ROM/BOOTMEM.INC

	include ../ROM/BOOTROM.INC

	org $100

	sei

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	ldaa	#$03
	staa	GPIO_DIR+3

loop	ldab	GPIO_DATA+3
	pshb
	andb	#$0C
	rorb
	rorb
	stab	GPIO_DATA+3
	pulb

	pshb
	andb	#$0F
	ldx	#ledhex
	abx
	ldaa	0,x
	staa	IO_L7LO

	pulb
	rorb
	rorb
	rorb
	rorb
	andb	#$0F
	ldx	#ledhex
	abx
	ldaa	0,x
	staa	IO_L7HI

	bra	loop

hello	db	'GPIO test', 10, 13, 0

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71
