;
; Check SPI
;

	include ../DEVMAP.INC

	include ../ROM/BOOTMEM.INC

	include ../ROM/BOOTROM.INC

	org $100

	sei

	ldx	#hello
;	int	F_PUTSTR
	jsr	F_UART_PUTS

	ldaa	SPI_DATA+1
	clrb
	bsr	printhex

	ldaa	#$A5
	staa	SPI_DATA+1

busy	ldab	SPI_STATUS
	bitb	#SPI_READY
	beq	busy

	ldaa	SPI_DATA+1
	clrb
	bsr	printhex

	jmp	F_RESET


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

hello	db	'SPI test', 10, 13, 0

ledhex	db	$3f, $06, $5B, $4F, $66, $6D, $7D, $07, $7F, $6F, $77, $7C, $39, $5E, $79, $71
