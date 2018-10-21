BOOTROM_BASE	equ	$F800

		section	$40
VIRQ		ds	2
VSWI		ds	2
VNMI		ds	2
VRST		ds	2
END_ADDR	ds	2
		ends
