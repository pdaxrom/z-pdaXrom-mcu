; MCU BUILTIN IO

REG_DDRP2   equ $01     ; PORT 2 DDR
REG_PORT2   equ $03     ; PORT 2 I/O Address
REG_TCSR1   equ $08     ;Timer Control/Status Reg 1
REG_FRCH    equ $09     ; Free Running Counter MSB
REG_FRCL    equ $0A     ; Free Running Counter LSB
REG_OCR1H   equ $0B     ; Output Compare Reg 1 MSB
REG_OCR1L   equ $0C     ; Output Compare Reg 1 LSB
REG_TCSR2   equ $0F     ;Timer Control/Status Reg 2
REG_RMCR    equ $10     ;RATE AND MODE CONTROL REGISTER
REG_TRCSR1  equ $11     ;TXD/RXD CONTROL AND STATUS REG.
REG_RDR     equ $12     ;RECEIVE DATA REGISTER
REG_TDR     equ $13     ;TRANSMIT DATA REGISTER
REG_RP5CR   equ $14     ; RAM/PORT 5 Control register
REG_PORT5   equ $15     ; PORT 5 I/O Address
REG_DDRP6   equ $16     ; PORT 6 DDR       
REG_PORT6   equ $17     ; PORT 6 I/O Address
REG_DDRP5   equ $20     ; PORT 5 DDR
REG_TRCSR2  equ $1E     ;TXD/RXD CONTROL AND STATUS REG#2.
REG_TCSSR3  equ $1B     ;TIMER CONTROL AND STATUS REG#3.
REG_TCONR   equ $1C     ;TIMER2 CONSTANT REG.
REG_T2CNT   equ $1D     ;TIMER2 COUNTER REG.

REG_TRCSR	equ REG_TRCSR1

BIT_IRQ1E	equ	1
BIT_IRQ2E	equ	2
BIT_MRE		equ	4
BIT_HLTE	equ	8
BIT_AMRE	equ	16
BIT_STBYFLAG	equ	32
BIT_RAME	equ	64
BIT_STBYPWR	equ	128

; PAGE SELECTOR
MEMPAGE_SEL	equ	REG_PORT6
MEMPAGE_MASK	equ	$17
MEMPAGE_ROM	equ	$08
MEMPAGE_FMASK	equ	$E0

; IRQ ROUTER
INT_ROUTER	equ	$E6FE
INT_SEL_IRQ1	equ	0
INT_SEL_IRQ2	equ	1
INT_SEL_NMI	equ	2
INT_SEL_RES	equ	3

; SimpleIO module

IO_L8BIT	equ	$E6A0
IO_L7HI		equ	$E6A1
IO_L7LO		equ	$E6A2
IO_L2RGB	equ	$E6A3
IO_SWKEYS	equ	$E6A4

IO_TIM_CONFIG	equ	$E6A8
IO_TIM_STATUS	equ	$E6A8
IO_TIM_DIV	equ	$E6A9

IO_TIM_RUN	equ	$1
IO_TIM_IEN	equ	$40
IO_TIM_IRQ	equ	$80

; PSG

PSG_DATA	equ	$E6B0
PSG_ADDR	equ	$E6B1

; SPI HOST

SPI_DATA	equ	$E6C0
SPI_STATUS	equ	$E6C2
SPI_CONFIG	equ	$E6C2
SPI_PRESCALER	equ	$E6C3
SPI_POUT	equ	$E6C4

SPI_READY	equ	$80
SPI_SSM		equ	$20
SPI_16B		equ	$10
SPI_SS0		equ	$01
SPI_SS1		equ	$02

; PS2 IO
PS2_DATA	equ	$E6D0
PS2_CONFIG	equ	$E6D1

PS2_BIT_IRQ	equ	$80
PS2_BIT_IEN	equ	$40
PS2_BIT_RDY	equ	$20
PS2_BIT_BSY	equ	$10
PS2_BIT_TIM	equ	$08
PS2_BIT_E1	equ	$04
PS2_BIT_E0	equ	$02
PS2_BIT_REL	equ	$01

; VPU

VPU_DATA	EQU	$E610
VPU_ADDR	EQU	$E611
VPU_CONFIG	EQU	$E613
VPU_AUTOOFFS	EQU	$E614
VPU_VADDR	EQU	$E615
VPU_HSOFFS	EQU	$E617
VPU_VSOFFS	EQU	$E618
VPU_HSIZE	EQU	$E619
VPU_VSIZE	EQU	$E61A
VPU_CURPOS	EQU	$E61B
VPU_CURLINS	EQU	$E61C
VPU_CURLINE	EQU	$E61D

VPU_B_IRQ	EQU	$80
VPU_B_IEN	EQU	$40
VPU_B_VBL	EQU	$20
VPU_B_GRF	EQU	$10
VPU_B_CURINV	EQU	$08
VPU_B_CUR	EQU	$04
VPU_B_ID	EQU	$02
VPU_B_AUTO	EQU	$01

;
