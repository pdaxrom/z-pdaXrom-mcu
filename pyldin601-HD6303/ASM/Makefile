#UNIASM = ../DEVEL/uniasm
AS03 = ./SMALL-C/as03

all: $(AS03)		\
    UART.CMD		\
    RAMCHK.CMD		\
    RAMTEST.CMD		\
    MICROBASIC.CMD	\
    MON.BIN		\
    SIMPLEIO.CMD	\
    IRQTEST.CMD		\
    HCMSSPI.CMD		\
    VPU.CMD		\
    VPUGRF.CMD		\
    VPUTXT.CMD		\
    VIRQTEST.CMD	\
    PSGDEMO.CMD		\
    CHIPTUNE.CMD	\
    RAMPAGECHK.CMD	\
    ROMPAGECHK.CMD	\
    PS2TEST.CMD		\
    PS2LEDS.CMD		\
    LOADBIOS.CMD	\
    LOADPAGE.CMD	\
    PS2KEY.CMD		\
    SDTEST.CMD		\
    MISC.CMD		\
    FLASHROM.CMD	\
    FLASHBIOS.CMD

$(UNIASM):
	make -C $(shell dirname $(UNIASM)) $(shell basename $(UNIASM))

$(AS03):
	make -C $(shell dirname $(AS03)) $(shell basename $(AS03))

.SUFFIXES: .ASM .CMD

#.ASM.CMD: ../DEVMAP.INC ../BOOTROM.INC
#	$(UNIASM) $< $(basename $@) $@

.ASM.CMD: ../DEVMAP.INC ../BOOTROM.INC
	$(AS03) -l $(basename $@).LST -o $@ $<

MON.BIN: 6303MON.CMD
	dd if=/dev/zero ibs=1k count=128 | tr "\000" "\377" >MON.BIN
	dd if=6303MON.CMD of=MON.BIN conv=notrunc bs=1 seek=$$((0xF000))


clean:
	rm -f *.LST *.CMD ymstreamer *.BIN
