UNIASM = ../DEVEL/uniasm

all: $(UNIASM)		\
    IRQTEST.CMD		\
    GPIO.CMD		\
    MATRIX.CMD		\
    COG.CMD		\
    SPI.CMD		\
    MATRIXSPI.CMD	\
    COGSPI.CMD		\
    COGSPITXT.CMD	\
    HCMSSPI.CMD

$(UNIASM):
	make -C $(shell dirname $(UNIASM)) $(shell basename $(UNIASM))

.SUFFIXES: .ASM .CMD

.ASM.CMD: ../DEVMAP.INC ../BOOTROM.INC
	$(UNIASM) $< $(basename $@) $@

clean:
	rm -f *.LST *.CMD ymstreamer
