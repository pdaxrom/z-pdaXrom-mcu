#UNIASM = ../DEVEL/uniasm
AS03 = ../ASM/SMALL-C/as03

all: $(AS03) bootloader rom.bin

$(UNIASM):
	make -C $(shell dirname $(UNIASM)) $(shell basename $(UNIASM))

$(AS03):
	make -C $(shell dirname $(AS03)) $(shell basename $(AS03))

rom.bin: $(AS03)
	cp ../../generic/src/ROM/BOOTROM.INC .
	cp ../../generic/src/ROM/BOOTMEM.INC .
	sed -i -e 's/$FF00/$FE00/' BOOTMEM.INC
	cpp -P -traditional -DHD6303Y ../../generic/src/ROM/ROM.ASM > ROM.S
	$(AS03) -l ROM.LST -o ROM.CMD ROM.S
	dd if=/dev/zero ibs=1k count=128 | tr "\000" "\377" >rom.bin
	dd if=ROM.CMD of=rom.bin conv=notrunc bs=1 seek=$$((0x1fe00))
	dd if=/dev/zero ibs=1k count=4 | tr "\000" "\377" >bios.bin
	dd if=ROM.CMD of=bios.bin conv=notrunc bs=1 seek=$$((0x0e00))

bootloader: ../../generic/src/ROM/bootloader.c
	$(CC) $^ -o $@ -Wall -O2 -g

clean:
	rm -f bios.bin rom.bin *.LST *.CMD *.o *.mem bootloader *.SYS *.CMD *.ERR *.S BOOTROM.INC BOOTMEM.INC
