UNIASM = ../../generic/src/DEVEL/uniasm

all: $(UNIASM) bootloader rom.bin

$(UNIASM):
	make -C $(shell dirname $(UNIASM)) $(shell basename $(UNIASM))

rom.bin: $(UNIASM)
	cpp -P -traditional -DHD6303 ../../generic/src/ROM/ROM.ASM > ROM.S
	$(UNIASM) ROM.S ROM.LST ROM.CMD
	dd if=/dev/zero ibs=1k count=128 | tr "\000" "\377" >rom.bin
	dd if=ROM.CMD of=rom.bin conv=notrunc bs=1 seek=$$((0x1e00))

bootloader: ../../generic/src/ROM/bootloader.c
	$(CC) $^ -o $@ -Wall -O2 -g

clean:
	rm -f rom.bin *.LST *.CMD *.o *.mem bootloader *.SYS *.CMD *.ERR *.S
