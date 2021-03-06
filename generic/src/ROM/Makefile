UNIASM = ../DEVEL/uniasm

ifneq (Microsoft, $(findstring Microsoft, $(shell uname -r)))
SCUBACMD = /opt/diamond/3.9_x64/ispfpga/bin/lin64/scuba
FOUNDRY  = /opt/diamond/3.9_x64/ispfpga
SCUBA = \
	export LD_LIBRARY_PATH="/opt/diamond/3.9_x64/tcltk/lib:/opt/diamond/3.9_x64/ispfpga/bin/lin64:/opt/diamond/3.9_x64/bin/lin64:/opt/diamond/3.9_x64/ispfpga/bin/lin64"; \
	export FOUNDRY="$(FOUNDRY)"; \
	export WSLENV="$WSLENV:FOUNDRY/l"; \
	$(SCUBACMD)
else
SCUBACMD = /mnt/Storage/Games/lscc/diamond/3.10_x64/ispfpga/bin/nt64/scuba.exe
FOUNDRY  = /mnt/Storage/Games/lscc/diamond/3.10_x64/ispfpga
SCUBA = \
	export FOUNDRY="$(FOUNDRY)"; \
	export WSLENV="$WSLENV:FOUNDRY/l"; \
	$(SCUBACMD)
endif

all: $(UNIASM) bootloader ../../rtl/mcu_rom.v ../../rtl/mcu_ram.v

$(UNIASM):
	make -C $(shell dirname $(UNIASM)) $(shell basename $(UNIASM))

BOOTROM.INC: ROM.ASM
	awk '{ if ($$1 == "romvecbeg") show=1; else if ($$1 == 'romvecend') show=0; else if (show == 1 && $$2 != "") { if ($$2 == "0") cnt++; else printf "F_%s\tEQU\t$$%02X\n",toupper($$2),0xc0+cnt++; } }' $^ > $@

rom.mem: ROM.ASM ../DEVMAP.INC
	cpp -P -traditional ROM.ASM > ROM.S
	$(UNIASM) ROM.S ROM.LST ROM.CMD
#	awk '/\yVEC.+ds/ { print $$3"\t\tEQU\t$$"$$2; }' ROM.LST >> BOOTROM.INC
	hexdump -v -e '"%04_ax: " 16/1 "%02x " "\n"' ROM.CMD > rom.mem
	cp -f ROM.CMD rom.bin

vrom.mem:
	cat vrom.mem.in > vrom.mem
#	gzip -dc video.roz | hexdump -v -e '"%04_ax: " 16/1 "%02x " "\n"' > vrom.mem

../../rtl/mcu_rom.v: rom.mem
	cd ../../rtl && \
	$(SCUBA) -w -n mcu_rom -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type bram -wp 00 -rp 1100 -addr_width 8 -data_width 8 -num_rows 256 -cascade -1 -memfile ../src/ROM/rom.mem -memformat orca
#	$(SCUBA) -w -n mcu_rom -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type rom -addr_width 8 -num_rows 2048 -data_width 8 -outdata REGISTERED -cascade -1 -resetmode SYNC -sync_reset  -memfile ROM/rom.mem -memformat orca

../../rtl/mcu_ram.v:
	cd ../../rtl && \
	$(SCUBA) -w -n mcu_ram -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type bram -wp 10 -rp 1000 -addr_width 12 -data_width 8 -num_rows 4096 -cascade -1 -mem_init0 -writemode NORMAL

../../rtl/vrom.v: vrom.mem
	cd ../../rtl && \
	$(SCUBA) -w -n vrom -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type bram -wp 00 -rp 1100 -addr_width 10 -data_width 8 -num_rows 1024 -cascade -1 -memfile ../src/ROM/vrom.mem -memformat orca
#	$(SCUBA) -w -n vrom -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type bram -wp 00 -rp 1100 -addr_width 11 -data_width 8 -num_rows 2048 -cascade -1 -memfile ROM/vrom.mem -memformat orca
#	$(SCUBA) -w -n vrom -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type rom -addr_width 8 -num_rows 2048 -data_width 8 -outdata UNREGISTERED -cascade -1 -resetmode SYNC -sync_reset  -memfile ROM/vrom.mem -memformat orca

../../rtl/vpu_cache.v:
	cd ../../rtl && \
	$(SCUBA) -w -n vpu_cache -lang verilog -synth lse -bus_exp 7 -bb -arch xo2c00 -type sdpram -rdata_width 8 -data_width 8 -num_rows 64 -outData UNREGISTERED

../../rtl/mcu_pll.v:
	cd ../../rtl && \
	$(SCUBA) -w -n mcu_pll -lang verilog -synth lse -arch xo2c00 -type pll -fin 12 -fclkop 24 -fclkop_tol 0.0 -fclkos 64 -fclkos_tol 0.0 -fclkos2 8 -fclkos2_tol 0.0 -trimp 0 -phasep 0 -trimp_r -trims 0 -phases 0 -trims_r -phases2 0 -phase_cntl STATIC -fb_mode 1 

bootloader: bootloader.c
	$(CC) $^ -o $@ -Wall -O2 -g

KERNEL.SYS: KERNEL.ASM BOOTROM.INC ../DEVMAP.INC FAT16.INC
	$(UNIASM) KERNEL.ASM KERNEL KERNEL.CMD
	mv -f KERNEL.CMD KERNEL.SYS

BOOTSEC.CMD: BOOTSEC.ASM BOOTROM.INC ../DEVMAP.INC FAT16.INC
	$(UNIASM) BOOTSEC.ASM BOOTSEC BOOTSEC.CMD

BOOTSECP.CMD: BOOTSECP.ASM BOOTROM.INC ../DEVMAP.INC FAT16.INC
	$(UNIASM) BOOTSECP.ASM BOOTSECP BOOTSECP.CMD

clean:
	rm -f rom.bin *.LST *.CMD *.o *.mem bootloader *.SYS *.CMD *.ERR *.S
	rm -f ../../rtl/mcu_rom* ../../rtl/mcu_ram* ../../rtl/vrom.* ../../rtl/mcu_pll.* ../../rtl/vpu_cache.*
	rm -f ../../rtl/tb_mcu_rom* ../../rtl/tb_mcu_ram*
