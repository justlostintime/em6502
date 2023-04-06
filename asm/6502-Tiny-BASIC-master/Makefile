
ASM=/usr/bin/dasm

all: mytbm.hex

clean:
	rm -f ../bin/mytbm.bin *.hex *.lst *.sym

mytbm.hex: mytb.asm storage.asm support.asm basic.il config.inc IL.inc
	$(ASM) mytb.asm -v4 -R -l../../lst/mytbm.lst -o../../bin/mytbm.bin -s../../lst/mytbm.sym



