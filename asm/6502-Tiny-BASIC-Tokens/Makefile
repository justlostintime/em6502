
ASM=/usr/bin/dasm

all: mytb.hex

clean:
	rm -f ../../bin/mytb.bin *.hex *.lst *.sym

mytb.hex: mytb.asm storage.asm tasks.asm support.asm basic.il config.inc IL.inc io.asm
	$(ASM) mytb.asm -v4 -R -l../../lst/mytb.lst -o../../bin/mytb.bin -s../../map/mytb.sym



