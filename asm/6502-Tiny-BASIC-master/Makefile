
ASM=/usr/bin/dasm

all: mytb.hex

clean:
	rm -f ../bin/mytb.bin *.hex *.lst *.sym

mytb.hex: mytb.asm storage.asm support.asm basic.il config.inc IL.inc
	$(ASM) mytb.asm -R -l../mytb.lst -o../../bin/mytb.bin -s../mytb.sym



