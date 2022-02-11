
ASM=/usr/bin/dasm

all: ctmon65.hex

clean:
	rm -f ../bin/ctmon65.bin *.hex *.lst

ctmon65.hex: ctmon65.asm acia.asm config.inc io.asm
	$(ASM) ctmon65.asm -R -l../ctmon65.lst -o../../bin/ctmon65.bin -s../ctmon65.sym



