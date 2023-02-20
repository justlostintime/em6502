
ASM=/usr/bin/dasm

all: ctmon65.hex

clean:
	rm -f ../../bin/ctmon65.bin ctmon65.hex ../../lst/ctmon65.lst ../../sym/ctmonsym.sym


ctmon65.hex: ctmon65.asm acia.asm config.inc io.asm
	$(ASM) ctmon65.asm -R -l../../lst/ctmon65.lst -o../../bin/ctmon65.bin -s../../sym/ctmon65.sym



