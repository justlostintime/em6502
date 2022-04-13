
ASM=/usr/bin/dasm

all: useirq.bin

clean:
	rm -f ../bin/useirq.bin useirq.hex useirq.lst useirq.sym

useirq.bin: useirq.asm ctmon65.inc config.inc
	$(ASM) useirq.asm -R -l../useirq.lst -o../../bin/useirq.bin -s../useirq.sym

