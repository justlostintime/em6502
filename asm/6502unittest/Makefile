
ASM=/usr/bin/dasm

all: unittest.hex

clean:
	rm -f ../../bin/unittest.bin *.hex ../unittest.lst ../unittest.sym

unittest.hex: unittest.asm
	$(ASM) unittest.asm  -d -v4 -R -l../unittest.lst -o../../bin/unittest.bin -s../unittest.sym



