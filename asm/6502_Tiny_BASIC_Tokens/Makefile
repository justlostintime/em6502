
ASM=/usr/bin/dasm

all: mytb.hex

clean:
	rm -f ../../bin/mytb.bin ../../bin/mytb.hex ../../lst/mytb.lst ../../sym/mytb.sym

mytb.hex: mytb.asm tokenizer.asm print.asm mem.asm gosub.asm ilvectortable.asm ipc.asm  compile.asm storage.asm tasks.asm support.asm basic.il config.inc ILKeyText.inc IL.inc io.asm
	$(ASM) mytb.asm -v4 -R -l../../lst/mytb.lst -o../../bin/mytb.bin -s../../sym/mytb.sym



