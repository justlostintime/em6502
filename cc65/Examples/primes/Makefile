SOURCES = primes.c
PROGRAM = primes
DESTROOT = ../../../
CC65ROOT = ../../cc65Integration/
CC65_TARGET=kim1
CC      = ~/6502/cc65/bin/cl65
O2HEX   = ~/6502/cc65/bin/srec_cat

CFLAGS  = -v -L $(CC65ROOT)lib -g -T -Oi -l $(DESTROOT)lst/$(PROGRAM).lst --cpu 65c02 -Ln $(DESTROOT)sym/$(PROGRAM).sym -I $(CC65ROOT)include  -t $(CC65_TARGET) \
          --create-dep $(<:.c=.d) -O --asm-include-dir $(CC65ROOT)asminc --cfg-path $(CC65ROOT)cfg

LDFLAGS = -v -g -L $(CC65ROOT)lib -Oi -t $(CC65_TARGET) --cpu 65c02 -Ln $(DESTROOT)sym/$(PROGRAM).sym -m $(DESTROOT)map/$(PROGRAM).map -Wl --dbgfile,$(DESTROOT)sym/$(PROGRAM).dbg \
          --asm-include-dir $(CC65ROOT)asminc --cfg-path $(CC65ROOT)cfg

HEXFLAGS = -binary -offset 0x0200  -Address_Length 2

########################################

.SUFFIXES:
.PHONY: all clean
all: $(PROGRAM)

ifneq ($(MAKECMDGOALS),clean)
-include $(SOURCES:.c=.d)
endif

%.o:%.c
	$(CC) -c $(CFLAGS) -o $@ $<

$(PROGRAM): $(SOURCES:.c=.o)
	$(CC) $(LDFLAGS) -o ../../../bin/$@.raw $<

%.hex:%.raw
$(PROGRAM): $(SOURCES:.o=.raw)
	$(O2HEX) ../../../bin/$@.raw $(HEXFLAGS) -o $@.hex -Intel


clean:
	$(RM) $(SOURCES:.c=.o) $(SOURCES:.c=.d) ../../../bin/$(PROGRAM).raw ../../../map/$(PROGRAM).map ../../../lst/$(PROGRAM).lst ../../../sym/$(PROGRAM).sym
