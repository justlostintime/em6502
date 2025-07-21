SOURCES = hello.c
PROGRAM = hello
DESTROOT = ../../../
CC65ROOT = ../../cc65Integration/
CC65_TARGET=kim
CC      = cl65
CFLAGS  = -v -L $(CC65ROOT)lib -g -T -Oi -l $(DESTROOT)lst/$(PROGRAM).lst --cpu 65c02 -Ln $(DESTROOT)sym/$(PROGRAM).sym -I $(CC65ROOT)include  -t $(CC65_TARGET) \
          --create-dep $(<:.c=.d) -O --asm-include-dir $(CC65ROOT)asminc --cfg-path $(CC65ROOT)cfg

LDFLAGS = -v -g -L $(CC65ROOT)lib -Oi -t $(CC65_TARGET) --cpu 65c02 -Ln $(DESTROOT)sym/$(PROGRAM).sym -m $(DESTROOT)map/$(PROGRAM).map -Wl --dbgfile,$(DESTROOT)sym/$(PROGRAM).dbg \
          --asm-include-dir $(CC65ROOT)asminc --cfg-path $(CC65ROOT)cfg

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
	$(CC) $(LDFLAGS) -o ../../../bin/$@.raw $^

clean:
	$(RM) $(SOURCES:.c=.o) $(SOURCES:.c=.d) ../../../bin/$(PROGRAM).raw ../../../map/$(PROGRAM).map ../../../lst/$(PROGRAM).lst ../../../sym/$(PROGRAM).sym
