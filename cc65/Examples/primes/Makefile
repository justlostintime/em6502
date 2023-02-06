SOURCES = primes.c
PROGRAM = primes
CC65_TARGET=kim1
CC      = cl65
CFLAGS  = -g -T -Oi -l ../../../lst/$(PROGRAM).lst --cpu 65c02 -Ln ../../../sym/$(PROGRAM).sym -t $(CC65_TARGET) --create-dep $(<:.c=.d) -O
LDFLAGS = -g -Oi -t $(CC65_TARGET) -Ln ../../../sym/$(PROGRAM).sym -m ../../../map/$(PROGRAM).map -Wl --dbgfile,../../../sym/$(PROGRAM).dbg


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
