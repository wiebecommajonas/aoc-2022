FLAGS = -O2
HFLAGS = $(FLAGS) -XNoImplicitPrelude
DONE1 = $(wildcard Days/Day[1-9].hs)
DONE2 = $(wildcard Days/Day1[0-9].hs)
DONE3 = $(wildcard Days/Day2[0-5].hs)

all: $(sort $(DONE1:%.hs=%)) $(sort $(DONE2:%.hs=%)) $(sort $(DONE3:%.hs=%))
	@rm -f -- Aoc.hi Aoc.o

%: %.hs
	stack ghc -- $(HFLAGS) -o $@ $^
	@rm -f -- $@.hi $@.o

clean: 
	@rm -f -- Days/Day[1-9] Days/Day1[0-9] Days/Day2[0-5]

.PHONY: all clean
