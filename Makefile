PROGS = buddhabrot
GHCFLAGS = -O3 -rtsopts

buddhabrot: GHCFLAGS += -with-rtsopts=-K32M

all: $(PROGS)

%: %.hs
	ghc $(GHCFLAGS) --make $@

clean:
	$(RM) $(PROGS) *.hi *.o

.PHONY: clean
