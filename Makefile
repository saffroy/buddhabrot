PROGS = buddhabrot
GHC ?= ghc
GHCFLAGS = -O3 -rtsopts
ifneq ($(PROFILE),)
GHCFLAGS += -prof -auto-all -caf-all -fforce-recomp
endif

buddhabrot: GHCFLAGS += -with-rtsopts=-K32M

all: $(PROGS)

%: %.hs
	$(GHC) $(GHCFLAGS) --make $@

clean:
	$(RM) $(PROGS) *.hi *.o

.PHONY: clean
