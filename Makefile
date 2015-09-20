PROGS = buddhabrot
GHC ?= ghc
NPROCS = $(shell getconf _NPROCESSORS_ONLN)
GHCFLAGS = -Wall -O3 -rtsopts -fllvm -threaded
ifneq ($(PROFILE),)
GHCFLAGS += -prof -auto-all -caf-all -fforce-recomp
endif


all: $(PROGS)

buddhabrot: *.hs

%: %.hs
	$(GHC) $(GHCFLAGS) --make -j$(NPROCS) $@

clean:
	$(RM) $(PROGS) *.hi *.o

.PHONY: clean
