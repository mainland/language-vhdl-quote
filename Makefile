#
# Useful variables
#
OS=$(shell uname -s | tr "[:upper:]" "[:lower:]")
ARCH=$(shell uname -m)

#
# Programs and default flags
#
GHC=ghc
GHCFLAGS+=-Wall

RUNGHC=runghc
RUNGHCFLAGS+=-Wall -fno-warn-unused-imports

HAPPY=happy 
HAPPYFLAGS=-agci

ALEX=alex 
ALEXFLAGS=-gi

#
# Support cabal sandbox and stack package databases. We prefer stack.
#
STACK_LTS=lts-9.17/8.0.2
STACK_PKGDB=$(HOME)/.stack/snapshots/$(ARCH)-$(OS)/$(STACK_LTS)/pkgdb

ifneq ($(wildcard $(STACK_PKGDB)/*.conf),)
CABAL_MACROS=$(wildcard .stack-work/dist/$(ARCH)-$(OS)/Cabal-*/build/autogen/cabal_macros.h)

GHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db=$(STACK_PKGDB)

RUNGHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db --ghc-arg=$(STACK_PKGDB)
else ifneq ($(wildcard .cabal-sandbox/*-packages.conf.d),)
CABAL_MACROS=dist/build/autogen/cabal_macros.h

GHCFLAGS += \
	-no-user-package-db \
	-package-db $(wildcard .cabal-sandbox/*-packages.conf.d)

RUNGHCFLAGS += \
	-no-user-package-db \
	-package-db --ghc-arg=$(wildcard .cabal-sandbox/*-packages.conf.d)
endif

#
# Support Cabal's MIN_VERSION
#
#RUNGHCFLAGS += -optP-include -optPdist/build/autogen/cabal_macros.h
#GHCFLAGS += -optP-include -optPdist/build/autogen/cabal_macros.h

GHC_PACKAGES += \
	-hide-all-packages \
	-package array \
	-package base \
	-package binary \
	-package bytestring \
	-package containers \
	-package directory \
	-package exception-mtl \
	-package exception-transformers \
	-package filepath \
	-package haskell-src-meta \
	-package mainland-pretty \
	-package mtl \
	-package primitive \
	-package process \
	-package srcloc \
	-package syb \
	-package symbol \
	-package text \
	-package template-haskell \
	-package test-framework \
	-package test-framework-hunit \
	-package transformers \
	-package vector \
	-package HUnit \
	-package QuickCheck

GHCFLAGS+=$(GHC_PACKAGES)

SOURCE = \
	Language/VHDL/Parser.hs \
	Language/VHDL/Parser/Alex.hs \
	Language/VHDL/Parser/Exceptions.hs \
	Language/VHDL/Parser/Monad.hs \
	Language/VHDL/Parser/Tokens.hs \
	Language/VHDL/Quote.hs \
	Language/VHDL/Syntax.hs

GENERATED_INSTANCES = \
	Language/VHDL/Syntax-instances.hs

GENERATED_PARSER = \
	Language/VHDL/Parser/Lexer.hs \
	Language/VHDL/Parser/Parser.hs

SRC = \
	$(patsubst %,$(SRCDIR)%,$(SOURCE)) \
	$(patsubst %,$(SRCDIR)%,$(GENERATED_INSTANCES)) \
	$(patsubst %,$(SRCDIR)%,$(GENERATED_PARSER))


.PHONY : all
all : $(SRC)

.PHONY : clean
clean :
	rm -rf obj
	rm -f $(patsubst %,$(SRCDIR)%,$(GENERATED_PARSER))
	rm -f $(patsubst %.hs,$(SRCDIR)%.info,$(GENERATED_PARSER))

#
# cabal_macros.h
#
dist/build/autogen/cabal_macros.h :
	cabal build

#
# Lexer and parser generation
#
$(SRCDIR)%.hs : $(SRCDIR)%.y
	$(HAPPY) $(HAPPYFLAGS) -o $@ $<

$(SRCDIR)%.hs : $(SRCDIR)%.x
	$(ALEX) $(ALEXFLAGS) -o $@ $<

#
# Generated instances
#
Language/VHDL/Syntax-instances.hs : bin/gen-instances.hs bin/Derive.hs
	$(RUNGHC) $(RUNGHCFLAGS) -ibin -DONLY_TYPEDEFS $< > $@ || rm -f $@

#
# Tests
#
TESTFLAGS=-I. -DFULL_HASKELL_ANTIQUOTES

unit : tests/unit/Main.hs $(SRC)
	@mkdir -p obj
	$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj $(TESTFLAGS) -o $@

vhdl : tests/vhdl/Main.hs $(SRC)
	@mkdir -p obj
	$(GHC) $(GHCFLAGS) --make $< -odir obj -hidir obj $(TESTFLAGS) -o $@

#
# Print Makefile variables
#
print-%: ; @echo $*=$($*)
