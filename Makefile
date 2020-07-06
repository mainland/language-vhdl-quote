#
# Useful variables
#
OS=$(shell uname -s | tr "[:upper:]" "[:lower:]")
ARCH=$(shell uname -m)

#
# Programs and default flags
#
GHC=stack ghc --
GHCFLAGS+=-O2 -Wall

RUNGHC=stack runghc --
RUNGHCFLAGS+=-Wall -fno-warn-unused-imports

HAPPY=stack exec happy --
HAPPYFLAGS=-agci

ALEX=stack exec alex --
ALEXFLAGS=-gi

#
# Use stack package databases.
#
STACK_PKGDB=$(shell stack path --snapshot-pkg-db)
STACK_LOCAL_PKGDB=$(shell stack path --local-pkg-db)

GHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db=$(STACK_PKGDB) \
	-package-db=$(STACK_LOCAL_PKGDB)

RUNGHCFLAGS += \
	-clear-package-db \
	-global-package-db \
	-package-db --ghc-arg=$(STACK_PKGDB)

#
# GHC flags
#
GHCPACKAGES += \
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

GHCFLAGS+=$(GHCPACKAGES)

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
	rm -f unit vhdl

#
# cabal_macros.h
#
dist/build/autogen/cabal_macros.h :
	cabal build

#
# Generated instances
#
Language/VHDL/Syntax-instances.hs : bin/gen-instances.hs bin/Derive.hs
	$(RUNGHC) $(RUNGHCFLAGS) -ibin -DONLY_TYPEDEFS $< > $@ || rm -f $@

#
# Lexer and parser generation
#
$(SRCDIR)%.hs : $(SRCDIR)%.y
	$(HAPPY) $(HAPPYFLAGS) -o $@ $<

$(SRCDIR)%.hs : $(SRCDIR)%.x
	$(ALEX) $(ALEXFLAGS) -o $@ $<

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
