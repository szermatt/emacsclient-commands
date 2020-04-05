SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Configurable variables
DESTDIR ?= ~/
BINDIR ?= bin/
OUTDIR ?= out

OPT_GO_FLAGS ?= -ldflags="-s -w"

commands := $(basename $(notdir $(wildcard cmd/*.go)))

all: $(addprefix $(OUTDIR)/,$(commands))
.PHONY: all

clean:
	rm -fr $(OUTDIR)
.PHONY: clean

install: $(addprefix $(DESTDIR)$(BINDIR),$(commands))
.PHONY: install

$(OUTDIR)/%: cmd/%.go
	go build -o $@ $<

$(DESTDIR)$(BINDIR)%: cmd/%.go
	go build $(OPT_GO_FLAGS) -o $@ $<

