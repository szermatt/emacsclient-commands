# Makefile setup
SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

# Configurable variables
DESTDIR ?= /usr/local/
BINDIR ?= bin/
OUTDIR ?= $(root)out

DEV_GO_FLAGS ?=
OPT_GO_FLAGS ?= -ldflags="-s -w"

# Commands created by this project
commands := $(basename $(notdir $(wildcard $(root)cmd/*.go)))

