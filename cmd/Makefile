root=../
include $(root)common.mk

all: $(addprefix $(OUTDIR)/,$(commands))
.PHONY: all

$(OUTDIR)/%: %.go $(wildcard $(root)*.go)
	go build $(DEV_GO_FLAGS) -o $@ $<

