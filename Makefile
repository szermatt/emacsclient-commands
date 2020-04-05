root=
include $(root)common.mk

all: $(addprefix $(OUTDIR)/,$(commands))
.PHONY: all

clean:
	rm -fr $(OUTDIR)
.PHONY: clean

install: $(addprefix $(DESTDIR)$(BINDIR),$(commands))
.PHONY: install

$(OUTDIR)/%: cmd/%.go
	go build $(DEV_GO_FLAGS) -o $@ $<

$(DESTDIR)$(BINDIR)%: cmd/%.go
	go build $(OPT_GO_FLAGS) -o $@ $<

