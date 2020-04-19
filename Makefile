root=
include $(root)common.mk

all: $(addprefix $(OUTDIR)/,$(commands))
.PHONY: all

clean:
	rm -fr $(OUTDIR)
.PHONY: clean

install: $(addprefix $(DESTDIR)$(BINDIR),$(commands) epipe)
.PHONY: install

test: all
	go test
.PHONY: test

$(OUTDIR)/%: $(root)cmd/%.go $(wildcard $(root)*.go)
	go build $(DEV_GO_FLAGS) -o $@ $<

$(DESTDIR)$(BINDIR)%: $(root)cmd/%.go $(wildcard $(root)*.go)
	go build $(OPT_GO_FLAGS) -o $@ $<

$(DESTDIR)$(BINDIR)epipe: $(DESTDIR)$(BINDIR)ebuf
	cp $< $@

