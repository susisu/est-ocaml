# config
SRCDIR = src
TEMPDIR = _temp
PREFIX = /usr/local
BINDIR = $(PREFIX)/bin
TARGET = est

# build tools
MENHIR = menhir --table
OCB_FLAGS = -use-ocamlfind \
	-use-menhir -menhir "$(MENHIR)" \
	-I src \
	-pkg core -pkg menhirLib -pkg re2 \
	-tag "ppx(ppx-jane -as-ppx)" \
	-tag thread \
	-tag bin_annot \
	-tag short_paths \
	-cflags "-w A-4-9-33-40-41-42-43-34-44" \
	-cflags -strict-sequence
OCB = ocamlbuild $(OCB_FLAGS)

# --------

.PHONY: all clean sanity \
	update-msgs compare-msgs msgs \
	byte native debug \
	build install uninstall


all: build;

clean:
	$(OCB) -clean
	rm -rf $(TEMPDIR)


# parser error messages
update-msgs:
	mkdir -p $(TEMPDIR)
	$(MENHIR) src/parser.mly --update-errors src/parser.messages \
		| sed 's/[ \t]*$$//' \
		> $(TEMPDIR)/parser.messages.new
	cp src/parser.messages $(TEMPDIR)/parser.messages.bak
	cp $(TEMPDIR)/parser.messages.new src/parser.messages

compare-msgs:
	mkdir -p $(TEMPDIR)
	$(MENHIR) src/parser.mly --list-errors > $(TEMPDIR)/parser.messages.raw
	$(MENHIR) src/parser.mly \
		--compare-errors $(TEMPDIR)/parser.messages.raw \
		--compare-errors src/parser.messages

msgs: compare-msgs
	$(MENHIR) src/parser.mly --compile-errors src/parser.messages > $(SRCDIR)/parser_messages.ml


# build targets
byte: msgs
	$(OCB) src/main.byte

native: msgs
	$(OCB) src/main.native

debug: msgs
	$(OCB) -tag debug src/main.byte


# build & install
build: native
	mv main.native $(TARGET)

install: $(TARGET)
	mkdir -p $(BINDIR)
	install $(TARGET) $(BINDIR)

uninstall:
	rm -rf $(BINDIR)/$(TARGET)
