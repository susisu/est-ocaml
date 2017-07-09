TEMPDIR = _temp

MENHIR = menhir --table

OCAMLBUILD_FLAGS = -use-ocamlfind \
	-use-menhir -menhir "$(MENHIR)" \
	-I src \
	-pkg core -pkg menhirlib -pkg re2 \
	-tag "ppx(ppx-jane -as-ppx)" \
	-tag thread \
	-tag bin_annot \
	-tag short_paths \
	-cflags "-w A-4-9-33-40-41-42-43-34-44" \
	-cflags -strict-sequence

OCAMLBUILD = ocamlbuild $(OCAMLBUILD_FLAGS)


.PHONY: default clean sanity \
	update-parser-messages compare-parser-messages parser-messages \
	byte native debug default


default: native;

clean:
	$(OCAMLBUILD) -clean
	rm -rf $(TEMPDIR)

sanity:
	mkdir -p $(TEMPDIR)
	which ocamlbuild ocamlfind menhir
	ocamlfind query core menhirlib re2


update-parser-messages: sanity
	$(MENHIR) src/parser.mly --update-errors src/parser.messages \
		| sed 's/[ \t]*$$//' \
		> $(TEMPDIR)/parser.messages.new
	cp src/parser.messages $(TEMPDIR)/parser.messages.bak
	cp $(TEMPDIR)/parser.messages.new src/parser.messages

compare-parser-messages: sanity
	$(MENHIR) src/parser.mly --list-errors > $(TEMPDIR)/parser.messages.raw
	$(MENHIR) src/parser.mly --compare-errors $(TEMPDIR)/parser.messages.raw \
		--compare-errors src/parser.messages

parser-messages: sanity compare-parser-messages
	$(MENHIR) src/parser.mly --compile-errors src/parser.messages > src/parser_messages.ml


byte: sanity parser-messages
	$(OCAMLBUILD) src/main.byte

native: sanity parser-messages
	$(OCAMLBUILD) src/main.native

debug: sanity parser-messages
	$(OCAMLBUILD) -tag debug src/main.byte
