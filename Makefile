LIBS = "yojson lwt lwt.unix"

OCB_FLAGS = -use-ocamlfind -pkgs $(LIBS) -tag bin_annot
OCB = ocamlbuild $(OCB_FLAGS)
MAIN="lamduct"

all: 	native

clean:
	$(OCB) -clean
	rm -f $(MAIN)
	rm -f version.ml

native: version.ml
	$(OCB) $(MAIN).native
	cp $(MAIN).native $(MAIN)

profile: version.ml
	$(OCB) -tag profile $(MAIN).native
	cp $(MAIN).native $(MAIN)

debug: version.ml
	$(OCB) -tag debug $(MAIN).native
	cp $(MAIN).native $(MAIN)

version.ml:
	@echo "let number = \"0.2\"" > version.ml
	@echo "let name   = \"Thames\"" >> version.ml
	@echo "let git    = \"$(shell git rev-parse --short HEAD)\"" >> version.ml

.PHONY: all clean native profile debug
