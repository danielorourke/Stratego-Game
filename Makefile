UNITS=board gui state command monte ai
MLS_WITHOUT_MLIS=main
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
MLIS=$(UNITS:=.mli)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) 
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics -pkg str -plugin-tag 'package(bisect_ppx-ocamlbuild)'
PKGS=ounit2,graphics,str

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip stratego.zip *.ml* *.txt _tags Makefile

clean:
	ocamlbuild -clean
	rm -rf search.zip doc.public doc.private _coverage bisect*.coverage

bisect-test:
	BISECT_COVERAGE=YES $(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential


bisect: clean bisect-test
	bisect-ppx-report html

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)