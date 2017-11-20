build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

test:
	rm -f test/*.cm* test/*.o
	ocamlfind ocamlopt -ppx '`ocamlfind query ppx_deriving`/ppx_deriving _build/src/ppx_deriving_crowbar.cma' -package crowbar -package ppx_deriving -package compiler-libs -dsource -c test/test.ml

parsetree-source:
	rm -f test/*.cm* test/*.o
	ocamlfind ocamlopt -ppx '`ocamlfind query ppx_import`/ppx_import' -ppx '`ocamlfind query ppx_deriving`/ppx_deriving _build/src/ppx_deriving_crowbar.cma' -package crowbar -package ppx_deriving -package compiler-libs -dsource -c test/parsetree.ml

parsetree:
	ocamlfind ocamlopt -ppx '`ocamlfind query ppx_import`/ppx_import' -ppx '`ocamlfind query ppx_deriving`/ppx_deriving _build/src/ppx_deriving_crowbar.cma' -package crowbar -package ppx_deriving.runtime -package compiler-libs -package ocaml-migrate-parsetree -linkall -linkpkg test/asttypes.ml test/parsetree.ml -o test/$@

clean:
	ocamlbuild -clean

.PHONY: build test doc clean

VERSION      := $$(opam query --version)
NAME_VERSION := $$(opam query --name-version)
ARCHIVE      := $$(opam query --archive)

release:
	git tag -a v$(VERSION) -m "Version $(VERSION)."
	git push origin v$(VERSION)
	opam publish prepare $(NAME_VERSION) $(ARCHIVE)
	opam publish submit $(NAME_VERSION)
	rm -rf $(NAME_VERSION)

.PHONY: release tests
