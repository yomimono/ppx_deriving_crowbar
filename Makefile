build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

test_modules:
	rm -f test/*.cm* test/*.o
	ocamlfind ocamlopt -ppx '`ocamlfind query ppx_deriving`/ppx_deriving _build/src/ppx_deriving_crowbar.cma' -package crowbar -package ppx_deriving -package compiler-libs -dsource -c test/test_modules.ml

test:
	rm -f test/*.cm* test/*.o
	ocamlfind ocamlopt -ppx '`ocamlfind query ppx_deriving`/ppx_deriving _build/src/ppx_deriving_crowbar.cma' -package crowbar -package ppx_deriving -package compiler-libs -dsource -c test/test.ml

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
