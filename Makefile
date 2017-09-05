build:
	cp pkg/META.in pkg/META
	ocaml pkg/build.ml native=true native-dynlink=true

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
