###############################################################################
# Prelude
###############################################################################

###############################################################################
# Main targets
###############################################################################

#TODO: also test.bc
default:
	bash -c "dune build _build/install/default/bin/{efuns,efuns_client}"

all:
	dune build
clean:
	dune clean
test:
	dune runtest -f
install:
	dune install

.PHONY: all clean install test

build-docker:
	docker build -t "efuns" .
build-docker-ocaml5:
	docker build -t "efuns" --build-arg OCAML_VERSION=5.2.1 .
build-docker-light:
	docker build -f Dockerfile.light -t "efuns-light" .

###############################################################################
# Developer targets
###############################################################################

# see https://github.com/semgrep/semgrep
check:
	osemgrep --experimental --config semgrep.jsonnet --strict --error

# -filter semgrep
visual:
	codemap -screen_size 3 -efuns_client efuns_client -emacs_client /dev/null .
sync:
	@echo go to docs/literate/
index:
	$(MAKE) clean
	$(MAKE)
	codegraph_build -lang cmt .
