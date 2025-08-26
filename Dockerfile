# Build efuns (and semgrep and codemap) with OCaml 4.14.2 via OPAM on Ubuntu.
# Similar to codegraph Dockerfile

FROM ubuntu:22.04
# alt: 24.04

# Setup a basic C dev environment
RUN apt-get update # needed otherwise can't find any package
RUN apt-get install -y build-essential autoconf automake pkgconf git wget curl

# Setup OPAM and OCaml
RUN apt-get install -y opam
# Initialize opam (disable sandboxing due to Docker)
RUN opam init --disable-sandboxing -y
ARG OCAML_VERSION=4.14.2
RUN opam switch create ${OCAML_VERSION} -v

# Add efuns external libs (which are also external libs of codemap)
# alt: use opam-depext
RUN apt-get install -y libcairo2-dev libgtk2.0-dev


# Install semgrep libs (and its many dependencies)
WORKDIR /semgrep
RUN git clone --recurse-submodules https://github.com/aryx/semgrep-libs /semgrep
#coupling: https://github.com/aryx/semgrep-libs/blob/master/Dockerfile
# external dependencies of semgrep-libs itself
# alt: make install-deps-UBUNTU-for-semgrep-core
RUN apt-get install -y pkg-config libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev
RUN ./configure
RUN eval $(opam env) && make
RUN eval $(opam env) && make install-semgrep-libs
#TODO: can't because then can't find -ltree-sitter
# RUN rm -rf /semgrep

# Install codemap libs (visualization, commons2_) for codegraph
WORKDIR /codemap
# alt: add codemap as a submodule in codegraph source
RUN git clone https://github.com/aryx/codemap /codemap
RUN ./configure
RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && make install
RUN rm -rf /codemap

# Back to efuns
WORKDIR /src

# Install other dependencies
COPY efuns.opam configure ./
RUN ./configure

# Now let's build from source
COPY . .

RUN eval $(opam env) && make
RUN eval $(opam env) && make all
RUN eval $(opam env) && make install

# Test
RUN eval $(opam env) && efuns --help && efuns_client --help
# TODO run more tests, make test!
