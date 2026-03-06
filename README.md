# Efuns

An Emacs clone written entirely in OCaml, using GTK/Cairo for rendering.

Efuns was written originally by Fabrice Le Fessant
(INRIA Rocquencourt, FRANCE) in 1999.

It was forked by Yoann Padioleau in 2015 who took over its maintenance
and added many features (see `changes.org`, `authors.txt`, and the
screenshot below).

![Efuns with minimap](docs/efuns-with-minimap.jpg)

## Building

### Prerequisites

OCaml 4.14+ (via opam >= 2.1), gcc, git, curl, pkg-config.

On Ubuntu/Debian:
```bash
apt-get install build-essential pkg-config opam curl libpcre3-dev libpcre2-dev libgmp-dev libev-dev libcurl4-gnutls-dev libcairo2-dev libgtk2.0-dev
```

On macOS:
```bash
brew install opam pkg-config cairo gtk+
```

### Quick start

```bash
git clone --recurse-submodules https://github.com/aryx/efuns
cd efuns
./configure     # installs opam deps and sets up tree-sitter (run infrequently)
make            # routine build
make test       # run tests
```

### Docker

A reference build using Ubuntu is provided:

```bash
docker build -t efuns .
```

## Usage

```bash
efuns --help
efuns <file>
```
