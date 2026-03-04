(lang dune 2.7)
; 2.7 instead of 3.7 so dune does not generate confusing .pp.ml for codegraph
; and syncweb_indexer

(name efuns)
(using menhir 2.0)

(generate_opam_files)

(source (github aryx/fork-efuns))
(homepage "https://github.com/aryx/fork-efuns")
(maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
(authors "Yoann Padioleau <yoann.padioleau@gmail.com>")

(package (name efuns)
  (synopsis "Emacs clone in OCaml")
  (description "\
Efuns is an Emacs-like text editor written in OCaml with a GTK/Cairo GUI
and semantic code highlighting via semgrep parsers.

For more information see https://github.com/aryx/fork-efuns
"
  )

  (maintainers "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (authors "Yoann Padioleau <yoann.padioleau@gmail.com>")
  (license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

  (depends
    (dune (>= 3.7))
    ; efuns's own deps
    lablgtk
    cairo2-gtk
    cairo2-pango
    logs
    ; External deps of vendored submodules (semgrep-libs, semgrep-pfff-langs).
    ; These are needed because the submodules are built from source as part of
    ; the dune workspace, so their opam deps are not resolved automatically.
    ; coupling: semgrep-libs/dune-project, semgrep-pfff-langs/dune-project
    ; -- from semgrep-libs (mainly commons) --
    ANSITerminal
    alcotest
    alcotest-lwt
    atdgen
    bos
    (cmdliner (and (>= "1.3.0") (< "2.0.0")))
    digestif
    fmt
    fpath
    git
    lwt
    lwt_ppx
    memtrace
    ocolor
    ocamlgraph
    pcre
    pcre2
    ppx_deriving
    ppx_deriving_yojson
    ppx_hash
    ppx_inline_test
    ppx_sexp_conv
    ppxlib
    re
    semver
    sexplib
    testo
    timedesc
    trace
    (opentelemetry (< "0.11"))
    (opentelemetry-client-ocurl (< "0.11"))
    ambient-context-lwt
    uri
    uuidm
    yojson
    ; -- from semgrep-pfff-langs --
    ; coupling: menhir version must match what semgrep-pfff-langs parsers
    ;  were tested with — newer menhir can generate different conflict reports
    ;  and slightly different parser code (e.g., tuple shapes in actions).
    (menhir (= "20230608"))
    parmap
    tsort
    visitors
    yaml
  )
)

(package (name ppx_interactive)
  (synopsis "PPX deriver for interactive editor actions")
  (description "Registers OCaml functions as interactive editor commands.")
)
