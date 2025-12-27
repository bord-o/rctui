(lang dune 3.20)

(name rctui)

(pin
 (url "git+https://github.com/tmattio/mosaic.git")
 (package
  (name toffee)))

(pin
 (url "../mosaic")
 (package
  (name mosaic))
 (package
  (name matrix)))

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name rctui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml mosaic eio_main ppx_deriving)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
