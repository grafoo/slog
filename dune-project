(lang dune 3.13)

(name slog)

(generate_opam_files true)

(source
 (github grafoo/slog))

(authors "Manuel Graf")

(maintainers "Manuel Graf")

(license UNLICENSE)

(documentation https://github.com/grafoo/slog)

(package
 (name slog)
 (synopsis "April 1st RFC")
 (description "Blogging via DNS aka slog")
 (depends ocaml dune)
 (tags
  (dns txt rfc april 1st fools day)))
