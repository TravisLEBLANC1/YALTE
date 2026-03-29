(lang dune 3.16)
(using menhir 3.0)
(name FaustML)

(generate_opam_files false)

(source
 (github TravisLEBLANC1/YALTE))

(authors "Travis LEBLANC")

(maintainers "Travis LEBLANC")

(license LICENSE)

(package
 (name FaustML)
 (synopsis "Simple lambda term evaluator")
 (depends ocaml dune))