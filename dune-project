(lang dune 3.7)

(name olirvu)

(generate_opam_files true)

(source
 (github ArulselvanMadhavan/olirvu))

(authors "Arulselvan Madhavan")

(maintainers "arulselvan1234@gmail.com")

(license MIT)

(package
 (name olirvu)
 (synopsis "Computational Geometry Algorithms")
 (description "A longer description")
 (depends
  (ocaml (>= 4.14.1))
  dune
  (bonsai (>= 0.15.1))
  (brr (>= 0.0.4))
  (conduit-lwt (>= 6.0.0))
  (cohttp-lwt-unix (>= 5.0.0))
  (integers_stubs_js (>= 1.0))
  (atdgen (>= 2.11.0))
  (ppx_make (>= 0.3.2)))
 (tags
  (topics geometry algorithms visualization ocaml vega)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
