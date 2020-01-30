(lang dune 2.1)
(using menhir 2.0)

(name tiger)
(generate_opam_files true)

(source (github vsiles/tiger))
(authors "Vincent Siles <vincent.siles@ens-lyon.org>")
(maintainers "Vincent Siles <vincent.siles@ens-lyon.org>")

(package
    (name tiger)
    (synopsis "WIP implementation of Andrew Appel Tiger book.")
    (description "WIP implementation of Andrew Appel Tiger book.")
    (depends
        (ocaml (>= 4.08.0))
        (dune (>= 2.0))
        core
        menhir
        ounit2
    )
)
