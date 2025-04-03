(lang dune 3.17)

(name shelter)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license ISC)


(package
 (name shelter)
 (synopsis "Shelter from the Storm")
 (description "A shell session shim to help you explore!")
 (depends
   ocaml
   eio_posix
   void
   zfs
   cid
   ppx_repr
  )
 (tags
  ("shell")))

