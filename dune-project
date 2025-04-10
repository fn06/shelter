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
   (ocaml (< "5.3.0~~")) ; for the irmin pin only
   (ctypes (< "0.23.0")) ; for a const ptr mismatch with zfs from https://github.com/yallop/ocaml-ctypes/pull/782
   eio_posix
   zfs
   cid
   ppx_repr
   irmin-git
   morbig
   ppx_blob
   cmdliner
  )
 (tags
  ("shell")))

