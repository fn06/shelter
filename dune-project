(lang dune 3.17)
(using menhir 3.0)

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
   ;vendored zfs
   capnp-rpc-net
   capnp-rpc-unix
   index
   cid
   ppx_repr
   irmin-git
   (yojson (< "3.0.0"))
   (morbig (= "0.11.0"))
   ppx_blob
   cmdliner
  )
 (tags
  ("shell")))

