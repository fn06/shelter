(lang dune 3.8)
(generate_opam_files true)
(using mdx 0.4)

(name vurl)

(maintainers patrick@sirref.org)

(authors "Anil Madhavapeddy" "Patrick Ferris")

(homepage https://github.com/quantifyearth/vurl)
(bug_reports https://github.com/quantifyearth/vurl/issues)

(package
  (name vurl)
  (synopsis "Version URLs")
  (depends
    uri
    cid
    (capnp-rpc :dev)
    ezjsonm
    (mdx :with-test)))


(package
  (name vurl-eio)
  (synopsis "Version URLs and resolvers using Eio")
  (depends
   vurl
   (eio (>= 1.0))
   cohttp-eio
   git-unix
   http-cookie
   tls-eio
   (capnp-rpc-lwt :dev)
   (capnp-rpc-unix :dev)
   yojson
   (eio_main :with-test)))
