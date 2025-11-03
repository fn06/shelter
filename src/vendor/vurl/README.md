## Vurl

Vurls are an attempt to add versioning to URI resolution. For example, what should happen when we request `https://doi.org/10.1109/SASOW.2012.14`?

<!-- $MDX non-deterministic=command -->
```ocaml
# Eio_main.run @@ fun env ->
  Vurl_eio.with_default ~net:env#net env#cwd @@ fun () ->
  let vurl = Vurl.of_uri "https://doi.org/10.1109/SASOW.2012.14" in
  let vurl, file = Vurl.file vurl in
  Vurl.pp Format.std_formatter vurl;;

{
  "intentional_uri": "https://doi.org/10.1109/SASOW.2012.14",
  "segments": [
    {
      "uri": "file:./_data/document-6498375",
      "cid": "bag5qgeraipjyvov4axsmb4pktfhmleqi4oc2lno5if6f6wjyq37w4ktncvxq"
    },
    {
      "uri": "https://ieeexplore.ieee.org/document/6498375/",
      "cid": "bag5qgeraipjyvov4axsmb4pktfhmleqi4oc2lno5if6f6wjyq37w4ktncvxq"
    },
    {
      "uri": "http://ieeexplore.ieee.org/document/6498375/",
      "cid": "bag5qgerap5iaobunfnlovfzv4jeq2ygp6ltszlrreaskyh3mseky5osh2boq"
    }
  ]
}
- : unit = ()
```
