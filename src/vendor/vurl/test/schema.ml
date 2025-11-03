let cid = "zb2rhe5P4gXftAwvA4eXQ5HJwsER2owDyS9sKaQRRVQPn93bA"
let uri2 = "file:///tmp/my/file.txt"

let () =
  let uri_with_file =
    Fmt.str
      {| { "intentional_uri": "https://example.org", "segments": [ { "uri": "%s", "cid": "%s"} ] } |}
      uri2 cid
  in
  Vurl.of_string_exn uri_with_file
  |> Vurl.to_string
  |> Format.pp_print_string Format.std_formatter
