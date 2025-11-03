Vurl Specification
------------------

A Vurl is a possibly versioned URI. Another way to look at this is a Vurl is
either a normal URI (e.g. `https://example.org/index.html`) or a URI with a
resolution chain. The resolution chain will typically go from a base URI
(hereafter called the *intentional URI*) to some resource (e.g. a file). The
chain maybe comprise of zero or more _segments_ where a _segment_ is a step in
the resolution chain consisting of a pair: the next URI and a content identifier
(cid) for the body of the resolution step.

An unresolved URI is a Vurl.

```ocaml
# let example_org_html = Vurl.of_string_exn "https://example.com/index.html";;
val example_org_html : Vurl.t = <abstr>
```

A URI resolved to a file is also a Vurl.

```ocaml
# let example_org_html_file = Vurl.file example_org_html;;
Exception: Failure "No resolvers registered".
```

Resolution only makes sense in the context of a resolver. By default in the
OCaml implementation there are none.

```ocaml
# let vurl, file =
  Eio_main.run @@ fun env ->
  Vurl_eio.with_default ~net:(Eio.Stdenv.net env) (Eio.Stdenv.cwd env)
  @@ fun () -> Vurl.file example_org_html;;


val vurl : Vurl.t = <abstr>
val file : Vurl.Resource.File.t =
  {Vurl.Resource.File.path = "./_data/index.html"}
```

And we can inspect the vurl to see the resolutions.

```ocaml
# Vurl.to_string vurl;;
- : string =
"{\"intentional_uri\":\"https://example.com/index.html\",\"segments\":[{\"uri\":\"file:./_data/index.html\",\"cid\":\"bag5qgera5kh2y7df7nmjwdktkyhveupxj6pjwjbupdolnm7kpg26gzcjzdmq\"},{\"uri\":\"https://example.com/index.html\",\"cid\":\"bag5qgera5kh2y7df7nmjwdktkyhveupxj6pjwjbupdolnm7kpg26gzcjzdmq\"}]}"
```

In this case we went straight from the intentional URI to a downloaded
`index.html` file.

## Format

Using the example, a vurl is either a plain URI or if there have been some
resolution steps, it is a JSON object. The following is a JSON schema for the
format.

<!-- $MDX file=./schema.json -->
```json
{
	"$schema": "https://json-schema.org/draft/2020-12/schema",
	"title": "Vurl with segments",
	"type": "object",
	"properties": {
		"intentional_uri": {
			"type": ["string"],
			"description": "The original intentional URI."
		},
		"segments": {
			"type":"array",
			"description": "Resolution results for an intentional URI.",
			"items": {
				"type": "object",
                "properties": {
                    "uri": {
                        "type": ["string"],
                        "description": "The URI for this resolution step"
                    },
                    "cid": {
                        "type": ["string"],
                        "description": "The content identifier of the blob of data"
                    }
                }
			},
			"minItems": 1
		}
	},
	"required": [
		"intentional_uri",
		"segments"
	]
}
```
