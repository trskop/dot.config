# yx-download

> Wrapper for wget/curl intended for downloading files and keeping track of
> metadata in {{file.ext.download}} file.

- Download a file:

`yx download {{https://example.com/some/file.ext}}`

- Download a file, but specify the name under which it will be stored:

`yx download {{https://example.com/some/file.ext}} --output={{some-file.ext}}`

- Download a file and check its digest in the process:

`yx download --{{sha1|sha256|sha512}}={{checksum}} {{https://example.com/some/file.ext}}`

- Re-download a file using information from metadata file:

`yx download --config={{some/file.ext.download}}`
