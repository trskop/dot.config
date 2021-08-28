# TLDR Client Configuration

Client for [tldr-pages](https://tldr.sh/) that supports custom pages.


## Directory Structure

```
${XDG_CONFIG_HOME:-${HOME}/.config}/tldr/
│   -- TLDR client configuration file library used by `config.dhall`.
├── Config/
│   └── *.dhall
├── NonEmpty/
│   └── *.dhall
│
│   -- TLDR client configuration file.
├── config.dhall
│
│   -- TLDR pages that are distributed with `dot.config`.
├── pages.en_GB/
│   └── common/
│       └── *.md
│
│   -- This file.
└── README.md
```

## Configuration

See [`config.dhall`](./config.dhall).


## Reading Material

* `tldr(1)` manual page
* [github.com/trskop/tldr-client#readme](https://github.com/trskop/tldr-client#readme)
