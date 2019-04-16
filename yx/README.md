# YX (Y repeat X)

Command Wrapper toolset `yx` is supposed to provide general purpose
personalised tooling.

Following host-specific files need to be supplied:

```
${config}/yx/this/system-info.dhall
```

Following host-specific files can be optionally supplied:

```
${config}/yx/default/aliases-local.dhall
${config}/yx/path/path-local.dhall
${config}/yx/this/backup-local.dhall
${config}/yx/this/nix-packages-local.dhall
${config}/yx/this/packages-local.dhall
${config}/yx/this/purge-packages-local.dhall
```
