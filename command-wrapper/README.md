CommandWrapper configuration, see [github.com/trskop/command-wrapper
](https://github.com/trskop/command-wrapper) for more information.


# Host-specific Configuration

Some host-specific files may be supplied without modifying existing
configuration files.  These configuration files are either ignored by Git, see
[`.gitignore`](../.gitignore) for more information, or are outside of this
repository.  In the later case they may be versioned separately.


## Host-specific Configuration For `cd`

* [`./cd/directories.dhall`](./cd/directories.dhall)
* [`./cd/directories-local.dhall`](./cd/directories-local.dhall)
* `~/.local/src/localhost/dot.config/command-wrapper/cd/directories-local.dhall`


## Host-specific Configuration For `exec`

* [`./exec/commands.dhall`](./exec/commands.dhall)
* [`./exec/commands-local.dhall`](./exec/commands-local.dhall)
* `~/.local/src/localhost/dot.config/command-wrapper/exec/commands-local.dhall`
