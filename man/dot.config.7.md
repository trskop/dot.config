% DOT-CONFIG(7) User's Dot Files | User's Dot Files
% Peter Trsko
% 17th July 2021

# NAME

dot-config - Documentation of user's dot files.


# DESCRIPTION

User's dot files stored and shared via [dot.config Git repository
](https://github.com/trskop/dot.config); installation and updates are handled
mostly by:

```Bash
yx this
```

For more information see `yx-this(1)` or:

```Bash
yx man this
```


# HOW IT WORKS

TBD


# DIRECTORIES AND FILES

`${XDG_CONFIG_HOME:-${HOME}/.config}/`
:   Configuration directory mandated by XDG Base Directory Specification which
    is used for managed user's dot files.

    See also `file-hierarchy(7)` section HOME DIRECTORY for more information.

`${XDG_CONFIG_HOME:-${HOME}/.config}/dot.config.init.hs`
:   Shake build for:

    * installing configuration files (dot files),

    * generating configuration files,

    * installing Command Wrapper,

    * installing Genbashrc,

    * installing Nix,

    * installing FZF,

    * installing fonts,

    * etc.

    See also `file-hierarchy(7)` section HOME DIRECTORY for more information.

`${XDG_DATA_HOME:-${HOME}/.local/share}/man`
:   User's manual pages, including this one.

    See also `file-hierarchy(7)` section HOME DIRECTORY for more information.

`${HOME}/bin` and `${HOME}/.local/bin`
:   Executables available via user's search path environment variable
    (`$PATH`).

    At the moment both paths are used, but the plan is to transition to use
    `${HOME}/.local/bin` fully.

    See also `file-hierarchy(7)` section HOME DIRECTORY for more information.


# SEE ALSO

dot.bashrc(7),
dot.gitconfig(7),
file-hierarchy(7),
yx-this(1)


# BUGS

<https://github.com/trskop/dot.config/issues>
