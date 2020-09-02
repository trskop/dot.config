% DOT-CONFIG(7) User's Dot Files | User's Dot Files
% Peter Trsko
% 2nd September 2020

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


# SEE ALSO

dot.bashrc(7),
dot.gitconfig(7),
yx-this(1)


# BUGS

<https://github.com/trskop/dot.config/issues>
