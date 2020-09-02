% DOT-GITCONFIG(7) User's gitconfig | User's Dot Files
% Peter Trsko
% 2nd September 2020

# NAME

dot-gitconfig - Documentation of user's gitconfig.


# DESCRIPTION

User's Git configuration shared via the "dot.config" repository.

Git supports hierarchy of configuration files which is described in
[git-config documentation](https://git-scm.com/docs/git-config#FILES).

Syntax of Git configuration file is described in [git-config: Configuration
File](https://git-scm.com/docs/git-config#_configuration_file).

Includes/imports are documented in [git-config: Configuration File: Includes
](https://git-scm.com/docs/git-config#_includes).


# DIRECTORIES AND FILES

`${XDG_CONFIG_HOME:-${HOME}/.config}/git/config`
:   User's Git configuration file (gitconfig). It's syntax and meaning is
    documented in `git-config(1)`, but one can also use
    <https://git-scm.com/docs/git-config> as well as:

    ```Bash
    git help config
    ```

`${XDG_CONFIG_HOME:-${HOME}/.config}/git/local.gitconfig`
:   Git configuration file included by

    ```Bash
    ${XDG_CONFIG_HOME:-${HOME}/.config}/git/config
    ```

    This one is, however, not committed into the "dot.config" repository. It is
    intended to be used for machine-specific or experimental configuration. In
    other words, options specified in this file are not shared across machines.

`${XDG_CONFIG_HOME:-${HOME}/.config}/git/*.gitconfig`
:   Other Git configuration files included by:

    ```Bash
    ${XDG_CONFIG_HOME:-${HOME}/.config}/git/config
    ```

    These are committed into the "dot.config" repository


# SEE ALSO

command-wrapper(1),
command-wrapper-cd(1),
command-wrapper-config(1),
git-config(1),
yx-apt(1),
yx-this(1),
yx-xpdf(1)

*   [dot.config Git repository](https://github.com/trskop/dot.config)
*   [git-config: Files](https://git-scm.com/docs/git-config#FILES)
*   [git-config: Configuration File](https://git-scm.com/docs/git-config#_configuration_file)
*   [git-config: Configuration File: Includes](https://git-scm.com/docs/git-config#_includes)

# BUGS

<https://github.com/trskop/dot.config/issues>
