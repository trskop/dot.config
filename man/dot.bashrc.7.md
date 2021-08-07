% DOT-BASHRC(7) User's Bashrc | User's Dot Files
% Peter Trsko
% 17th July 2021

# NAME

bashrc - Documentation of user's Bashrc setup


# DESCRIPTION

Documentation of User's `bashrc` as generated using `genbashrc`.  For it to
work correctly following files must be present:

`${HOME}/.bashrc`
:   A symbolic link pointing to:

    ```
    ${XDG_CONFIG_HOME:-${HOME}/.config}/bash/dot.bashrc
    ```

`${XDG_CONFIG_HOME:-${HOME}/.config}/bash/dot.bashrc`
:   Real `bashrc` file that calls `genbashrc` command, if available, to
    generate most of what is described here.

    See also `file-hierarchy(7)` section HOME DIRECTORY for more information.


# BASH SETTINGS

`set -b`
:   Notify immediately of job termination instead of waiting for prompt to be
    redrawn.

`set -o vi`
:   Use vi-style line editing interface.

`set bell-style visible`
:   Make the terminal "blink" instead of beeping when bell character is
    printed.

`shopt -s checkwinsize`
:   Check the terminal window size after each command and, if necessary, update
    the values of `LINES` and `COLUMNS`.

`shopt -s histappend`
:   Append to the Bash history file, don't overwrite it.

`HISTCONTROL=ignoredups`
:   Don't put duplicate lines into Bash history.

`HISTSIZE`
:   Set to 100 000 lines.

`HISTFILESIZE`
:   Not set, let it grow as needed.

`HISTIGNORE`
:   Don't add following lines into Bash history:

    * `ls`
    * `pwd`


# PROMPT

Generic format:

```
#[NIX]USER@HOSTNAME[#SCREEN_WINDOW]:DIRECTORY[GIT_PS1][⟦CD_LEVEL⟧][∃] ⊢
```

Standard prompt when nothing special is applied is therefore:

```
#USER@HOSTNAME:DIRECTORY ⊢
```

Reason for it to start with `#` is that it is a comment in Bash if accidentally
copied. Individual fields of the generic syntax are:

`NIX`
:   If in Nix shell (detected using `IN_NIX_SHELL` environment variable) then the
    prompt is prefixed with `⟪nix⟫` as follows

    ```
    #⟪nix⟫user@host:~ ⊢
    ```

    Normally Nix sets its own prompt, but we made sure that the user's prompt
    definition is restored.

`#SCREEN_WINDOW`
:   When inside a `screen` session then prompt will contain the Screen window
    number (value taken from `WINDOW` environment variable):

    ```
    #user@hostname#1:~⟦1⟧ ⊢
    ```

`DIRECTORY`
:   Is just the last path element or `~`:

    ```
    #user@hostname:~ ⊢
    #user@hostname:~ ⊢ cd /tmp
    #user@hostname:tmp ⊢
    ```

`GIT_PS1`
:   Expanded value of `__git_ps1` provided by Git installation itself.

`⟦CD_LEVEL⟧`
:   When Command Wrapper's `cd` subcommand was used to enter a subshell then
    the level of nesting (taken from `CD_LEVEL` environment variable) will be
    shown in the prompt:

    ```
    #user@hostname:~⟦1⟧ ⊢
    ```

`∃`
:   In case of being inside `direnv` or `yx env` environment there will be `∃`
    present in the prompt:

    ```
    #user@hostname:~∃ ⊢
    ```


# KEY BINDINGS

`ALT+c`
:   Runs directory search and selection via `fzf` resulting in:

    ```Bash
    cd DIRECTORY
    ```

    Where `DIRECTORY` is what user selected in `fzf` menu.

`CTRL+f`
:   Calls and expands:

    ```Bash
    yx cd --self-command --shell
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    yx cd --shell DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.

`CTRL+ALT+f`
:   Calls and expands

    ```Bash
    yx cd --bash-command
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    cd DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.

`CTRL+k`
:   Calls and expands:

    ```Bash
    yx cd --self-command
    ```

    Into following command which ends up being executed and stored in Bash
    history:

    ```Bash
    yx cd DIRECTORY
    ```

    Where `DIRECTORY` is what user selected during expansion of the original
    command.

`CTRL+r`
:   Runs Bash history search using `fzf`.

`CTRL+t`
:   Runs `fzf` and pastes the selected file path into the command line.


# ALIASES

To list all aliases just type `alias` without any arguments. Some important
ones are listed here:

`evil`
:   Open root login shell by invoking the following:

    ```bash
    sudo su -
    ```

`rg`
:   Ripgrep doesn't read any configuration file unless `RIPGREP_CONFIG_PATH`
    environment variable is set. This alias sets it to the following before
    calling `rg` binary:

    ```bash
    RIPGREP_CONFIG_PATH="${XDG_CONFIG_HOME:-${HOME}/.config}/.config/ripgrep/ripgreprc"
    ```

`term-title` TITLE
:   Set terminal title. Alias for:

    ```bash
    printf "\033]2;%s\007"
    ```

`this`
:   Alias for:

    ```bash
    yx this
    ```

    For more details see `yx-this(1)` or:

    ```
    yx help [--man] this
    ```

`tmux`
:   Tmux doesn't support XDG Base Directory standard. This alias makes sure
    that correct configuration file is used:

    ```
    ${XDG_CONFIG_HOME:-${HOME}/.config}/.config/tmux/tmux.conf
    ```

    And that `TERM` is set to something `tmux` can handle.

`xpdf`
:   Alias for:

    ```bash
    yx xpdf
    ```

    For more information see `yx-xpdf(1)` or:

    ```
    yx help [--man] xpdf
    ```

`yank`
:   Alias for `yank-cli` since Debian disambiguates its packages.


## APT

Following aliases for `yx apt` are present:

* `apt`
* `apt-cache`
* `apt-get`

For more information see `yx-apt(1)` or:

```
yx help [--man] apt
```


## DHALL

`dhall`
:   Alias for:

    ```
    yx config --dhall
    ```

    For more information see `command-wrapper-config(1)` or:

    ```
    yx help [--man] config
    ```

`dhall-{bash|diff|exec|filter|format|freeze|hash|lint|repl|resolve|text}`
:   Alias for:

    ```
    yx config --dhall-{bash|diff|exec|filter|format|freeze|hash|lint|repl|resolve|text}
    ```

    For more information see `command-wrapper-config(1)` or:

    ```
    yx help [--man] config
    ```


# ENVIRONMENT VARIABLES

## EDITOR, VISUAL

`EDITOR` and `VISUAL` environment variables are set to (in specified order of
precedence):

* `nvim` if Neovim is installed.

* `vim` if Vim is installed.

* Not set if none of the above.

## ORIGINAL\_PS1

This variable is set to the same value as `PS1` to allow resetting `PS1` if it
gets modified. This is used for things like `nix-shell` which likes to override
user defined `PS1`.

## MPLAYER\_HOME

MPlayer doesn't support XDG Base Directory Specification, but it does allow us
to override directory path where it's looking for configuration:

```
MPLAYER_HOME="${XDG_CONFIG_HOME:-${HOME}/.config}/mplayer"
```

See `mplayer(1)` for more information.


# SEE ALSO

command-wrapper(1),
command-wrapper-cd(1),
command-wrapper-config(1),
file-hierarchy(7),
yx-apt(1),
yx-this(1),
yx-xpdf(1)


# BUGS

<https://github.com/trskop/dot.config/issues>
