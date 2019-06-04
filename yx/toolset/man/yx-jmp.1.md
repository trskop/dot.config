% YX-JMP(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 12th May 2019


# NAME

`yx-jmp` -- Let user select a relevant file to edit based on various sorces.


# USAGE

yx \[GLOBAL\_OPTIONS] jmp \[\--auto|\--tmux|-t|\--kitty|-k]
\[\--root-dir=*DIR*|\--root-dir *DIR*] \[\--syntax=*SYNTAX*|\--syntax *SYNTAX*]

yx \[GLOBAL\_OPTIONS] jmp {\--git-status|-g}

yx \[GLOBAL\_OPTIONS] jmp {\--git-commit|-G} \[*COMMIT*]

yx \[GLOBAL\_OPTIONS] jmp \--git-grep *GIT\_GREP\_ARGUMENTS*

yx \[GLOBAL\_OPTIONS] jmp \[\--file={FILE|-}|\--file {*FILE*|-}|-f {FILE|-}]
\[\--root-dir=*DIR*|\--root-dir *DIR*] \[\--syntax=*SYNTAX*|\--syntax *SYNTAX*]

yx \[GLOBAL\_OPTIONS] jmp {\--help|-h}

yx \[GLOBAL\_OPTIONS] help jmp


# DESCRIPTION

By default it looks for GHC error messages in Tmux scrollback buffer, and
allows user to select one of the files where those errors were encountered for
editing.  At the moment it supports only GHC error format, however it shouldn't
be hard to add other formats as well.

With `--git-status` or `--git-commit` option it uses Git repository as a source
instead.  See documentation of individual options for details.

Reason for supporting Tmux only is that it was really easy to access its
scrollback buffer as text.  If a terminal emulator provides similar
functionality then this script can be adapted to support it.  Some terminals
support things like URL matching.  In some cases it should be possible to use
that as well.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--auto
:   Use heuristic to decide if running in Tmux or Kitty.  This is the default
    mode, when no other mode-changing option is specified.

\--tmux, **-t**
:   Look for files to open in Tmux scrollback buffer.

\--kitty, **-k**
:   Look for files to open in Kitty terminal emulator scrollback buffer.

\--git-status, **-g**
:   Use `git status` command as a source instead of scrollback buffer.  This is
    Useful if you are working on set of files that weren't yet commited.

\--git-commit [*COMMIT*], **-G** [*COMMIT*]
:   Use list of files changed as part of a *COMMIT* as a source instead of Tmux
    scrollback buffer.  Defaults to 'HEAD' if *COMMIT* is not specified.
    Especially useful if you like to have a set of WIP (work in progres)
    commits.

\--git-grep *GIT\_GREP\_ARGUMENTS*
:   Same as:

    ```
    git grep GIT_GREP_ARGUMENTS | yx jmp -f - --syntax=grep-with-line-numbers
    ```

\--file={*FILE*|-}, **\--file** {*FILE*|-}, **-f** {*FILE*|-}
:   Read list of files from *FILE* or `stdin`.

\--root-dir=*DIR*, **\--root-dir** *DIR*
:   When executing editor use this directory as a parent dir for the file that
    is being edited.

\--syntax=*SYNTAX*, **\--syntax** *SYNTAX*
:   Select *SYNTAX* of input from which filenames will be parsed.  Possible
    values for *SYNTAX* are:

    *   *plain* (just list of files)
    *   *ghc* (Haskell compiler)
    *   *psc* (PureScript compiler)
    *   *grep* (output of grep command)
    *   *grep-with-line-numbers* (output of grep command when invoked with
        `{-n|--line-number}` option; `git grep` output has this format by
        default)

    This optinon is ignored when one of `--git-*` options was specified.

\--\[no-]neovim-remote
:   Allow/disable Neovim remote control functionality when `NVIM_LISTEN_ADDRESS`
    environment variable is set.  When enabled 'yx jmp' will try to open file
    in remote Neovim instead of starting a new editor process.

\--lines=\[-]*NUM*, **\--lines** \[-]*NUM*, **-n** \[-]*NUM*
:   Parse input upto *NUM* lines; with the leading '-' it counts the lines from
    the end, i.e. parses all lines except the last *NUM* lines.

\--help, -h
:   Print short help message and exit.  Same as: `yx help jmp`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-jmp.dhall`
:   Configuration file that allows user to specify what editor to use, and what
    menu tool will be used for selecting a file.

    Type signature of configuration expression is:

    ```
    { editor
        : ∀(file : Text)     -- File to be edited.
        → ∀(line : Natural)  -- Move cursor to this line.
        → { command : Text
          , arguments : List Text
          }

    , neovimRemote
        : ∀(file : Text)     -- File to be edited.
        → ∀(line : Natural)  -- Move cursor to this line.
        → { command : Text
          , arguments : List Text
          }

    , menu : { command : Text, arguments : List Text }
    }
    ```

    See also *EXAMPLES* section for an example of `.../yx/yx-jmp.dhall` config
    file.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
    information on how Command Wrapper figures out where to look for this
    configuration file.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/yx/yx-jmp.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-jmp.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

`TMUX`, **`TMUX_PANE`**
:   Allows `yx jmp` to determine if it's running inside Tmux.

`KITTY_WINDOW_ID`, **`TERM`**
:   Allows `yx jmp` to determine if it's running inside Kitty terminal emulator.

`VIM`, **`VIMRUNTIME`**
:   Allows `yx jmp` to determine if it's running inside Vim/Neovim terminal.
    Even if Vim/Neovim is running inside Tmux or Kitty we are unable to access
    scrollback buffer when running inside Vim/Neovim terminal.
    <https://vi.stackexchange.com/questions/6178/detect-neovim-terminal-from-bash-in-bashrc>

`NVIM_LISTEN_ADDRESS`
:   If set it allows `yx jmp` to open files in remote Neovim instead of
    starting new editory.  See also `--[no-]neovim-remote` option.


# EXAMPLES

Create initial configuration (Bash):

```
cat > ${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-jmp.dhall <<EOF
{ editor =
      λ(file : Text)
    → λ(line : Natural)
    → { command =
          -- Use value from $VISUAL environment variable. Default to "nvim" if
          -- it's not defined.
          env:VISUAL as Text ? "nvim"

      , arguments = [file, "+${Natural/show line}"]
      }

, neovimRemote =
      λ(file : Text)
    → λ(line : Natural)
    → { command = "nvr"
      -- Open files using ':vsplit'.
      , arguments = ["--nostart", "-O", file]
      }

, menu =
    { command = "fzf"
    , arguments =
        [ "--reverse"  -- Show menu below cursor.
        , "--tac"      -- Show list of files in reverse
                       -- order (last on command line
                       -- first in the list).
        , "--no-sort"  -- Don't sort the list of files.
        ]
    }
}
EOF
```

Using `ls` or `find` as a source:

```
yx jmp --file=<(ls *.json) --syntax=plain
```


# SEE ALSO

yx-env(1), yx-new(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* [neovim-remote](https://github.com/mhinz/neovim-remote)
* [Neovim restore --remote (+clientserver) #1750
  ](https://github.com/neovim/neovim/issues/1750)


# BUGS

<https://github.com/trskop/dot.config/issues>
