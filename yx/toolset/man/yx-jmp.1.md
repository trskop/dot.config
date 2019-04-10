% YX-JMP(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 10th April 2019


# NAME

`yx-jmp` -- Let user select a relevant file to edit based on various sorces.


# USAGE

yx \[GLOBAL\_OPTIONS] jmp \[\--tmux|-t]

yx \[GLOBAL\_OPTIONS] jmp {\--git-status|-g}

yx \[GLOBAL\_OPTIONS] jmp {\--git-commit|-G} \[*COMMIT*]

yx \[GLOBAL\_OPTIONS] jmp {\--file={*FILE*|-}|\--file {*FILE*|-}|-f {*FILE*|-}}

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

\--tmux, -t
:   (DEFAULT) Look for GHC error messages in Tmux scrollback buffer.  This is
    the default mode, when no options are specified.

\--git-status, -g
:   Use `git status` command as a source instead of scrollback buffer.  This is
    Useful if you are working on set of files that weren't yet commited.

\--git-commit [*COMMIT*], -G [*COMMIT*]
:   Use list of files changed as part of a *COMMIT* as a source instead of Tmux
    scrollback buffer.  Defaults to 'HEAD' if *COMMIT* is not specified.
    Especially useful if you like to have a set of WIP (work in progres)
    commits.

\--file={*FILE*|-}, \--file {*FILE*|-}, -f {*FILE*|-}
:   Read list of files from *FILE* or `stdin`.

\--root-dir=*DIR*, \--root-dir *DIR*
:   When executing editor use this directory as a parent dir for the file that
    is being edited.

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


# SEE ALSO

yx-env(1), yx-new(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
