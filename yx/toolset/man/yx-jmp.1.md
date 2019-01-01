% YX-JMP(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 31st December 2018


# NAME

`yx-jmp` -- Search Tmux history for GHC error messages, and allow user to open
it in an editor.


# USAGE

yx \[GLOBAL\_OPTIONS] jmp

yx \[GLOBAL\_OPTIONS] jmp {\--help|-h}

yx \[GLOBAL\_OPTIONS] help jmp


# DESCRIPTION

Find GHC error messages in Tmux scrollback buffer and edit selected one in an
editor.  At the moment it supports only GHC error format, however it shouldn't
be hard to add other formats as well.

Reason for supporting Tmux only is that it was really easy to access its
scrollback buffer as text.  If a terminal emulator provides similar
functionality then this script can be adapted to support it.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-h, \--help
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

yx-env(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
