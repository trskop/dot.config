% YX(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 10th March 2019

# NAME

`yx` -- Command Wrapper toolset intended for personal productivity tools.


# USAGE

yx \[GLOBAL\_OPTIONS] SUBCOMMAND \[\--] \[SUBCOMMAND\_ARGUMENTS]

yx \[GLOBAL\_OPTIONS] help \[SUBCOMMAND]

yx \[GLOBAL\_OPTIONS] config \[SUBCOMMAND] \[CONFIG\_OPTIONS]

yx \[GLOBAL\_OPTIONS] completion \[COMPLETION\_OPTIONS]

yx {\--help|-h}


# DESCRIPTION

The name comes from "Y repeat X".  Basic idea is to create a structure around
those ad-hoc tools, and scripts, that we as advanced UNIX/Linux users create
quite frequently.

See also *DESCRIPTION* of `command-wrapper(1)`, since `yx` is just a thin
wrapper around it.  Actually it's just a symlink to `command-wrapper`
executable.


# GLOBAL OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page
section *GLOBAL OPTIONS*.


# SUBCOMMANDS

All Command Wrapper subcommands are available as well.  This includes internal
(`help`, `config`, `completion`) subcommands as well as external ones.
Following is the list of Command Wrapper's external commands (at the time of
writing):

* *cd* -- Start a new subshell / Tmux window / terminal emulator in a selected
  directory.  See `command-wrapper-cd(1)` for more details.

* *exec* -- Execute predefined command with a user specified environment.  See
  `command-wrapper-exec(1)` for more details.

* *skel* -- Generate subcommand skeleton for specific Command Wrapper
  environment, i.e. toolset.  See `command-wrapper-skel(1)` for more details.

Please consult `command-wrapper(1)` documentation for more details.

Yx toolset comes with number of its own external subcommands:

* *env* -- `yx-env(1)`

* *jmp* -- `yx-jmp(1)`

* *new* -- `yx-new(1)`

* *path* -- `yx-path(1)`

* *this* -- `yx-this(1)`


# EXIT STATUS

For documentation of *EXIT STATUS* codes see `command-wrapper(1)` manual page
section *EXIT STATUS*.


# FILES AND DIRECTORIES

For documentation of *FILES AND DIRECTORIES* that are being used by `yx`
toolset, please, see also `command-wrapper(1)` documentation section *FILES AND
DIRECTORIES*.  Entries listed below are mostly just specialisation of what is
defined in `command-wrapper(1)`.

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/default.dhall`
:   Top-level Command Wrapper configuration file for `yx` toolset.

    See `command-wrapper(1)` documentation section *FILES AND DIRECTORIES* for
    more deatils.

`${HOME}/.local/lib/yx/`
:   Directory where `yx` toolset subcommands are installed as:

    ```
    ${HOME}/.local/lib/yx/yx-${subcommand}
    ```

    See `command-wrapper(1)` documentation section *FILES AND DIRECTORIES* for
    more deatils.

`${HOME}/bin/yx`, `${HOME}/.local/bin/yx`
:   These are the common places where `yx` toolset "executable" is installed.
    In reality this is just a symbolic link to
    `${HOME}/.local/lib/command-wrapper/command-wrapper`, and the path where it
    is located doesn't matter that much, but it has to be in user's `$PATH`.

    See `command-wrapper(1)` documentation section *FILES AND DIRECTORIES* for
    more deatils about where `command-wrapper` executable is installed, and why.


# ENVIRONMENT VARIABLES

For documentation of *ENVIRONMENT VARIABLES* see `command-wrapper(1)` manual
page section *ENVIRONMENT VARIABLES*.


# SEE ALSO

command-wrapper(1), yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-this(1)
command-wrapper-cd(1), command-wrapper-exec(1), command-wrapper-skel(1),
command-wrapper-subcommand-protocol(7)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# SUBCOMMAND PROTOCOL

Command Wrapper, therefore, yx toolset as well, follows a specific calling
convention for external subcommands.  Subcommands are required to follow a
specific protocol when invoked.  This protocol is described in
`command-wrapper-subcommand-protocol(7)` manual page.


# BUGS

* [Yx specific issues](https://github.com/trskop/dot.config/issues)

* [Command Wrapper issues](https://github.com/trskop/command-wrapper/issues)
