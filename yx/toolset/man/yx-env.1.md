% YX-ENV(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 23nd December 2018


# NAME

`yx-env` -- **TODO**


# USAGE

yx \[GLOBAL\_OPTIONS] env

yx \[GLOBAL\_OPTIONS] env {-i|\--init}

yx \[GLOBAL\_OPTIONS] env {-u DIR|\--dry-run DIR}

yx \[GLOBAL\_OPTIONS] env {-s|\--script}


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-i, \--init
:   Create env config (`.yx-env` by default) in the current directory.

-s, \--script
:   Generate a script that needs to be included in `.bashrc` for `yx env`
    functionality to be enabled.

-u *DIR*, \--dry-run *DIR*
:   Print out what would happen if we cded into *DIR*.


# EXIT STATUS

TODO


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-env.dhall`
:   Configuration file.  **TODO**

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
        ${XDG_CONFIG_HOME}/yx/yx-env.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-env.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

`YX_ENV_STATE`
:   File path to a state file.  Usually it is in the form
    `${XDG_CONFIG_HOME:-${HOME}/.cache}/yx-env/state${RANDOM}.dhall`.

    If `yx env` is hooked into current shell then this variable will always be
    present and cannot be used to check if we are running in a specific
    environment.

    Reason for using state file instead of holding everything in an environment
    variable is that there is a size limit to environment variable size as well
    as an environment block.


# EXAMPLES

**TODO**


# SEE ALSO

command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
