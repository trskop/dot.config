% YX-JMP(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 30th December 2018


# NAME

`yx-jmp` -- **TODO**


# USAGE

yx \[GLOBAL\_OPTIONS] jmp

yx \[GLOBAL\_OPTIONS] jmp {\--help|-h}

yx \[GLOBAL\_OPTIONS] help jmp


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-h, \--help
:   Print short help message and exit.  Same as `yx help jmp`.


# EXIT STATUS

TODO


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-jmp.dhall`
:   Configuration file.  **TODO: Configuration file is currently unused.**

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

**TODO**


# SEE ALSO

yx-env(1), yx-path(1), yx-this(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
