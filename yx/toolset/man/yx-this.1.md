% YX-THIS(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 27th December 2018


# NAME

`yx-this` -- **TODO**


# USAGE

yx \[GLOBAL\_OPTIONS] this \[OPTIONS]

yx \[GLOBAL\_OPTIONS] this {\--help|-h}

yx \[GLOBAL\_OPTIONS] help this


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-h, \--help
:   Print short help message and exit.  Same as `yx help this`.


# EXIT STATUS

TODO


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-this.dhall`
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
        ${XDG_CONFIG_HOME}/yx/yx-this.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-this.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# EXAMPLES

**TODO**


# SEE ALSO

yx-env(1), yx-path(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
