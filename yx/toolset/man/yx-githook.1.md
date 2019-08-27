% YX-GITHOOK(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 27th August 2019


# NAME

`yx-githook` -- Wrapper for `githooks(5)`


# USAGE

yx \[GLOBAL\_OPTIONS] githook --hook=HOOK [ARGUMENT ...]

yx \[GLOBAL\_OPTIONS] githook --print-default-config

yx \[GLOBAL\_OPTIONS] githook {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] githook


# DESCRIPTION

Simple script that executes `githooks(5)` based on Dhall configuration file.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--hook=*HOOK*
:   Run *HOOK* with specified *ARGUMENT*s.  What arguments are accepted by which
    hook is documented in `githooks(5)`.

\--print-default-config
:   Print default configuration file that does nothing.

\--help, -h
:   Print short help message and exit.  Same as: `yx help githook`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-githook.dhall`
:   Configuration file used by this subcommand.  See `--print-default-config`.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/yx/yx-githook.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-githook.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# SEE ALSO

yx-apt(1), yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-remarkable(1),
yx-this(1), yx-xpdf(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
