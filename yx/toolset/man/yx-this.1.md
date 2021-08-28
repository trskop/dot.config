% YX-THIS(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 28th August 2021


# NAME

`yx-this` - Simplify system management and common system operations.


# USAGE

yx \[GLOBAL\_OPTIONS] this

yx \[GLOBAL\_OPTIONS] this {\--update|-U} \[\--system|-s] \[\--install|-i] \[\--user|-u]

yx \[GLOBAL\_OPTIONS] this {\--edit|-e}

yx \[GLOBAL\_OPTIONS] this {\--default-config}

yx \[GLOBAL\_OPTIONS] this {\--help|-h}

yx \[GLOBAL\_OPTIONS] help this


# DESCRIPTION

This subcommand started as a huge set of Bash scripts that simplified system
management.  Most important feature was a configuration that contained a list
of packages that were installed.  These packages were annotated with a lot of
information, and especially reason why they were installed.

**TODO: At the moment only bare minimum of functionality was translated into this
subcommand.**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--update, -U
:   Update system and user's tools.  What is updated can be restricted by
    specifying additional options:

    \--system, -s
    :   Update/upgrade system.

    \--user, -u
    :   Update only user tools, environment and configuration.

    \--install, -i
    :   Install packages based on what is present in configuration file.

    We can consisder the default value to be as if we specified all options
    `--system --user --install`, however, that can be overriden in the
    configuration file.  See *FILES AND DIRECTORIES* section for more
    information on configuration file.

\--edit, -e
:   Edit configuration file using user's preferred editor.
    **TODO: Currently not implemented**

\--default-config
:   Generate default configuration file, if it doesn't exist, otherwise it
    refuses to do so.  See *FIlES AND DIRECTORIES* section for more details
    about configuration file.

\--help, -h
:   Print short help message and exit.  Same as: `yx help this`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


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

yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
