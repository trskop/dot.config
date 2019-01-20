% YX-NEW(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 20th January 2019


# NAME

`yx-new` -- Simple package/project templating application.


# USAGE

yx \[GLOBAL\_OPTIONS] new *TEMPLATE\_NAME* \[*DIRECTORY*]

yx \[GLOBAL\_OPTIONS] new {\--list|\--ls|-l}

yx \[GLOBAL\_OPTIONS] new {\--help|-h}

yx \[GLOBAL\_OPTIONS] help new


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--list, \--ls, -l
:   List available templates.

*TEMPLATE\_NAME*
:   Name of a template to use when creating package/project.

*DIRECTORY*
:   Package/project directory to be created using specific template referenced
    by *TEMPLATE\_NAME*.

\--help, -h
:   Print short help message and exit.  Same as: `yx help new`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-new.dhall`
:   **TODO**

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
        ${XDG_CONFIG_HOME}/yx/yx-new.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-new.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# EXAMPLES

**TODO**


# SEE ALSO

yx-env(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
