% YX-REMARKABLE(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 13th July 2019


# NAME

`yx-remarkable` -- Better UI for existing reMarkable tools.


# USAGE

yx \[GLOBAL\_OPTIONS] remarkable sync

yx \[GLOBAL\_OPTIONS] remarkable {ls|tree|mkdir|rmdir} *DIRECTORY*

yx \[GLOBAL\_OPTIONS] remarkable {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] remarkable


# DESCRIPTION

Better UI for existing reMarkable tools, not their reimplementation.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--help, -h
:   Print short help message and exit.  Same as: `yx help remarkable`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-remarkable.dhall`
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
        ${XDG_CONFIG_HOME}/yx/yx-remarkable.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-remarkable.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# EXAMPLES

**TODO**


# SEE ALSO

yx-apt(1), yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-this(1), yx-xpdf(1),
yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)

* [Awesome reMarkable -- A curated list of projects related to the reMarkable
  tablet](https://github.com/reHackable/awesome-reMarkable)


# BUGS

<https://github.com/trskop/dot.config/issues>
