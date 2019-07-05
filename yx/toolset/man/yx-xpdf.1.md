% YX-XPDF(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 4th July 2019


# NAME

`yx-xpdf` -- Compatibility wrapper that provides xpdf-like command line
interface for other PDF viewers.


# USAGE

yx \[GLOBAL\_OPTIONS] xpdf \[\--fullscreen]
  \[*PDF_FILE* \[*PAGE_NUMBER*|+*NAMED_DESTINATION*]]

yx \[GLOBAL\_OPTIONS] xpdf {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] xpdf


# DESCRIPTION

Compatibility wrapper that provides xpdf-like command line interface for other
PDF viewers.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--fullscreen, -fullscreen
:   Open PDF viewer in fullscreen mode if it's supported.

\--help, -h, -?, -help
:   Print short help message and exit.  Same as: `yx help apt`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`DISPLAY`
:   If not set then `yx xpdf` will look for textual viewer instead of GUI
    enabled one.

`PAGER`
:   In case of textual viewer that doesn't support pagination `yx xpdf` will
    try to figure out what pager to use.  Setting `PAGER` environment variable
    will tell it to use that one.


# SEE ALSO

yx-apt(1), yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-this(1), yx(1),
command-wrapper(1)

apt(8), apt-get(8), apt-cache(8)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
