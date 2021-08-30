% YX-XPDF(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 30th August 2021


# NAME

`yx-xpdf` - Compatibility wrapper that provides xpdf-like command line
interface for other PDF viewers.


# USAGE

yx \[GLOBAL\_OPTIONS] xpdf \[\--fullscreen]
  \[*PDF_FILE* \[*PAGE_NUMBER*|+*NAMED_DESTINATION*]]

yx \[GLOBAL\_OPTIONS] xpdf {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] xpdf


# DESCRIPTION

Compatibility wrapper that provides xpdf-like command line interface for other
PDF viewers. The feature set is highly limited to work with wide range of PDF
viewers.

Currently supported PDF viewers in the order as they are tried:

*   Evince — GNOME Desktop document viewer for multiple document formats
    ([wiki.gnome.org/Apps/Evince](https://wiki.gnome.org/Apps/Evince)).

*   Atril — a fork of Evince for MATE Desktop Environment
    ([github.com/mate-desktop/atril#atril](https://github.com/mate-desktop/atril#atril)).

*   GNU GV — X Window System document viewer for PostScript and PDF documents
    ([gnu.org/software/gv/](https://www.gnu.org/software/gv/))

*   `pdftotext` — We default to this one if `DISPLAY` environment variable is
    not set. See *ENVIRONMENT VARIABLES* section and `pdftotext(1)` manual
    page.


# EXAMPLES

Open a document:

```bash
yx xpdf /path/to/a/paper.pdf
```

Open a document on a specific page:

```bash
yx xpdf /path/to/a/paper.pdf :42
```

Use textual viewer to open a document:

```bash
DISPLAY= yx xpdf /path/to/a/paper.pdf
```


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--fullscreen, -fullscreen
:   Open PDF viewer in fullscreen mode if it's supported.

\--help, -h, -?, -help
:   Print short help message and exit.  Same as: `yx help xpdf`.


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

yx(1), command-wrapper(1), pdftotext(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
