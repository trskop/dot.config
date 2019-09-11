% YX-DOWNLOAD(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 11th September 2019


# NAME

`yx-download` -- Wrapper for `curl(1)`/`wget(1)` with the sole purpose of
downloading files.


# USAGE

yx \[GLOBAL\_OPTIONS] download \[*OPTIONS*]
\[\--output=*FILE*|\--output *FILE*|-o *FILE*] *URL*

yx \[GLOBAL\_OPTIONS] download {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] download


# DESCRIPTION

Wrapper for `curl(1)` or `wget(1)` with the sole purpose of downloading
files.  Not only it does download the file it also creates a `.download` file,
which is a simple sh-like config containing various metadata including original
URL from which the file was downloaded.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

\--config=*FILE*, \--config *FILE*, -c *FILE*
:   Download file based on configuration *FILE*. It has to contain at least one
    entry "URL=<url>".

\--output=*FILE*, \--output *FILE*, -o *FILE*
:   Use *FILE* as output file name. If output file name is not specified, then it
    is derived from URL.

\--no-checksum
:   When downloading file based on specified URL then don't compute checksums.
    In case of download based on configuration file, with checksums specified,
    don't check them after successful download.

\--checksum (Enabled by default.)
:   When downloading file based on specified URL then compute checksums.
    In case of download based on configuration file, with checksums specified,
    check them after successful download.

\--check=*FILE*
:   Same as specifying `--checksum', `--no-download' and `--config=`*FILE*
    together.

\--sha1=*CHECKSUM*, \--sha256=*CHECKSUM*, \--sha512=*CHECKSUM*, \--md5=*CHECKSUM*
:   Check if downloaded file matches CHECKSUM when downloaded. These options
    are ignored when downloading file based on configuration file where these
    can be entered.

\--http-proxy=\[http://]*HOST*\[:*PORT*]
:   Use specified HTTP proxy server by exporting http_proxy environment
    variable for wget/curl. Warning: this script doesn't check if HOST and PORT
    are syntactically valid values.

\--download, -d (Enabled by default.)
:   Download file specified by URL or using `.download' file.

\--no-download, -n
:   Don't download, but create `.download' file or if that is provided then
    just verify checksum(s).

\--help, -h
:   Print short help message and exit.  Same as: `yx help download`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.

3
:   Output file already exists.  This applies to both, the file being
    downloaded as well as metadata file, which has `.download` extension.


# FILES

**TODO**


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

**TODO**


# SEE ALSO

yx-apt(1), yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-remarkable(1),
yx-this(1), yx-xpdf(1), yx(1), command-wrapper(1)

curl(1), wget(1)


# BUGS

<https://github.com/trskop/dot.config/issues>
