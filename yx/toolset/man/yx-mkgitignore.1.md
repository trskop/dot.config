% YX-MKGITIGNORE(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 28th August 2021


# NAME

`yx-mkgitignore` - Generate a gitignore file using selected snippets.


# USAGE

yx \[GLOBAL\_OPTIONS] mkgitignore \[\--] \[*FILE*]

yx \[GLOBAL\_OPTIONS] mkgitignore {\--help|-h}

yx \[GLOBAL\_OPTIONS] help [\--man] mkgitignore


# DESCRIPTION

Generate a `.gitignore` file (see `gitignore(5)`) by composing snippets from
[Github's gitignore templates/snippets repository
](https://github.com/github/gitignore#readme). The way how this is done is by
opening a list of available snippets in an editor so that user can select which
ones should be used.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

*FILE*
:   Write generated gitignore content into this *FILE*. If *FILE* is not provided
    then following value will be used:

    ```
    generated.gitignore
    ```

\--help, -h
:   Print short help message and exit.  Same as: `yx help mkgitignore`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.

3
:   Dependency or a command wasn't found in `$PATH`.

4
:   Unable to create a temporary file.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`VISUAL`
:   If `VISUAL` environment variable is defined then its value is interpreted
    as an editor command. When this variable is not defined or we are on a dumb
    terminal (terminals with very limited capabilities, see [Wikipedia:
    Computer terminal — Dub terminals
    ](https://en.wikipedia.org/wiki/Computer_terminal#Dumb_terminals)) then we
    try the `EDITOR` environment variable.

    When neither `VISUAL` nor `EDITOR` is defined then we use `vi` as the
    default.

`EDITOR`
:   When `VISUAL` environment variable is not defined or if we are on a dumb
    terminal (terminals with very limited capabilities, see [Wikipedia:
    Computer terminal — Dub terminals
    ](https://en.wikipedia.org/wiki/Computer_terminal#Dumb_terminals)) then
    we interpret value of `EDITOR` environment variable as editor command to be
    used.

    When neither `VISUAL` nor `EDITOR` is defined then we use `vi` as the
    default.


# SEE ALSO

yx(1), command-wrapper(1), gitignore(5)

* [Github's collection of .gitignore templates
  ](https://github.com/github/gitignore#readme)
* [Wikipedia: Computer terminal — Dub terminals
  ](https://en.wikipedia.org/wiki/Computer_terminal#Dumb_terminals)
* [Git Documentation](https://git-scm.com/doc)


# BUGS

<https://github.com/trskop/dot.config/issues>
