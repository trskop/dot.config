% YX-PATH(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 27th December 2018


# NAME

`yx-path` -- **TODO**


# USAGE

yx \[GLOBAL\_OPTIONS] path \[\--plain|-p] \[\--type|-t] \[\--] \[EXPRESSION \[...]]

yx \[GLOBAL\_OPTIONS] path {\--help|-h}

yx \[GLOBAL\_OPTIONS] help path


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-p, \--plain
:   Plain output, final Dhall expression must result in one of: `Text`,
    `Natural`, or `Integer`.

-t, \--type
:   Print type of final Dhall expression instead of its value.
    functionality to be enabled.

-h, \--help
:   Print short help message and exit.  Same as `yx help path`.

*EXPRESSION*
:   Dhall expression that has access to `data` record, `Paths` and `Config`
    types.  If *EXPRESSION* is not supplied then identity function is assumed,
    i.e. value of `data` is returned.


# EXIT STATUS

TODO


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-path.dhall`
:   Configuration file defines additional fields of the `data` provided when
    `path` command is executed.  In full it can be accessed using:

    ```
    yx path data.config
    ```

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
        ${XDG_CONFIG_HOME}/yx/yx-path.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-path.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# EXAMPLES

```
peter@machine ~ $ yx path --type data.paths.xdg
{ cacheDir :
    Text
, configDir :
    Text
, dataDir :
    Text
, userDirs :
    { desktop :
        Text
    , documents :
        Text
    , download :
        Text
    , music :
        Text
    , pictures :
        Text
    , publicShare :
        Text
    , templates :
        Text
    , videos :
        Text
    }
}
```

```
peter@machine ~ $ yx path --plain data.paths.xdg.cacheDir
/home/peter/.cache
```


# SEE ALSO

yx-env(1), yx-jmp(1), yx-this(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
