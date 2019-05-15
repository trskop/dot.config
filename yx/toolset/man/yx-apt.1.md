% YX-APT(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 14th May 2019


# NAME

`yx-apt` -- Inteligent wrapper for *apt* package manager.


# USAGE

yx \[GLOBAL\_OPTIONS] apt \[*APT_OPTIONS*] \[*APT_COMMAND*]

yx \[GLOBAL\_OPTIONS] jmp {\--init-config|--init-packages}

yx \[GLOBAL\_OPTIONS] jmp {\--help|-h}

yx \[GLOBAL\_OPTIONS] help jmp


# DESCRIPTION

Intercept `apt` calls so that they can be done more safely/consistently with
the user expectations.


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

*APT_OPTIONS*
:   See `apt(8)`, `apt-get(8)` and `apt-cache(8)` for more details.

*APT_COMMAND*
:   See `apt(8)`, `apt-get(8)` and `apt-cache(8)` for more details.

\--init-config
:   Create default configuration file if it doesn't exist.  Parent directories
    are created as well.

\--init-packages
:   Create packages file.  A configuration file that contains what packages
    should be installed/removed.

\--help, -h
:   Print short help message and exit.  Same as: `yx help apt`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-apt.dhall`
:   Configuration file that allows user to specify what editor to use, where
    packages config is located, and what apt command to use for individual
    actions.

    Type signature of configuration expression is:

    ```
    { aptCommand :
          ∀ ( action
            : < autoclean | autoremove | build-dep | changelog | clean
              | depends | dist-upgrade | download | edit-sourceshelp
              | full-upgrade | install | list | moo | policy | purge | rdepends
              | remove | search | show | showsrc | source | update | upgrade
              >
            )
        → ∀(arguments : List Text)
        → List Text
    , editor : List Text
    , packagesFile : Text
    }
    ```

    See also *EXAMPLES* section for an example of `.../yx/yx-apt.dhall` config
    file.

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
    information on how Command Wrapper figures out where to look for this
    configuration file.

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/apt/packages.dhall`
:   Default location for packages config where we store list of packages to
    have installed and list of packages to have purged from the system.

    Type signature of configuration expression is:

    ```
    { install : List Text
    , purge : List Text
    }
    ```

    This file should be kept under version control and synced to a remote
    backup.  Its purpose is to be able to help recreate the system if
    necessary.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/yx/yx-apt.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-apt.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.


# BASH CONFIGURATION

```
alias apt='yx apt'
alias apt-cache='yx apt'
alias apt-get='yx apt'
source <(yx completion --script --subcommand=apt --alias=apt --alias=apt-cache --alias=apt-get)
```


# EXAMPLES

To generate default/initial values of configuration files call:

```
yx apt --init-config
yx apt --init-packages
```

Example configuration file:

```
let Action =
      < autoclean | autoremove | build-dep | changelog | clean | depends
      | dist-upgrade | download | edit-sourceshelp | full-upgrade | install
      | list | moo | policy | purge | rdepends | remove | search | show
      | showsrc | source | update | upgrade
      >

let apt = λ(arguments : List Text) → [ "apt" ] # arguments

let sudoApt = λ(arguments : List Text) → [ "sudo" ] # apt arguments

in  { packagesFile =
            (env:XDG_CONFIG_HOME as Text ? "${env:HOME as Text}/.config")
        ++  "/yx/apt/packages.dhall"
    , editor =
        [ env:VISUAL as Text ? env:EDITOR as Text ? "nvim" ]
    , aptCommand =
          λ(action : Action)
        → λ(arguments : List Text)
        → ( merge
            { autoclean = sudoApt
            , autoremove = sudoApt
            , build-dep = sudoApt
            , changelog = apt
            , clean = sudoApt
            , depends = apt
            , dist-upgrade = sudoApt
            , download = apt
            , edit-sourceshelp = sudoApt
            , full-upgrade = sudoApt
            , install = sudoApt
            , list = apt
            , moo = apt
            , policy = apt
            , purge = sudoApt
            , rdepends = apt
            , remove = sudoApt
            , search = apt
            , show = apt
            , showsrc = apt
            , source = apt
            , update = sudoApt
            , upgrade = sudoApt
            }
            action
          )
          arguments
    }
```

Example packages configuration file:

```
{ install =
    [ "curl", "git", "neovim", "socat"
    ] : List Text
, purge =
    [ "nano"
    ] : List Text
}
```


# SEE ALSO

yx-env(1), yx-jmp(1), yx-new(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

apt(8), apt-get(8), apt-cache(8)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)


# BUGS

<https://github.com/trskop/dot.config/issues>
