% YX-ENV(1) YX Toolset 0.1.0 | YX Toolset
% Peter Trsko
% 31st December 2018


# NAME

`yx-env` -- **TODO**


# USAGE

yx \[GLOBAL\_OPTIONS] env

yx \[GLOBAL\_OPTIONS] env {\--init|-i}

yx \[GLOBAL\_OPTIONS] env {\--dry-run *DIR*|-u *DIR*}

yx \[GLOBAL\_OPTIONS] env {\--allow|-a} *DIR*

yx \[GLOBAL\_OPTIONS] env {\--ignore|-a} *DIR*

yx \[GLOBAL\_OPTIONS] env {\--dump|-p|\--diff|-d} *FILE*

yx \[GLOBAL\_OPTIONS] env {\--script|\-s|\--run-hook=*PID*|\-r *PID*}

yx \[GLOBAL\_OPTIONS] env {\--help|-h}

yx \[GLOBAL\_OPTIONS] help env


# DESCRIPTION

**TODO**


# OPTIONS

For documentation of *GLOBAL_OPTIONS* see `command-wrapper(1)` manual page.

-i, \--init
:   Create env config (`.yx-env` by default) in the current directory.

-u *DIR*, \--dry-run *DIR*, \--dry-run=*DIR*
:   Print out what would happen if we descended into *DIR*.

-a *DIR*, \--allow *DIR*, \--allow=*DIR*
:   Allow specified env config to be used to modify environment.

-g *DIR*, \--ignore *DIR*, \--ignore=*DIR*
:   Ignore specified env config instead of using it to modify environment.

-p *FILE*, \--dump *FILE*, \--dump=*FILE*
:   Dump current environment in the form of Haskell associative list.
    **TODO: Plan is to change the output format to Dhall.**

-d *FILE*, \--diff *FILE*, \--diff=*FILE*
:   Diff current environment against existing dump.  Output is a Dhall
    description of changes that would be performed to transition from the
    environment described in *FILE* in to the current one.

-s, \--script
:   Generate a script that needs to be included in `.bashrc` for `yx env`
    functionality to be enabled.

-r, \--run-hook *PID*, \--run-hook=*PID*
:   Run shell hook, i.e. generate shell script that will modify environment
    if evaluated.  *PID* is the process ID of the shell that is executing it.
    This way we can track nested shells, and properly handle state in those
    cases.

-h, \--help
:   Print short help message and exit.  Same as `yx help env`.


# EXIT STATUS

For documentation of generic *EXIT STATUS* codes see `command-wrapper(1)`
manual page section *EXIT STATUS*.  Any *EXIT STATUS* codes specific to this
subcommand, if any, will be listed below.


# FILES

`${XDG_CONFIG_HOME:-$HOME/.config}/yx/yx-env.dhall`
:   Configuration file.  **TODO**

    Type signature of configuration expression:

    ```

    -- Command 'yx env' will look for these env config files
    -- in directories.  If 'None Text' then '.yx-env' is used.
    { envFileName
        : Optional
            ( ∀(toolset : Text)     -- "yx"
            → ∀(subcommand : Text)  -- "env"
            → Text
            )

    -- Template of env config (name depends on 'envFileName',
    -- by default its '.yx-env'.  See documentation for
    -- '--init' option for more details.
    , initEnv : Text

    , installScript
        -- "yx_env" ("${toolsetName}_${subcommandName}")
        -- Usually used as a base name for functions.
        : ∀(name : Text)

        -- Toolset command ("yx"), hopefully full path
        -- in the future.
        → ∀(toolset : Text)

        -- Scripts used to hook into individual shells.
        → { bash : Text
          , fish : Text
          , tcsh : Text
          , zsh : Text
          }
    }
    ```

    See also `XDG_CONFIG_HOME` in *ENVIRONMENT VARIABLES* section for more
    information on how Command Wrapper figures out where to look for this
    configuration file.

`.yx-env`
:   Env(ironment) config that is loaded when a directory with it present is
    entered.  For the file to be loaded it has to be allowed first, see
    `--allow` option for more details.  File can also be explicitly ignored.
    In such case it is skipped when searching for a suitable `.yx-env` file,
    see `--ignore` option for that.

    Name of this file can be overriden in configuration by specifying
    `envFileName`.

    To create initial env config just run:

    ```
    yx env --init
    ```

    Which will create `.yx-env` file in the current directory.


# ENVIRONMENT VARIABLES

See also `command-wrapper(1)` *ENVIRONMENT VARIABLES* section.  Everything
mentioned there applies to this subcommand as well.

`XDG_CONFIG_HOME`
:   Overrides where this subcommand expects its configuration file.  It follows
    this simple logic:

    * If `XDG_CONFIG_HOME` environment variable is set then the configuration
      file has path:

        ```
        ${XDG_CONFIG_HOME}/yx/yx-env.dhall
        ```

    * If `XDG_CONFIG_HOME` environment variable is not set then default value
      is used instead:

        ```
        ${HOME}/.config/yx/yx-env.dhall
        ```

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on rationale behind this.

`YX_ENV_STATE_DIR`
:   Full path to a directory where state files are stored.  If it's not set
    then it will be populated using the value of `XDG_RUNTIME_DIR`:

    ```
    ${XDG_RUNTIME_DIR}/yx-env
    ```

    To override this default value users can define `YX_ENV_STATE_DIR` them
    selves.  In such case the resolution will be as following:

    ```
    ${YX_ENV_STATE_DIR:-${XDG_RUNTIME_DIR}}/yx-env
    ```

    If `yx env` is hooked into current shell then this variable will always be
    present.  Setting it to a different value won't have immediate effect.  It
    will be used only when new env state file (see `YX_ENV_STATE` vairiable) is
    created.  To force `yx env` to use a different value of `YX_ENV_STATE` it
    has to be set before `yx env` is hooked into current shell.

    See [XDG Base Directory Specification
    ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
    for more information on `XDG_RUNTIME_DIR`.

`YX_ENV_STATE`
:   File path to a state file.  Usually it is in the form
    `${XDG_CONFIG_HOME:-${HOME}/.cache}/yx-env/state${RANDOM}.dhall`.

    If `yx env` is hooked into current shell then this variable will always be
    present and cannot be used to check if we are running in a specific
    environment.

    Reason for using state file instead of holding everything in an environment
    variable is that there is a size limit to environment variable size as well
    as an environment block.  There are other restrictions comming to play as
    well, like maximum command line length, or that we like to pass a lot of
    environment variables to Docker containers.  All of this can cause
    environment variables or the whole environment to be truncated.
    Interestingly Linux allows huge environment variable blocks, see
    `execve(2)` manual page for details, however, some applications and
    commands aren't handling it properly.

    The downside of using state files is that they may pile up, if we don't do
    a proper cleanup, and we have to create a new file every time we change
    something to provide sense of atomicity.  Some of the negative aspects
    can be avoided by using directories like `XDG_RUNTIME_DIR` to store these
    files.

    At the moment state files are in Dhall format.  This may change in the
    future.

`YX_ENV_DIR`
:   Directory path of where currently loaded env config lies.  If there is no
    environment loaded then this variable is not present.

    Scripts can use this variable to detect that they are in a specific
    environment.


# EXAMPLES

**TODO**


# SEE ALSO

yx-jmp(1), yx-path(1), yx-this(1), yx(1), command-wrapper(1)

* [XDG Base Directory Specification
  ](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
* Similar projects:
    - [`direnv`] (https://direnv.net)


# BUGS

<https://github.com/trskop/dot.config/issues>
