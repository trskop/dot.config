# Habit

Command Wrapper toolset `habit` provides tooling used to help with my daily
work-related tasks.  Most of the tools (subcommands) and their configuration is
not published.

Core idea is to use Direnv or Direnv+Nix to configure individual projects and
use `habit` as a namespace for project-specific actions.  This is done by
exporting `COMMAND_WRAPPER_LOCAL_CONFIG_DIR` containing path to project-specic
`habit` configuration.  Having user-level configuration allows people to use
their own aliases, subcommands, and other settings.

For more information see:

*   [Command Wrapper Documentation: Direnv](https://github.com/trskop/command-wrapper#direnv)
*   [Command Wrapper Documentation: Direnv and Nix](https://github.com/trskop/command-wrapper#direnv-and-nix)
*   [`command-wrapper(1)` manual page](https://github.com/trskop/command-wrapper/blob/master/command-wrapper/man/command-wrapper.1.md)


## `pgpass.d/`

Files in `pgpass.d` contain credentials to non-production databases intended
for development use.  Project-specific Direnv can concatenate them into a file
that is then used by project-specific `habit` configuration.
