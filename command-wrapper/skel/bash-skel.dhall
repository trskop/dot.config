  λ(libFile : Text)
→ λ(subcommandName : Text)
→ ''
#!/usr/bin/env bash

set -e

source '${libFile}'

function printHelp() {
    local -r command="''${COMMAND_WRAPPER_NAME} ''${COMMAND_WRAPPER_SUBCOMMAND}"
    local -r commandHelp="''${COMMAND_WRAPPER_NAME} help ''${COMMAND_WRAPPER_SUBCOMMAND}"

    cat <<EOF
TODO: Hereby I promise to describe this subcommand one day.

Usage:

  ''${command}
  ''${command} {--help|-h}
  ''${commandHelp}

Options:

  -h, --help
    Print short help message and exit.  Same as: ''${commandHelp}
EOF
}

# Read configuration file in Dhall format and declare Bash variables based on
# what's in it.  Subcommand configuration files are required to be in Dhall
# format, however, if you are more comfortable with JSON or YAML then there
# are tools to convert Dhall into them:
#
# * 'dhall-to-json'
# * 'dhall-to-yaml'
#
# Best approach in such case would be to create a temporary file with
# JSON/YAML content that is then used as a configuration file instead.
#
# Note that Subcommands aren't required to use configuration files.  If you
# don't need to use it then delete this function and relevant code in the
# 'main' function.
function declareCfg() {
    local -r configFile="$1"; shift
    local -r name="$1"; shift

    dhall-to-bash --declare "''${name}" < "''${configFile}"
}

function main() {
    # This function makes sure that this subcommand was executed using
    # expected calling convention.  If it wasn't it will terminate this
    # script with appropriate error message.  It is hard to guarantee
    # anything in case of it being executed in any other way.  Mentioned
    # calling convention is described in a dedicated manual page:
    #
    #   command-wrapper-subcommand-protocol(7)
    dieIfExecutedOutsideOfCommandWrapperEnvironment

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "''${arg}" in
            # Supporting '--help' is required by the
            # command-wrapper-subcommand-protocol(7).
            -h|--help)
                printHelp
                exit 0
                ;;
            # TODO: Define additional options here.
            -*)
                die 1 "'%s': Unknown option." "''${arg}"
                ;;
            # TODO: Define arguments here.
            *)
                die 1 "'%s': Too many arguments." "''${arg}"
                ;;
        esac
    done

    # Subcommands aren't required to use configuration files.  If you don't
    # need it then just delete or commend-out config file related code.
    #
    # When subcommand is using configuration file and that doesn't exist it
    # is allowed to do one of the following:
    #
    # 1. Use hardcoded defaults.
    # 2. Fail with error message indicating that the configuration file is
    #    missing.
    #
    # In case of first option subcommand is allowed to generate default
    # configuration file.
    #
    # See command-wrapper-subcommand-protocol(7) manual page for more
    # information.
    if [[ ! -e "''${COMMAND_WRAPPER_CONFIG}" ]]; then
        notice "'%s': Generating default configuration file." \
            "''${COMMAND_WRAPPER_CONFIG}"

        cat > "''${COMMAND_WRAPPER_CONFIG}" <<< '{=}'

        # Alternative would be to use 'dhall format', however, it doesn't
        # preserve comments at the moment.
        #dhall format > "''${COMMAND_WRAPPER_CONFIG}" <<< '{=}'

        # In case of choosing option 2.
        #die 1 "'%s': Configuration file is missing." \
        #    "''${COMMAND_WRAPPER_CONFIG}"
    fi

    info "'%s': Loading configuration file." "''${COMMAND_WRAPPER_CONFIG}"
    eval "$(declareCfg "''${COMMAND_WRAPPER_CONFIG}" 'config')"
    info "'%s': Configuration file loaded." "''${COMMAND_WRAPPER_CONFIG}"

    # TODO: Implement me!  Here is the place where the real functionality
    # should be.
}

main "$@"
''
