#!/usr/bin/env bash

set -e

function declareCfg() {
    local -r configFile="$1"; shift
    local -r name="$1"; shift

    dhall-to-bash --declare "${name}" < "${configFile}"
}

function main() {
    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
            -h|--help)
                printHelp
                exit 0
                ;;
            -*)
                error 1 "'%s': %s" "${arg}" 'Unknown option.'
                ;;
            *)
                error 1 "'%s': %s" "${arg}" 'Too many arguments.'
                ;;
        esac
    done

    if  [[ -n "${COMMAND_WRAPPER_EXE}" ]]; then
        error 1 'Error: COMMAND_WRAPPER_EXE: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -n "${COMMAND_WRAPPER_NAME}" ]]; then
        error 1 'Error: COMMAND_WRAPPER_NAME: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    if  [[ -n "${COMMAND_WRAPPER_CONFIG}" ]]; then
        error 1 'Error: COMMAND_WRAPPER_CONFIG: %s: %s' \
            'Missing environment variable' \
            'This command must be executed inside command-wrapper environment.'
    fi

    eval "$(declareCfg "${COMMAND_WRAPPER_CONFIG}" 'config')"

    # TODO: Implement me!
}

main "$@"
