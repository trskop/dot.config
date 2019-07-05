#!/usr/bin/env bash

# Cannot be used due to sourced 'bash_completion'.
#set -euo pipefail

function usage() {
    cat <<EOF
Completion for docker-compose with Command Wrapper style UI.

Usage:

  ${0##*/} [--index=NUM] [--shell=SHELL] [-- [WORD ...]]
  ${0##*/} {--help|-h}
EOF
}

function main() {
    local -a words=()
    local index=0

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
          --index=*)
            index="${arg#*=}"
            ;;

          --shell=*)
            ;;

          --)
            words=("$@")
            break
            ;;

          -h|--help)
            usage
            exit 0
            ;;
        esac
    done

    if ! command -v 'docker-compose' &>/dev/null; then
        # TODO: Consider using default shell completion.
        exit 0
    fi

    if (( index >= ${#words[@]} )); then
        words+=('')
    fi

    function concat() {
        echo "$@"
    }

    local lineBefore
    lineBefore="$(concat "${words[@]:0:${index}+1}")"

    local COMP_LINE; COMP_LINE="$(concat "${words[@]}")"
    local COMP_POINT=$((${#lineBefore} + 1))
    local COMP_WORDBREAKS="'\"><=;|&(: "
    local -a COMP_WORDS=("${words[@]}")
    local COMP_CWORD="${index}"
    local -a COMPREPLY

    # Debugging:
    : index="${index}" 'words=(' "${words[@]}" ')'

    function complete() {
        :
    }

    function compgen() {
        command compgen "$@"
    }

    # shellcheck source=/dev/null
    source /usr/share/bash-completion/bash_completion

    # Yarn completion script taken from
    # <https://github.com/dsifford/yarn-completion>
    #
    # wget -P ~/.config/command-wrapper/exec/ https://raw.githubusercontent.com/dsifford/yarn-completion/master/yarn-completion.bash

    # shellcheck source=/dev/null
    source "$(dirname "$0")/yarn-completion.bash"

    _yarn

    for reply in "${COMPREPLY[@]}"; do
        echo "${reply% }"
    done
}

main "$@"
