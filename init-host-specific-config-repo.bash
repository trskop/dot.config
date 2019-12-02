#!/usr/bin/env bash

# shellcheck shell=bash

# This script is intentionally very simple Bash script.  It should be
# idempontent, so that it can be harmlessly reexecuted.

# Library for writing CommandWrapper subcommands in Bash.
#
# Copyright (c) 2019, Peter Trsko
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#
#     * Neither the name of Peter Trsko nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

set -euo pipefail

# Default location of host-specific configuration repository.
declare -r repo="${HOME}/.local/src/localhost"

# Usage:
#
#   printHelp
function printHelp() {
    local -r commandName="${0##*/}"

    cat <<EOF
Initialise host-specific configuration repository.

Usage:

  ${commandName} [--help|-h]

Options:

  --help, -h
      Print short help message and exit.

Repository location:

  ${repo}

This script is idempontent and it is safe to rerun it even if host-specific
configuration repository was already  initialised.
EOF
}

# Usage:
#
#   warning FORMAT [ARGUMENT [...]]
function warning() {
    local -r format="$1"; shift

    # shellcheck disable=SC2059
    printf "Warning: ${format}\n" "$@" 1>&2
}

# Usage:
#
#   error FORMAT [ARGUMENT [...]]
function error() {
    local -r format="$1"; shift

    # shellcheck disable=SC2059
    printf "Error: ${format}\n" "$@" 1>&2
}

# Usage:
#
#   die EXIT_CODE FORMAT [ARGUMENT [...]]
function die() {
    local -r -i exitCode="$1"; shift
    local -r format="$1"; shift

    error "${format}" "$@"
    exit "${exitCode}"
}

# Usage:
#
#   mkGitignore FILE
function mkGitignore() {
    local -r output="$1"; shift

    cat > "${output}" <<'EOF'
/dot.config
/this
EOF
}

# Usage:
#
#   mkReadme HOSTNAME FILE
function mkReadme() {
    local -r host="$1"; shift
    local -r output="$1"; shift

    cat > "${output}" <<EOF
# Private Host-specific Configuration

Version-controlled configuration files and documentation specific to current
host (machine).  Generic and publicly available configuration is in
\`~/.config\`, while private and host-specific stuff is kept here.


## ${host}

*   [\`${host}\`](./${host}/dot.config) -- Configuration files referenced from
    \`~/.config\`.  Don't use this directory directly when referencing
    configuration files.  Instead use \`./dot.config\` symbolic link.  That way
    configuration in \`~/.config\` will work on all machines that have
    repository such as this one.

*   [\`${host}\`](./${host}/notes) -- Hardware information, installation notes,
    etc.
EOF
}

# Usage:
#
#   main [--help|-h]
function main() {
    local prefix="${repo}"

    local arg
    while (( $# )); do
        arg="$1"; shift
        case "${arg}" in
            --help|-h)
                printHelp
                exit 0
                ;;

            -*)
                die 1 "'%s': Unknown option." "${arg}"
                ;;

            *)
                die 1 "'%s': Too many arguments." "${arg}"
                ;;
        esac
    done

    local host=''
    host="$(hostname --fqdn || true)"
    if [[ -z "${host}" ]]; then
        die 2 "Unable to detect hostname of this machine."
        exit 1
    fi

    if [[ -e "${prefix}/.git" ]]; then
        warning \
            "'%s': Git repository already initialised; not reinitialising." \
            "${prefix}"
    else
        if [[ -e "${prefix}" ]]; then
            git -C "${prefix}" init
        else
            git init "${prefix}"
        fi

        # It is hard to modify first commit, best to have it empty.
        git -C "${prefix}" -m 'Initial commit' --allow-empty
    fi

    local -a directories=(
        "${prefix}/${host}"
        "${prefix}/${host}/dot.config"
        "${prefix}/${host}/notes"
    )
    for dir in "${directories[@]}"; do
        if [[ -d "${dir}" ]]; then
            warning \
                "'%s': Directory already exists, skipping its creation." \
                "${dir}"
        else
            mkdir -p "${dir}"
        fi
    done

    if [[ -e "${prefix}/this" ]]; then
        warning "'%s': File already exists, skipping symlink creation." \
            "${prefix}/this"
    else
    (
        set -euo pipefail

        cd "${prefix}"
        ln -s "${host}" this
    )
    fi

    local dotConfig="${prefix}/dot.config"
    if [[ -e "${dotConfig}" ]]; then
        warning "'%s': File already exists, skipping symlink creation." \
            "${dotConfig}"
    else
    (
        set -euo pipefail

        cd "${prefix}"
        ln -s "this/${dotConfig##*/}" "${dotConfig##*/}"
    )
    fi

    local -r gitignore="${prefix}/.gitignore"
    if [[ -e "${gitignore}" ]]; then
        warning "'%s': File already exists, skipping its creation." \
            "${gitignore}"
    else
        mkGitignore "${gitignore}"
        git -C "${prefix}" add "${gitignore}"
    fi

    local -r readme="${prefix}/README.md"
    if [[ -e "${readme}" ]]; then
        warning "'%s': File already exists, skipping its creation." \
            "${readme}"
    else
        mkReadme "${readme}"
        git -C "${readme}" add "${readme}"
    fi
}

main "$@"
