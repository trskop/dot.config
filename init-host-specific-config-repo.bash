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

function mkGitignore() {
    local output="$1"; shift

    cat > "${output}" <<'EOF'
/dot.config
/this
EOF
}

function main() {
    local prefix="${HOME}/.local/src/localhost"
    local host=''

    if [[ -e "${prefix}/.git" ]]; then
        echo "Warning: '${prefix}': Git repository already initialised; not reinitialising."
    else
        if [[ -e "${prefix}" ]]; then
            git -C "${prefix}" init
        else
            git init "${prefix}"
        fi

        # It is hard to modify first commit, best to have it empty.
        git -C "${prefix}" -m 'Initial commit' --allow-empty
    fi

    host="$(hostname --fqdn)"
    if [[ -n "${host}" ]]; then
        if [[ ! -e "${prefix}/${host}" || ! -d "${prefix}/${host}" ]]; then
            echo "Warning: '${host}': Directory doesn't exist or is not a directory."
            mkdir -p "${prefix}/${host}/dot.config" "${prefix}/${host}/notes"
        fi
        if [[ -e 'this' ]]; then
            echo "Warning: 'this': File already exists, skipping symlink creation."
        else
        (
            set -euo pipefail

            cd "${prefix}"
            ln -s "${host}" this
        )
        fi
    else
        echo "Error: Unable to detect hostname of this machine."
        exit 1
    fi

    local dotConfig="${prefix}/dot.config"
    if [[ -e "${dotConfig}" ]]; then
        echo "Warning: '${dotConfig}': File already exists, skipping symlink creation."
    else
    (
        set -euo pipefail

        cd "${prefix}"
        ln -s "this/${dotConfig##*/}" "${dotConfig##*/}"
    )
    fi

    local -r gitignore="${prefix}/.gitignore"
    if [[ -e "${gitignore}" ]]; then
        echo "Warning: '${gitignore}': File already exists, skipping its creation."
    else
        mkGitignore "${gitignore}"
        git -C "${prefix}" add "${gitignore}"
    fi
}

main "$@"
