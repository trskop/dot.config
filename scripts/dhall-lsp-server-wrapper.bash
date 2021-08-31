#!/usr/bin/env bash

# Copyright (c) 2019-2021, Peter Trsko
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

declare -r repoUrl="https://github.com/dhall-lang/dhall-haskell"
declare -r dhallVersion='1.40.1'
declare -r dhallLspServerVersion='1.0.16'
declare -r arch='x86_64-linux' # TODO: Support other?
declare -r downloadUrl="${repoUrl}/releases/download/${dhallVersion}/dhall-lsp-server-${dhallLspServerVersion}-${arch}.tar.bz2"
declare -r downloadSha256='88b17e25cba8c3765f8388c880f285d27088ffa4832f96deb99a2cc1ff6bcf9c'

# Usage:
#
#   download DOWNLOAD_DIRECTORY URL SHA256
function download() {
    local -r root="$1"; shift
    local -r url="$1"; shift
    local -r sha256="$1"; shift

    local -r output="${root}/${url##*/}"

    if [[ ! -e "${output}" ]]; then
        mkdir -p "$(dirname "${output}")" || return 1
        curl --silent --location "${url}" > "${output}" || return 1
        sha256sum --quiet --check - <<< "${sha256}  ${output}" || return 1
    fi
    echo "${output}"
}

# Usage:
#
#   download DOWNLOAD_DIRECTORY TARBALL
function unpack() {
    local -r root="$1"; shift
    local -r tarball="$1"; shift

    local -r exe="${root}/bin/dhall-lsp-server"

    if [[ ! -e "${exe}" ]]; then
        tar -C "${root}" -xf "${tarball}" || return 1
        # chmod +x "${exe}"
    fi

    echo "${exe}"
}

function main() {
    local -r cacheDir="${XDG_CACHE_HOME:-${HOME}/.cache}/dhall-lsp-server/${dhallVersion}"

    local tarball
    tarball=$(download "${cacheDir}" "${downloadUrl}" "${downloadSha256}")

    local exe
    exe=$(unpack "${cacheDir}" "${tarball}")

    exec "${exe}" "$@"
}

main "$@"
