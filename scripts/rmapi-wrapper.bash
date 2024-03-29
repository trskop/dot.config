#!/usr/bin/env bash

# Copyright (c) 2020-2021, Peter Trsko
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

declare -r repoUrl='https://github.com/juruen/rmapi'
declare -r version='0.0.15'
declare -r arch='linuxx86-64' # TODO: Support other?
declare -r downloadUrl="${repoUrl}/releases/download/v${version}/rmapi-${arch}.tar.gz"
declare -r downloadSha256='bfe1d3430276804066ee58b6ea733bdfdcb2ad06e687bfc14ca0d2c33663f5fc'

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

    local -r exe="${root}/rmapi"

    if [[ ! -e "${exe}" ]]; then
        tar -C "${root}" -xf "${tarball}" || return 1
        # chmod +x "${exe}"
    fi

    echo "${exe}"
}

function main() {
    local -r cacheDir="${XDG_CACHE_HOME:-${HOME}/.cache}/rmapi/${version}"

    local tarball
    tarball=$(download "${cacheDir}" "${downloadUrl}" "${downloadSha256}")

    local exe
    exe=$(unpack "${cacheDir}" "${tarball}")

    exec "${exe}" "$@"
}

main "$@"
