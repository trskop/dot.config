# shellcheck shell=bash

# ~/.bashrc: executed by bash(1) for non-login shells.
#
# See /usr/share/doc/bash/examples/startup-files (in the package bash-doc) for
# examples.

# If not running interactively, don't do anything.
case $- in
    *i*) ;;
      *) return;;
esac

# shellcheck source=/dev/null
[[ -x "${HOME}/.local/bin/genbashrc" ]] && source <("${HOME}/.local/bin/genbashrc")
