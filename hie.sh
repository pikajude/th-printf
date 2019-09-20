#!/usr/bin/env bash

cd "$(dirname "${BASH_SOURCE[0]}")" || exit 1

argv=( "$@" )
argv=( "${argv[@]/\'/\'\\\'\'}" )
argv=( "${argv[@]/#/\'}" )
argv=( "${argv[@]/%/\'}" )

export HIE=1

exec nix-shell --run "hie ${argv[*]}"
