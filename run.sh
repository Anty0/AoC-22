#!/bin/sh

set -e

day="$1"
shift

make "./out/cal$day"
"./out/cal$day" "$@" +RTS -N8
