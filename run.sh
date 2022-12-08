#!/bin/sh

day="$1"
shift

make "./out/cal$day"
"./out/cal$day" "$@"
