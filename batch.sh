#!/usr/bin/env bash

set -u
set -e

languages=(
    'bash'
    'c'
    'c-sharp'
    'clojure'
    'cmake'
    'cpp'
    'css'
    'scss'
    'dockerfile'
    'elisp'
    'elixir'
    'glsl'
    'go'
    'go-mod'
    'heex'
    'html'
    'janet-simple'
    'java'
    'javascript'
    'json'
    'julia'
    'make'
    'markdown'
    'org'
    'perl'
    'proto'
    'python'
    'ruby'
    'rust'
    'surface'
    'sql'
    'toml'
    'tsx'
    'typescript'
    'verilog'
    'vhdl'
    'wgsl'
    'yaml'
    'dart'
)

if [ -z "${JOBS:-}" ]
then
    for language in "${languages[@]}"
    do
        ./build.sh "${language}"
    done
else
    printf "%s\n" "${languages[@]}" | xargs -P"${JOBS}" -n1 ./build.sh
fi
