#!/usr/bin/zsh

for file in $@; do
  scan-build -enable-checker alpha.security.taint.TaintPropagation \
    -analyzer-config alpha.security.taint.TaintPropagation:Config=taintprop_config.yaml \
    clang -c $file
done
