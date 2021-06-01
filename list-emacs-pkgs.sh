#!/usr/bin/env bash

echo ":a import <nixpkgs> {}
contains = s: (lib.hasPrefix \"$1\" s) || (lib.hasSuffix \"$1\" s)
names = (builtins.attrNames emacsPackages) ++ (builtins.attrNames emacsPackagesNg)
builtins.filter contains names" | nix repl | tail -n2 | tr ' ' '\n' | sed 's/["]//g' | sort | uniq
