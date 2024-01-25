#!/usr/bin/env bash

echo ":a import <nixpkgs> {}
contains = s: (lib.hasPrefix \"$1\" s) || (lib.hasSuffix \"$1\" s)
names = (builtins.attrNames emacsPackages) ++ (builtins.attrNames emacs.pkgs)
builtins.trace (builtins.toJSON (builtins.filter contains names))  0" | nix repl --quiet 2>&1 | grep "^trace" | sed 's/trace: //g'
