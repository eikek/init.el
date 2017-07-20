#!/usr/bin/env bash

echo ":a import <nixpkgs> {}
contains = s: (lib.hasPrefix \"$1\" s) || (lib.hasSuffix \"$1\" s)
names = (builtins.attrNames emacsPackages) ++ (builtins.attrNames emacs25PackagesNg)
builtins.filter contains names" | nix-repl
