# My Emacs Config

I install emacs using [nix](https://nixos.org/nix), a package manager
available for Linux and OSX. It allows to install emacs and all
desired packages and keep it up-to-date.

Packages are taken from the
[nixpkgs](https://github.com/NixOS/nixpkgs) collection, which creates
them from melpa and other sources.

Emacs and its packages are defined in my [nix
config](https://github.com/eikek/confnix/tree/nixos-23.11/pkgs/emacs).
More information about how to install emacs this way is in the
[manual](https://nixos.org/nixos/manual/index.html#module-services-emacs).

A list of packages can be generated with `list-emacs-pkgs.sh`
(requires `nix` to be installed):

```
./list-emacs-pkgs.sh counsel | jq
```

Then [use-package](https://github.com/jwiegley/use-package) is used to
configure emacs.
