# My Emacs Config

I install emacs using [nix](https://nixos.org/nix), a package manager
available for Linux and OSX. It allows to install emacs and all
desired packages and keep it updated as you wish.

Emacs and its packages are defined in `default.nix`. It can be build using

```
nix-build default.nix
```

After that completes, emacs is here: `./result/bin/emacs`.

To integrate it with `nix-env`, add something like the following to
`~/.nixpkgs/config`:


``` nix
let
  emacs =
    let
      dotemacs = builtins.filter builtins.pathExists [
         ../.emacs.d/default.nix
         ../workspace/projects/dot-emacs/default.nix
      ];
    in
    p: if (dotemacs == []) then {}
       else { myemacs = (import (builtins.head dotemacs) { pkgs = p; }); };
in
{
  packageOverrides = emacs;
}
```

The list of paths to `default.nix` must be adjusted as
appropriate. Then using `nix-env -iA myemacs` (or `nix-env -iA
nixos.myemacs`) installs this specific emacs in you local environment.

More information about how to install emacs this way is in
the
[manual](https://nixos.org/nixos/manual/index.html#module-services-emacs).
