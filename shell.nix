# Use this file with nix-shell or similar tools; see https://nixos.org/
with import <nixpkgs> {};
mkShell { buildInputs = [ freeimage ]
                        ++ import ./lib/github.com/diku-dk/lys/build-inputs.nix pkgs; }
