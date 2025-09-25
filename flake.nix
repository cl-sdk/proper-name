{
  description = "proper-name flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.05";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {

        packages.${system} = {
          default = pkgs.mkShell {
            name = "proper-name";
            buildInputs = [pkgs.sbcl];
          };
        };

        devShell = pkgs.mkShell {
            name = "proper-name";
            buildInputs = [pkgs.sbcl];
        };
      });
}
