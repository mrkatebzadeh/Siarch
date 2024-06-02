{
  description = "Siarch Nix config";
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    # home-manager.url = "github:nix-community/home-manager/master";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nix-darwin.url = "github:lnl7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

  };
  outputs =
    inputs@{ nixpkgs-unstable
    , nixpkgs
    , home-manager
    , nix-darwin
    , ...
    }:
    let
      unstable-packages = final: _prev: {
        unstable = import inputs.nixpkgs-unstable {
          system = final.system;
          config.allowUnfree = true;
        };
      };
    in
    {
      darwinConfigurations.SiAir = nix-darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        pkgs = import nixpkgs {
          config = { allowUnfree = true; };
          overlays = [
            unstable-packages
          ];
          system = "aarch64-darwin";
        };

        modules = [
          ./darwin
          home-manager.darwinModules.home-manager
          {
            users.users.siavash = {
              name = "siavash";
              home = "/Users/siavash";
            };
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              extraSpecialArgs = { };
              users.siavash.imports = [ ./home ];
            };
          }
        ];
      };
    };
}
