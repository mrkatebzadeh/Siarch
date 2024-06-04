{
  description = "Siarch Nix config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs@{self, nixpkgs-unstable, nixpkgs, home-manager, nix-darwin, ... }:
    let
      username = "siavash";
      inherit (self) outputs;
      systems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkNixos = modules:
        nixpkgs.lib.nixosSystem {
          inherit modules;
          specialArgs = { inherit inputs outputs; };
        };
      mkHome = modules: pkgs:
        home-manager.lib.homeManagerConfiguration {
          inherit modules pkgs;
          extraSpecialArgs = { inherit inputs outputs; };
        };

    in
    {
      packages =
        forAllSystems (system: nixpkgs.legacyPackages.${system});
      formatter =
        forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
      overlays = import ./overlays { inherit inputs; };

      homeConfigurations = {
        thinkpad = mkHome [ ./hosts/thinkpad_x230.nix
          ./home
          {
            home = {
              username = username;
              homeDirectory = "/home/${username}";
              stateVersion = "22.11";
            };
          }
        ]
          nixpkgs.legacyPackages."x86_64-linux";
      };

      darwinConfigurations = {
        macbookair = nix-darwin.lib.darwinSystem {
          specialArgs = {
            inherit inputs outputs;
            pkgs-unstable = import nixpkgs-unstable {
              system = "aarch64-darwin";
              config.allowUnfree = true;
            };
          };
          system = "aarch64-darwin";
          modules = [ ./hosts/macbookair.nix ];
        };
      };

      homeConfigurations = {
        homelab = mkHome [ ./hosts/homelab.nix ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
    };
}
