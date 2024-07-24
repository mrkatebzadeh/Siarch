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
    nixgl.url = "github:nix-community/nixGL";
  };

  outputs =
    inputs@{ self
    , nixpkgs-unstable
    , nixpkgs
    , home-manager
    , nix-darwin
    , nixgl
    , ...
    }:
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

      nixosConfigurations = {
        x230 = mkNixos [
          ./hosts/x230/nixos/configuration.nix
        ];
      };
      homeConfigurations = {
        x230 = mkHome [
          ./hosts/x230/configuration.nix
          ./home
          {
            home = {
              inherit username;
              homeDirectory = "/home/${username}";
            };
          }
        ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
      homeConfigurations = {
        shiraz = mkHome [
          ./hosts/shiraz/configuration.nix
          ./home
          {
            home = {
              inherit username;
              homeDirectory = "/home/${username}";
            };
          }
        ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
      homeConfigurations = {
        cloudlab = mkHome [
          ./hosts/cloudlab/configuration.nix
          ./home
          {
            home = {
              inherit username;
              homeDirectory = "/users/${username}";
            };
          }
        ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
      homeConfigurations = {
        duck = mkHome [
          ./hosts/duck/configuration.nix
          ./home
          {
            home = {
              username = "root";
              homeDirectory = "/root";
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
          modules = [
            ./hosts/macbookair/configuration.nix
            home-manager.darwinModules.home-manager
            {
              users.users.${username} = {
                name = username;
                home = "/Users/${username}";
              };
              home-manager = {
                backupFileExtension = "backup";
                useGlobalPkgs = false;
                useUserPackages = false;
                extraSpecialArgs = { inherit outputs; };
                users.${username}.imports = [
                  ./home
                  ./hosts/macbookair/home.nix
                ];
              };
            }
          ];
        };
      };

      homeConfigurations = {
        homelab = mkHome [ ./hosts/homelab/configuration.nix ]
          nixpkgs.legacyPackages."x86_64-linux";
      };
    };
}
