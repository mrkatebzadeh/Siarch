{
  description = "Siarch Nix config";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nhx = {
      url = "github:Ra77a3l3-jar/nhx";
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
    , hardware
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
      linuxSystems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
      lib = nixpkgs.lib;
      baseHomePkgs = nixpkgs.legacyPackages."x86_64-linux";

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
      homeHosts = {
        x230 = {
          modules = [ ./hosts/x230/configuration.nix ];
          username = username;
          homeDirectory = "/home/${username}";
        };
        shiraz = {
          modules = [ ./hosts/shiraz/configuration.nix ];
          username = username;
          homeDirectory = "/home/${username}";
        };
        cloudlab = {
          modules = [ ./hosts/cloudlab/configuration.nix ];
          username = username;
          homeDirectory = "/users/${username}";
        };
        duck = {
          modules = [ ./hosts/duck/configuration.nix ];
          username = "root";
          homeDirectory = "/root";
        };
        homelab = {
          modules = [ ./hosts/homelab/configuration.nix ];
          username = username;
          homeDirectory = "/home/${username}";
        };
      };
      mkHomeModules = host:
        host.modules
          ++ [
            ./home
            {
              home = {
                inherit (host) username homeDirectory;
              };
            }
          ]
          ++ (host.extraModules or []);

    in
    {
      packages =
        lib.genAttrs linuxSystems (system: nixpkgs.legacyPackages.${system}.hello);
      formatter =
        forAllSystems (system: nixpkgs.legacyPackages.${system}.nixfmt);
      overlays = import ./overlays { inherit inputs; };

      nixosConfigurations = {
        x230 = mkNixos [
          ./hosts/x230/nixos/configuration.nix
        ];
        homelab = mkNixos [
          ./hosts/homelab/nixos/configuration.nix
        ];
      };
      homeConfigurations = lib.mapAttrs
        (name: host:
          mkHome (mkHomeModules host) (host.pkgs or baseHomePkgs)
        )
        homeHosts;
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
                extraSpecialArgs = { inherit inputs outputs; };
                users.${username}.imports = [
                  ./home
                  ./hosts/macbookair/home.nix
                ];
              };
            }
          ];
        };
      };

    };
}
