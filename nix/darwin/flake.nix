{
  description = "Siarch Darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    nix-cli.url = "github:peterldowns/nix-search-cli";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, nix-cli }:
    let
      pkgs = import nixpkgs {
        system = "aarch64-darwin";
        config = {
          allowUnfree = true;
          allowUnfreePredicate = _: true;
        };
      };
      configuration = { ... }: {
        environment.systemPackages =
          [
            pkgs.git
            pkgs.vim
            pkgs.python311Packages.pynvim
            pkgs.skhd
            pkgs.yabai
            pkgs.jq
            pkgs.sketchybar
            pkgs.nix-search-cli
            pkgs.zellij
            pkgs.lazydocker
            pkgs.lazygit
            pkgs.lsd
            pkgs.htop
            pkgs.btop
            pkgs.jadx
            pkgs.nushell
            pkgs.emacs
            pkgs.curl
            pkgs.wget
            pkgs.stow
            pkgs.stylua
            pkgs.tree-sitter
            pkgs.kitty
            pkgs.raycast
            pkgs.jankyborders
            pkgs.asdf-vm
          ];

        security.pam.enableSudoTouchIdAuth = true;
        services.nix-daemon.enable = true;
        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        # Create /etc/zshrc that loads the nix-darwin environment.
        programs.zsh.enable = true; # default shell on catalina
        # programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 4;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";
        services.yabai.enable = true;
        services.skhd.enable = true;
        services.sketchybar.enable = true;
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#simple
      darwinConfigurations."SiAir" = nix-darwin.lib.darwinSystem {
        modules = [ configuration ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."SiAir".pkgs;
    };
}
