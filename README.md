# Siarch Nix Config Manager

This project manages personal NixOS/home-manager/nix-darwin configurations across Linux and macOS machines.
It leverages `flake.nix` plus the automation entry point `siarch.sh` so a single repository can
rebuild the appropriate flake target (Linux home-manager, macOS nix-darwin, or full NixOS) per host.

## Why using this tool

I use this repository to keep multiple devices in sync through Nix flakes instead of copying dotfiles
manually. The flake exposes a shared `home/` module tree plus host-specific bits under `hosts/`,
so each machine simply imports the pieces it needs while reusing the same bootstrap logic. `siarch.sh`
wraps the usual Nix invocations with installation checks, making it easy to declaratively reprovision
new machines or recover a busted config.

## How to install it

1. Make sure Git is available on the machine, then clone this repo:
   ```bash
   git clone https://github.com/mrkatebzadeh/Siarch.git ~/.siarch
   cd ~/.siarch
   ```
2. Run the helper script; it installs Nix (daemon mode) if necessary, then installs Home Manager:
   ```bash
   ./siarch.sh <host-name>
   ```

## How to use it

- Pick the host target that matches one of the entries under `hosts/` (e.g., `x230`, `homelab`, `shiraz`).
- Invoke the bootstrap script with the host name plus any extra arguments you would normally pass to
  `home-manager` or `nix-darwin`:
  ```bash
  ./siarch.sh x230  # runs nix run home-manager -- switch --flake " .#x230"
  ./siarch.sh macbookair  # runs nix-darwin switch with the same flake target
  ```
- To inspect the flake outputs without rebuilding, run `nix flake check` or `nix flake show` from the
  repository root. This evaluates the host configurations and ensures they are syntactically valid.

## How to config it

- `flake.nix` defines:
  - `packages`, `formatter`, and overlays for all supported systems.
  - A `nixosConfigurations` set for machines with a full NixOS system definition.
  - A `homeConfigurations` map generated from `homeHosts`, so every Linux host reuses the same
    shared `./home` modules plus any host-specific overrides.
  - `darwinConfigurations` built via `nix-darwin` for macOS hosts.
- `hosts/<name>/` directories hold `configuration.nix` files (and hardware configs) for each machine.
  Modify those files to adjust system services, packages, and user settings.
- The `home/` directory exports reusable Home Manager modules (zsh, editor, tools, etc.) that every host imports.
- After editing any of these files, re-run `./siarch.sh <host-name>` or `nix flake check` to validate the change
  before invoking `home-manager switch` or `nixos-rebuild` manually.
