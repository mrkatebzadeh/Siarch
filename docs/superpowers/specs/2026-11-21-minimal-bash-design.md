# Minimal Bash Setup Design

## Goal

Add a colorful, minimal Bash configuration that works both as a Stow package and through the `macbookair` Home Manager profile. Bash remains an optional shell; Zsh stays the default.

## Design

- Add `home/dotfiles/bash/.bashrc` and `home/dotfiles/bash/.bash_profile` as a Stow-ready package.
- For interactive Bash sessions, reuse `~/.config/shell/aliasrc`, initialize Starship if available, and otherwise show a small colored prompt.
- Make login Bash source `.bashrc` so macOS login shells use the same setup.
- In `hosts/macbookair/home.nix`, link both startup files from the dotfiles package using the existing Home Manager symlink pattern.
- Do not change the default shell, add dependencies, or add Bash-specific feature layers.

## Validation

Check Bash syntax, exercise the non-Starship fallback, and evaluate the `macbookair` Nix configuration if available.
