{ pkgs, ... }: {
  packages = with pkgs; [
    evil-helix
    yazi
    wezterm
    gnupg1
    emacs-lsp-booster
    #
    virtualenv
    pdf2svg
    patchelf
    bat
    atuin
    btop
    coreutils
    curl
    dbus
    fastfetch
    fd
    fontconfig
    fzf
    gawk
    git
    git-filter-repo
    gpg-tui
    htop
    lazydocker
    lazygit
    less
    lsd
    jq
    nix-search-cli
    nushell
    ripgrep
    stow
    tealdeer
    tree-sitter
    wget
    (python3.withPackages (ps: with ps; with python3Packages; [
      jupyter
      ipython
      pynvim
      pandas
      numpy
      matplotlib
      seaborn
      epc
      orjson
      sexpdata
      six
      setuptools
      rapidfuzz
      watchdog
    ]))
  ];
}
