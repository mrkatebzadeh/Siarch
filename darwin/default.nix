{ pkgs, ... }:
let wallpaper_path = "~/.siarch/home/dotfiles/share/backgrounds/wall.jpg";
in {
  environment = {
    shells = with pkgs; [ bash zsh ];
    loginShell = pkgs.zsh;
    systemPackages = with pkgs; [
      asdf-vm
      btop
      coreutils
      curl
      emacs
      fd
      fontconfig
      fzf
      gawk
      git
      git-filter-repo
      gpg-tui
      htop
      jadx
      kitty
      lazydocker
      lazygit
      less
      lsd
      nix-search-cli
      nushell
      python311Packages.pynvim
      ripgrep
      slack
      starship
      stow
      stylua
      telegram-desktop
      tree-sitter
      unstable.jankyborders
      unstable.jq
      unstable.neovim
      unstable.sketchybar
      unstable.skhd
      unstable.yabai
      vim
      wget
      zellij
    ];
    systemPath = [ "/opt/homebrew/bin" ];
    pathsToLink = [ "/Applications" ];
    variables = {
      FONTCONFIG_PATH = "~/.config/fontconfig";
    };
  };
  fonts.fontDir.enable = true;
  fonts.fonts = [
    pkgs.libertine
  ];

  services.yabai = {
    enable = true;
    # enableScriptingAddition = true;
  };
  services.skhd.enable = true;
  services.sketchybar.enable = true;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToEscape = true;
  services.nix-daemon.enable = true;
  system.defaults.CustomUserPreferences = {
    trackpad = {
      Clicking = true;
    };
    "com.apple.desktopservices" = {
      DSDontWriteNetworkStores = true;
      DSDontWriteUSBStores = true;
    };
    "com.apple.screensaver" = {
      askForPassword = 1;
      askForPasswordDelay = 0;
    };
    NSGlobalDomain = {
      WebKitDeveloperExtras = true;
      InitialKeyRepeat = 14;
      AppleInterfaceStyle = "Dark";
      KeyRepeat = 5;
    };
    "com.apple.dock" = {
      autohide = true;
    };
    "com.apple.finder" = {
      ShowExternalHardDrivesOnDesktop = false;
      ShowHardDrivesOnDesktop = false;
      ShowMountedServersOnDesktop = false;
      ShowRemovableMediaOnDesktop = false;
      _FXSortFoldersFirst = true;
      ShowStatusBar = true;
      FXEnableExtensionChangeWarning = false;
      FXDefaultSearchScope = "SCcf";
    };
    "com.apple.mail" = {
      DisableInlineAttachmentViewing = true;
    };
    "com.apple.AdLib" = {
      allowApplePersonalizedAdvertising = false;
    };
    "com.apple.print.PrintingPrefs" = {
      "Quit When Finished" = true;
    };
    "com.apple.SoftwareUpdate" = {
      AutomaticCheckEnabled = true;
      ScheduleFrequency = 1;
      AutomaticDownload = 1;
      CriticalUpdateInstall = 1;
    };
  };
  system.activationScripts.postUserActivation.text = ''
    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
    /usr/bin/osascript -e "tell application \"System Events\" to tell every desktop to set picture to \"${wallpaper_path}\" as POSIX file"
  '';
  system.stateVersion = 4;
  homebrew = {
    onActivation.cleanup = "uninstall";
    enable = true;
    caskArgs.no_quarantine = true;
    global.brewfile = true;
    masApps = { };
    casks = [
      "raycast"
      "brave-browser"
    ];
    taps = [ ];
    brews = [
      "gnu-sed"
    ];
  };
}