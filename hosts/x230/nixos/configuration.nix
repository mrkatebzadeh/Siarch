# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:

{
  imports =
    [
      # Include the results of the hardware scan.
      <nixos-hardware/lenovo/thinkpad/x230>
      <home-manager/nixos>
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
      networkmanager-openconnect
    ];
  };

  # Set your time zone.
  time.timeZone = "Europe/London";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    font = "ter-v14n";
    packages = with pkgs; [ terminus_font ];
    useXkbConfig = true; # use xkb.options in tty.
  };

  hardware.bluetooth.enable = true;
  hardware.bluetooth.powerOnBoot = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkb.layout = "gb";
    xkb.variant = "intl";
    videoDrivers = [ "intel" ];
    autorun = true;
    displayManager.lightdm.enable = false;
  };
  programs.hyprland.enable = true;

  services.kmscon.enable = true;

  # services.printing.enable = true;

  # Enable sound.
  hardware.enableRedistributableFirmware = true;
  sound = {
    enable = true;
    mediaKeys = {
      enable = true;
      volumeStep = "5%";
    };
  };
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true; ## If compatibility with 32-bit applications is desired.
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  users.users.siavash = {
    isSystemUser = true;
    uid = 1000;
    group = "siavash";
    createHome = true;
    home = "/home/siavash";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "audio" "video" "networkmanager" ]; # Enable ‘sudo’ for the user.
    #   packages = with pkgs; [
    #     firefox
    #     tree
    #   ];
  };
  users.groups.siavash = { };
  programs.zsh.enable = true;
  programs.light.enable = true;
  services.actkbd = {
    enable = true;
    bindings = [
      { keys = [ 222 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -A 10"; }
      { keys = [ 223 ]; events = [ "key" ]; command = "/run/current-system/sw/bin/light -U 10"; }
    ];
  };


  environment.systemPackages = with pkgs; [
    vim
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  system.stateVersion = "24.05"; # Did you read the comment?
}
