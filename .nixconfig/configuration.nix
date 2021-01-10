{ config, pks, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.grub = {
    enable = true;
    version = 2;
    # efiSupport = true;
    device = "/dev/sda";
  };
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.hostName = "nix";
  networking.wireless.enable = true;

  time.timeZone = "Europe/Berlin";

  networking.interfaces.enp0s3.useDHCP = true;

  i18n.defaultLocale = "de_DE.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "de";
  };

  services.xserver = {
    enable = true;
    layout = "de";
    videoDrivers = ["intel"];
    desktopManager = {
      default = "none";
      xterm.enable = false;
    };
    displayManager.gdm.enable = true;
    windowManager.i3.enable = true;

    libinput.enable = true;
  };

  services.printing.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.max = {
    isNormaluser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  environment.systemPackages = with pkgs; [
    alacritty
    curl
    vim
    firefox
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.openssh.enable = true;

  # networking.firewall.allowedTCPPorts = [];
  # networking.firewall.allowedUDPPorts = [];
  networking.firewall.enable = true;

  # Determines NixOS release.
  # Don't change this before reading the docs.
  # https://nixos.org/nixos/options.html
  system.stateVersion = "20.09";
}
