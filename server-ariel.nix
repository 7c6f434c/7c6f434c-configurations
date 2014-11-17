{config, pkgs, ...}:

{
  require = [];

  # Add your own options below and run "nixos-rebuild switch".
  # E.g.,
  #   services.sshd.enable = true;
  fileSystems = [
   {
     device = "/dev/sda1";
     mountPoint = "/";
   }
   {
     device = "/dev/sdb6";
     mountPoint = "/var/backups";
   }
  ];

  swapDevices = [{device = "/dev/sda5";}];

  networking= {
    useDHCP = false;
    hostName = "ariel.dev.mccme.ru";
    defaultGateway = "77.108.123.226";
    nameservers = ["213.171.48.241"];
    domain = "dev.mccme.ru";
    interfaces = [
      {
        name="eth0";
        ipAddress = "77.108.123.230";
        subnetMask = "255.255.255.224";
      }
    ];
  };

  services = {
    openssh.enable = true;
    monit = {
      enable = true;
      config = ''
      set alert raskin@mccme.ru
      set alert pam@mccme.ru
      
      set mailserver smtp.mccme.ru
      
      set eventqueue
        basedir /var/monit
        slots 65535
      
      set daemon 60
      
      set logfile /var/log/monit
      
      check system ariel.dev.mccme.ru
        if memory usage > 90% for 10 cycles then alert
        if loadavg (5min) > 5 for 10 cycles then alert
        if cpu usage (user) > 70% for 5 cycles then alert
        if cpu usage (system) > 70% for 5 cycles then alert
        if cpu usage (wait) > 80% for 10 cycles then alert
      
      check device -slash-at-ariel.dev.mccme.ru with path /
        if space usage > 70% then alert 
        if inode usage > 70% then alert
      
      check device -var-backups-at-ariel.dev.mccme.ru with path /var/backups
        if space usage > 85% then alert
        if space usage > 90% then alert
        if space usage > 95% then alert
      '';
    };
  };

  installer = {
    manifests = [
      http://hydra.nixos.org/project/nixpkgs/channel/latest/MANIFEST
      http://hydra.nixos.org/project/nixos/channel/latest/MANIFEST
    ];
  };

  environment.systemPackages = with pkgs;
    [
      vim screen rsync openssh
      mc socat mercurial monotone
      subversion monit utillinuxngCurses
    ];

  boot.loader.grub.device = "/dev/sda";
  boot.initrd.kernelModules = ["pata_via" "loop" "unix" 
    "uhci_hcd" "ehci_hcd"];

  security = {
    sudo = {
      enable = true;
      configFile = ''
      #generated
      raskin ALL = (ALL) ALL
      vitar ALL = (ALL) ALL
      dubov ALL = (ALL) ALL
      pam ALL = (ALL) ALL
      '';
    };
  };
}

