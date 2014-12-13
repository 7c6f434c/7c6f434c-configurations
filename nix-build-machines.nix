{
  gb-bxi7-4770r-1 = {
    hostName = "192.168.0.202";
    sshUser = "nix";
    sshKey = "/nix/var/nix/keys/id_nix_remote";
    system = "x86_64-linux"; 
    maxJobs = 6;
    supportedFeatures = ["kvm" ];
    speedFactor = "2";
  };
  gb-bxi7-4770r-1-i686 = {
    hostName = "192.168.0.202";
    sshUser = "nix";
    sshKey = "/nix/var/nix/keys/id_nix_remote";
    system = "i686-linux"; 
    maxJobs = 6;
    supportedFeatures = ["kvm" ];
    speedFactor = "2";
  };
}
