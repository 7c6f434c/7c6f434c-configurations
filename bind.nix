{pkgs, config, ...}:
{
      enable = true;
      ipv4Only = true;
      cacheNetworks = [
        "127.0.0.0/24"
	"::1/128"
	"localhost"
	"192.168.0.0/16"
      ];
      zones = [
        {
	  name = __substring 0 8 config.networking.hostName;
	  file = "/root/nix-sysconfig/" + (builtins.substring 0 8 config.networking.hostName);
	}
	{
	  name = "root-servers.net";
	  file = "" + ./root-servers;
	}
      ];
    }

