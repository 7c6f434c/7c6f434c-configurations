{pkgs, ...}:
{
      enableTCPIP = true;
      enable = true;
      authentication = ''
        host all all 192.168.0.0/16 md5
      '';
      extraConfig = ''
        work_mem = 16MB
        shared_buffers = 1GB
      '';
      package = pkgs.postgresql92;
    }
