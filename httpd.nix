{pkgs, config, ...}:
    let raskinHomeDir = ''
          <Directory /home/raskin/public_html>
            Options FollowSymlinks
            AllowOverride All
          </Directory>
    '';
	in
    {
      enable = true;

      adminAddr = "dev-null@example.com";

	# I trust myself. I can write .htaccess
      extraConfig = raskinHomeDir;
      hostName = "_default_";
      
      extraModules = [{ name = "php5"; path = "${pkgs.php}/modules/libphp5.so"; }];

	virtualHosts = 
	let
		sslFiles = {
			sslServerCert = "/var/certs/www/host.cert";
      			sslServerKey = "/var/certs/www/host.key";
		};
		homeDirTune = {
      			enableUserDir = true;						
			extraConfig = raskinHomeDir;
		};
		raskinSubDomain = {
			hostName = "raskin.${config.networking.hostName}.${config.networking.domain}";
			documentRoot = "/home/raskin/public_html" ;
		};
		mainDomain = {
			#hostName = "*";
			documentRoot = "/var/www";
		};
		optionalSSL = localCfg : [
			localCfg
			(localCfg // sslFiles // {enableSSL=true;})
		];
	in []
	++ (optionalSSL (mainDomain // homeDirTune))
	++ (optionalSSL (raskinSubDomain // homeDirTune))
	++ (optionalSSL (raskinSubDomain // homeDirTune // {hostName = "raskin";}))
	++ (optionalSSL (raskinSubDomain // homeDirTune // {hostName = "raskin-ipv4";}))
	;

	extraSubservices = [ ];
    }
