{pkgs,...}:
    {
      enable = true;	

      # Do not run on startup
      autorun = false;							
      
      # Create /etc/X11/xorg.conf for convenience
      exportConfiguration = true;					
      
      # Needed by, say, xmove
      enableTCP = true;							
      
      virtualScreen = {x=3520; y=2200;};
      
      # I get Caps-toggle between Latin/Cyrillic/Greek
      # Right Flag key allows entering, say, euro sign
      # or accents (using deadkeys in level 3)
      # Look xkeyboard_config package source.               
      layout = "us(altgr-intl),ru(common),gr(basic)";			
      xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
      																		
      #useXFS = "unix/:7100";
      driSupport = true;

      serverLayoutSection = ''
        Option "AIGLX" "true"
      ''; 

      desktopManager.xfce.enable = false;

    }
