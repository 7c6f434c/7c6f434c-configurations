{pkgs,...}:
    {
      enable = true;	

      /*package =  
      	(pkgs.xorg // {
		#xf86videoati = pkgs.xorg.xf86videoati_68_192;
		xf86videoati = pkgs.xorgReplacements.xf86videoati {
			suffix = "git";
			src = "" + /home/repos/xf86-video-ati ;
		};
	});*/
      
      # Do not run on startup
      autorun = false;							
      
      # Create /etc/X11/xorg.conf for convenience
      exportConfiguration = true;					
      
      # Needed by, say, xmove
      enableTCP = true;							
      
      # WXGA. 15.4" widescreen
      resolutions = [{x = 1280; y=800;}];
      virtualScreen = {x=3072; y=1280;};
      
	# ATi VESA BIOS doesn't support 1280x800
      videoDrivers = [ "ati" ]; 

      synaptics = {
        enable = true;
	dev = null;
      };

	# I get Caps-toggle between Latin/Cyrillic/Greek
	# Right Flag key allows entering, say, euro sign
	# or accents (using deadkeys in level 3)
	# Look xkeyboard_config package source.               
        layout = "us(altgr-intl-rich),ru(common),gr(basic)";			
        xkbOptions = "grp:caps_toggle, grp_led:caps, lv3:lwin_switch, terminate:ctrl_alt_bksp";	
      																		
      useXFS = "unix/:7100";
      driSupport = false;

      defaultDepth = 24;
      # Will watch free ATi drivers progress...
      # Force DRI on - or do not..
      deviceSection = ''
	Option "RenderAccel" "true"
	Option "AccelMethod" "EXA"
      '';

     serverLayoutSection = ''
	Option "AIGLX" "true"
  	#InputDevice  "Mouse[0]"
     ''; 

    }
