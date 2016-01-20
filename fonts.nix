{pkgs, ...}:
  {
    enableGhostscriptFonts = true;
	
	# So there is a big directory in current-system 
	# with all fonts.
    enableFontDir = true;
    									
    fonts = ([
      (pkgs.ghostscript + "/share/ghostscript/fonts/")
      pkgs.xorg.fontcronyxcyrillic
      pkgs.xorg.fontadobe100dpi
      pkgs.xorg.fontadobe75dpi
      pkgs.xorg.fontxfree86type1
      pkgs.mph_2b_damase
      pkgs.dejavu_fonts
      pkgs.liberation_ttf
      pkgs.unifont
      pkgs.wqy_zenhei
      pkgs.clearlyU
      pkgs.ucsFonts
      pkgs.junicode
      pkgs.lmodern
      (pkgs.libertine.passthru.function {
          createTTF = false;
	  createPFM = false;
	  createPFB = false;
	  createAFM = false;
	  createMAP = false;
	  createENC = false;
      })
      pkgs.arkpandora_ttf
      pkgs.andagii
      pkgs.anonymousPro
      pkgs.inconsolata
      pkgs.theano
      pkgs.oldstandard
      pkgs.tempora_lgc
      pkgs.gentium
      pkgs.cm_unicode
      pkgs.lmmath

      pkgs.paratype-pt-mono
      pkgs.paratype-pt-sans
      pkgs.paratype-pt-serif
    ]);

    enableCoreFonts = false;
  }
