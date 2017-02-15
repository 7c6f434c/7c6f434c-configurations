{pkgs, ...}:
  {
    enableGhostscriptFonts = true;
	
	# So there is a big directory in current-system 
	# with all fonts.
    enableFontDir = true;
    									
    fonts = ([
      pkgs.dejavu_fonts
      (pkgs.dejavu_fonts + "/share/fonts/truetype/")
      (pkgs.ghostscript + "/share/ghostscript/fonts/")
      pkgs.xorg.fontcronyxcyrillic
      pkgs.xorg.fontadobe100dpi
      pkgs.xorg.fontadobe75dpi
      pkgs.xorg.fontxfree86type1
      pkgs.mph_2b_damase
      pkgs.liberation_ttf
      pkgs.unifont
      pkgs.unifont_upper
      pkgs.wqy_zenhei
      pkgs.clearlyU
      pkgs.ucsFonts
      pkgs.junicode
      pkgs.lmodern
      pkgs.libertine
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
      pkgs.freefont_ttf
      pkgs.comic-neue

      pkgs.noto-fonts
      pkgs.noto-fonts-cjk
      pkgs.noto-fonts-emoji

      pkgs.paratype-pt-mono
      pkgs.paratype-pt-sans
      pkgs.paratype-pt-serif

      pkgs.dejavu_fonts
    ]);

    enableCoreFonts = false;
  }
