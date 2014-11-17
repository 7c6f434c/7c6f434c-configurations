pkgs: with pkgs;
vimHugeX.merge rec {
  name = "vim-huge-all";
  features = "huge";
  inherit gmp mpfr 
    ruby gettext;
  flags = composableDerivation.edf ({name = "ecl"; feat = "eclinterp"; enable = {buildNativeInputs = [ecl gmp mpfr];};});
  lua = lua5;
  cfg = {
        pythonSupport = true;
	tclSupport = true;
	cscopeSupport = true;
	xsmpSupport = true;
	multibyteSupport = true;
	ximSupport = true;
	nlsSupport = true;

	perlSupport = false;
	rubySupport = true;
	sniffSupport = true;
	#mzSchemeSupport = true;
	luaSupport = true;
	#hangulSupport = true;
	gettextSupport = true;
  };
}
