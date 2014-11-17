let 
pkgs=import <nixpkgs> {};
texLivePaths = with pkgs; [
	texLive texLiveExtra lmodern texLiveCMSuper
	texLiveLatexXColor texLivePGF
	texLiveBeamer texLiveModerncv tipa texLiveContext
	texDisser lmmath
];
myTexLive = pkgs.texLiveAggregationFun {
	paths = texLivePaths;
};
in 
with pkgs; myEnvFun {
	buildInputs=[myTexLive bash coreutils strace xpdf fontconfig asymptote
	  ghostscript];
	name="texlive-test";
}
