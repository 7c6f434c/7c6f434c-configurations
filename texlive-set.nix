pkgs:
/*
let 
  texLivePaths = with pkgs; [
          texLive texLiveExtra lmodern texLiveCMSuper 
	  texLiveLatexXColor texLivePGF
	  texLiveBeamer texLiveModerncv tipa texLiveContext 
          texDisser lmmath texinfo5 tex4ht texLiveModerntimeline
     ];
  myTexLive = pkgs.texLiveAggregationFun {
    paths = texLivePaths;
  };
in
  myTexLive
*/

pkgs.texlive.combine {
  inherit(pkgs.texlive)
    scheme-medium collection-langcyrillic collection-genericrecommended
    cm-super cm-unicode xcolor pgf beamer moderncv tipa disser 
    moderntimeline texinfo metafont multibib collection-fontutils
    epstopdf epspdfconversion epspdf algorithm2e relsize type1cm
    ucs todonotes xargs forloop pbox varwidth bigfoot environ trimspaces
    forest arydshln pgfopts elocalloc libertine mweights fontaxes
    totpages comment
    ;
}
