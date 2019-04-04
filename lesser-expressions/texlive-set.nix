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
    scheme-medium collection-langcyrillic 
    cm-super cm-unicode xcolor pgf beamer moderncv tipa disser urlbst
    moderntimeline texinfo metafont multibib collection-fontutils
    epstopdf epspdfconversion epspdf algorithm2e relsize type1cm logreq
    ucs todonotes xargs forloop pbox varwidth bigfoot environ trimspaces
    forest arydshln pgfopts elocalloc libertine mweights fontaxes
    totpages comment soul multirow threeparttable lastpage preprint
    mnsymbol paralist animate media9 ocgx2 breakcites biblatex bibtex
    xstring wrapfig ifoddpage inlinedef draftwatermark everypage
    enumitem cutwin moreverb mparhack bbding thmtools csquotes stmaryrd
    cleveref cancel ncctools changebar tocbibind shadow
    ;
}
