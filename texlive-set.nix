pkgs:
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
