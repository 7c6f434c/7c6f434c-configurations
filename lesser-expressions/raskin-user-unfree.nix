with import ./env-defs.nix;

linkFarm "raskin-packages" ([
                { name = "flashplayer-standalone" ; path = callPackage ./flashplayer.nix {};}
		]
		++ 
		(map justUse [
		"tptp"
                "vampire"
		])
		)
