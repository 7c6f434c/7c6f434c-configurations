#! /bin/sh

echo -e 'This should use a local expression microrepository to build a new package and an inherited one. Then this test will run a small test of built packages.\n Press Enter..' | fmt -w 72 
read

mkdir /tmp/test-local-nix-expression || true

nix-build ./local-packages.nix -A testPackage -o /tmp/test-local-nix-expression/echoPi
nix-build ./local-packages.nix -A bc -o /tmp/test-local-nix-expression/testBc

/tmp/test-local-nix-expression/echoPi
echo -e 'scale=1\n e(1)\n quit' | /tmp/test-local-nix-expression/testBc/bin/bc -l

rm -r /tmp/test-local-nix-expression 
