{ 
  pkgs ? import <nixpkgs> {}, firefox ? p: p.firefox, firefoxName ? "firefox"
  , marionette ? p: p.python2Packages.marionette-harness
  , profileContent ? null
  , baseProfile ? import ./firefox-profile.nix { inherit pkgs firefox; finalContent = profileContent; }
  , name ? "firefox-launcher"
}:
rec {
  socatCmd = (pkgs.lib.getBin pkgs.socat) + "/bin/socat";
  unshareCmd = (pkgs.lib.getBin pkgs.utillinux) + "/bin/unshare";
  firefoxCmd = (pkgs.lib.getBin pkgs.firefox) + "/bin/" + firefoxName;
  unionfsCmd = (pkgs.lib.getBin pkgs.unionfs-fuse) + "/bin/unionfs";
  fuserCmd = (pkgs.lib.getBin pkgs.psmisc) + "/bin/fuser";
  marionette_ = if builtins.isFunction marionette then marionette pkgs else marionette;
  marionetteEnv = pkgs.runCommand "marionette-env" { buildInputs = [ marionette_ ]; } ''
    echo "export PYTHONPATH='$PYTHONPATH'; export PATH='$PATH'" > "$out"
  '';
  marionettePythonPrologue = ''
    from marionette_driver.marionette import Marionette;
    import os;
    session = Marionette();
    session.port = int(os.getenv("MARIONETTE_PORT" or 2828));
    session.start_session();
  '';
  combineProfileScript = ''
    if test -n "$FIREFOX_PROFILE"; then
      _FIREFOX_PROFILE="$FIREFOX_PROFILE"
    else
      _FIREFOX_PROFILE="$(mktemp -d)"
    fi
    test -n "${baseProfile}" && yes n | cp -riT "${baseProfile}" "$_FIREFOX_PROFILE"
  '';
  homeScript = ''
    if test -z "$HOME"; then
      HOME="$(mktemp -d)"
      _HOME_KILL="$HOME"
    else
      _HOME_KILL=
    fi;
  '';
  marionetteScript = ''
    if test -n "$MARIONETTE_SOCKET"; then
      export MARIONETTE_PORT="''${MARIONETTE_PORT:-2828}"
      (
        source "${marionetteEnv}"
        socat unix-listen:"$MARIONETTE_SOCKET",forever,fork exec:'python -c '"'"'${marionettePythonPrologue}'"'"';'
      )
    fi
  '';
  cleanupScript = ''
    if test -n "$_HOME_KILL"; then
      rm -rf "$_HOME_KILL"
    fi
    if test -n "$MARIONETTE_SOCKET"; then
      "${fuserCmd}" "$MARIONETTE_SOCKET"
      rm -f "$MARIONETTE_SOCKET"
    fi
  '';
  firefoxProfileCombiner = pkgs.writeScriptBin "combine-firefox-profile" ''
    export FIREFOX_PROFILE="$1"
    ${combineProfileScript}
    echo "$_FIREFOX_PROFILE"
  '';
  firefoxLauncher = pkgs.writeScriptBin name ''
    ${homeScript}
    ${marionetteScript}
    echo "$FIREFOX_EXTRA_PREFS" >> "$FIREFOX_PROFILE/prefs.js"
    if test -n "$MARIONETTE_PORT"; then
      sed -e '/marionette[.]defaultPrefs[.]port/d' -i "$FIREFOX_PROFILE/prefs.js"
      echo "user_pref(\"marionette.defaultPrefs.port\",\"$MARIONETTE_PORT\");" >> "$FIREFOX_PROFILE/prefs.js"
      "${firefoxCmd}" --profile "$FIREFOX_PROFILE" --new-instance --marionette "$@"
    else
      "${firefoxCmd}" --profile "$FIREFOX_PROFILE" --new-instance "$@"
    fi
    ${cleanupScript}
  '';
  firefoxScripts = pkgs.runCommand "firefox-scripts" {} ''
    mkdir -p "$out/bin"
    ln -s "${firefoxProfileCombiner}"/bin/* "$out/bin"
    ln -s "${firefoxLauncher}"/bin/* "$out/bin"
  '';
}
