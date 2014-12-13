{pkgs ? (import <nixpkgs> {})}:
with pkgs;
pkgs // 
rec {
  glibSmall = lib.overrideDerivation (glib.override {
    perl = null;
  }) (x: {postInstall = (x.postInstall or "") +''
    rm "$out/bin"/{gdbus-codegen,gtester-report}
  '';});
  vimTiny = vimNox.override{
    source = "vim-nox";
    features = "tiny";
    config = {
      vim = {
        lua = false;
        ruby = false;
        perl = false;
        python = false;
        tcl = false;
        multibyte = true;
        cscope = false;
      };
    };
    libX11 = null;
    gtk = null;
    glib = null;
    flags = ["noX11"];
    gui = "none";
  };
  eudevSmall = eudev.override{
    glib = glibSmall;
  };
  lvm2Small = lvm2.override {
    udev = eudevSmall;
  };
}
