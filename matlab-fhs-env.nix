with import <nixpkgs> {};
buildFHSUserEnv
{
  name = "matlab-env";
  targetPkgs = p:
  (with p;
  [
    mesa.osmesa aspell portaudio pixman harfbuzz libxml2 qt5.
    qtbase libffi udev coreutils alsaLib dpkg gcc48 freetype glib fontconfig
    openssl which ncurses jdk11 pam dbus_glib dbus pango gtk2-x11
    atk gdk_pixbuf cairo ncurses5 mesa_glu zlib libglvnd
  ])
  ++ 
  (with p;
  [
    libX11
    libXcursor
    libXrandr
    libXext
    libSM
    libICE
    libX11
    libXrandr
    libXdamage
    libXrender
    libXfixes
    libXcomposite
    libXcursor
    libxcb
    libXi
    libXScrnSaver
    libXtst
    libXt
    libXxf86vm
    libXpm
    libXp
    libXmu
  ]);
}
