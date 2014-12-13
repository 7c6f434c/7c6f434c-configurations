{ pkgs ? import <nixpkgs> {},
  tools ? import ./tools.nix {}
  }:
with pkgs // tools;
rec {
  initrdHello = makeBasicInitrd {
    initScript = ''
      #!/init-tools/bin/sh
      ${init-hello}
    '';
  };
  qemuScriptHello = qemuLauncherFun {initrd = initrdHello;};

  initrdHelloSh = makeBasicInitrd {
    initScript = ''
      #!/init-tools/bin/sh
      echo Hello
      /init-tools/bin/sleep 10
    '';
  };
  qemuScriptHelloSh = qemuLauncherFun {initrd = initrdHelloSh;};

  initrdSh = makeOverridable makeUdevInitrd {
    modules = typicalNotebookModules;
    modulesAvailable = typicalNotebookAvailableModules;
  };
  qemuScriptSh = qemuLauncherFun {initrd = initrdSh;};
}
