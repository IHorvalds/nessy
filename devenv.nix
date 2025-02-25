{ pkgs, config, ... }:
{
  env.GREET = "Welcome to NES!";
  cachix.enable = false;
  enterShell = ''
    echo ${config.env.GREET}
  '';
}
