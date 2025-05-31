{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
{
  # https://devenv.sh/packages/
  packages = [
    pkgs.gnumake
    pkgs.nodejs
    pkgs.python3
    pkgs.poetry
    pkgs.rustc
  ];

  # https://devenv.sh/languages/
  languages = {
    javascript.enable = true;
    nix.enable = true;
    python.enable = true;
    python.poetry.enable = true;
    typescript.enable = true;
  };

  # https://devenv.sh/git-hooks/
  git-hooks.hooks = {
    actionlint.enable = true;
    eslint = {
      enable = true;
      settings.extensions = "\.(js|mjs|tsx?)$";
    };
    nixfmt-rfc-style.enable = true;
    ruff-format.enable = true;
    yamlfmt = {
      enable = true;
      settings.lint-only = false;
    };
    shellcheck.enable = true;
    taplo.enable = true;
  };

  # See full reference at https://devenv.sh/reference/options/
}
