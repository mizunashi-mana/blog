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
    nix.enable = true;
  };

  # https://devenv.sh/git-hooks/
  git-hooks.hooks = {
    actionlint.enable = true;
    nixfmt-rfc-style.enable = true;
    shellcheck.enable = true;
    taplo.enable = true;
    yamlfmt = {
      enable = true;
      settings.lint-only = false;
    };

    # custom hooks
    eslint-npm = {
      enable = true;
      entry = "npx eslint --fix";
      files = "\\.(js|mjs|ts|tsx)$";
    };
    prettier-npm = {
      enable = true;
      entry = "npx prettier --write";
      files = "\\.(css|scss|sass|less)$";
    };
    ruff-format-poetry = {
      enable = true;
      entry = "poetry run ruff format";
      types = [ "python" ];
    };
    stylelint-npm = {
      enable = true;
      entry = "npx stylelint --fix";
      files = "\\.(css|scss|sass|less)$";
    };
  };

  # See full reference at https://devenv.sh/reference/options/
}
