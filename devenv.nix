{ pkgs, ... }:
{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.ghcid
    pkgs.git
    pkgs.gitleaks
    pkgs.jdk19
    # Not using pkgs.leiningen because libpython-clj doesn't seem to support JDK 21
  ];

  # https://devenv.sh/scripts/
  scripts.hello.exec = "echo hello from $GREET";
  scripts.build.exec = ''
    cd "$DEVENV_ROOT/clj"
    ${pkgs.leiningen}/bin/lein deps
    cd "$DEVENV_ROOT/hs"
    ${pkgs.haskellPackages.stack}/bin/stack build --fast
  '';
  scripts.reddit.exec = ''
    cd "$DEVENV_ROOT/hs"
    DIST_DIR_PATH=$(${pkgs.haskellPackages.stack}/bin/stack path --dist-dir)
    REDDIT_EXECUTABLE="$DIST_DIR_PATH/build/reddit/reddit"
    "$REDDIT_EXECUTABLE" $@
  '';
  scripts.run.exec = ''
    cd "$DEVENV_ROOT/hs"
    ${pkgs.ghcid}/bin/ghcid --command="${pkgs.stack}/bin/stack ghci" -T="main" --warnings
  '';

  # TODO: Incorporate dependency installation via enterShell

  # https://devenv.sh/languages/
  # languages.nix.enable = true;
  languages.haskell.enable = true;
  languages.python = {
    enable = true;
    poetry.enable = true;
    poetry.activate.enable = true;
  };

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;
  pre-commit.hooks = {
    gitleaks = {
      enable = true;
      # https://github.com/gitleaks/gitleaks/blob/8de8938ad425d11edb0986c38890116525a36035/.pre-commit-hooks.yaml#L4C10-L4C54
      entry = "${pkgs.gitleaks}/bin/gitleaks protect --verbose --redact --staged";
    };
    nixpkgs-fmt.enable = true;
    ormolu.enable = true;
    prettier.enable = true;
    shellcheck = {
      enable = true;
      # https://github.com/cachix/pre-commit-hooks.nix/issues/105
      types_or = [ "shell" ];
    };
    # https://github.com/cachix/pre-commit-hooks.nix/issues/31#issuecomment-744657870
    trailing-whitespace = {
      enable = true;
      # https://github.com/pre-commit/pre-commit-hooks/blob/4b863f127224b6d92a88ada20d28a4878bf4535d/.pre-commit-hooks.yaml#L201-L207
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/trailing-whitespace-fixer";
      types = [ "text" ];
    };
  };

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";

  # See full reference at https://devenv.sh/reference/options/
}
