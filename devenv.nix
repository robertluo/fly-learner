{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

let
  # https://github.com/bhauman/clojure-mcp-light, run from a babashka git dep.
  # First run fetches the dep into ~/.gitlibs; later runs use the cached classpath.
  clojure-mcp-light = {
    tag = "v0.2.2";
    sha = "cf48cc6ab1d79809a97a74355492d44ee3bbc4ba";
  };
  cmlDeps = ''{:deps {io.github.bhauman/clojure-mcp-light {:git/tag "${clojure-mcp-light.tag}" :git/sha "${clojure-mcp-light.sha}"}}}'';
  # Absolute store paths so the scripts also work outside `devenv shell`
  # (Claude Code hooks call them via .devenv/profile/bin).
  cmlScript = description: main: {
    inherit description;
    exec = ''
      export PATH="${lib.makeBinPath [ pkgs.git pkgs.parinfer-rust pkgs.jdk21 ]}:$PATH"
      exec ${lib.getExe pkgs.babashka} -Sdeps '${cmlDeps}' -m ${main} "$@"
    '';
  };
in
{
  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.babashka # bb tasks
    pkgs.clj-kondo
  ];

  # https://devenv.sh/languages/
  languages.java.enable = true;
  languages.java.jdk.package = pkgs.jdk21;
  languages.clojure.enable = true;
  languages.javascript.enable = true; # shadow-cljs
  languages.javascript.npm.enable = true;

  # clojure-mcp-light CLI tools
  scripts.clj-paren-repair-claude-hook = cmlScript "Claude Code hook: fix Clojure delimiters on Write/Edit" "clojure-mcp-light.hook";
  scripts.clj-nrepl-eval = cmlScript "Evaluate Clojure via nREPL from the command line" "clojure-mcp-light.nrepl-eval";
  scripts.clj-paren-repair = cmlScript "Fix Clojure delimiters in files on demand" "clojure-mcp-light.paren-repair";

  # `devenv up` starts the backend nREPL and the shadow-cljs watcher.
  processes.repl.exec = "clojure -M:dev:nrepl";
  processes.cljs.exec = "npx shadow-cljs watch app";

  enterShell = ''
    clj-nrepl-eval --help >/dev/null 2>&1 || echo "warning: could not fetch clojure-mcp-light"
  '';

  enterTest = ''
    clojure -M:test
  '';
}
