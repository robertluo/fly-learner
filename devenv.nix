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

  # The learner in Apple's container on this project's data/ (the image: `bb image`, or
  # the published one). Datalevin wants one writer: not while `bb dev` holds data/.
  scripts.fly-up = {
    description = "Run the learner in a container on data/ (http://localhost:8080): fly-up [image]";
    exec = ''
      set -euo pipefail
      cd "$DEVENV_ROOT"
      image="''${1:-''${FLY_IMAGE:-ghcr.io/robertluo/fly-learner:latest}}"
      port="''${FLY_PORT:-8080}"
      if [ -f .nrepl-port ] && (exec 3<>"/dev/tcp/localhost/$(cat .nrepl-port)") 2>/dev/null; then
        echo "fly-up: the dev REPL (bb dev) has data/ open; stop it first (one writer)" >&2
        exit 1
      fi
      container system status >/dev/null 2>&1 || container system start --enable-kernel-install
      if container ls -q | grep -qx fly-learner; then
        echo "fly-learner is already running (fly-down stops it)"
        exit 0
      fi
      container rm fly-learner >/dev/null 2>&1 || true
      # the key: the environment's, else the keychain's (no keychain inside the container)
      if [ -z "''${ANTHROPIC_API_KEY:-}" ]; then
        ANTHROPIC_API_KEY="$(security find-generic-password -s learner -a anthropic -w 2>/dev/null || true)"
      fi
      export ANTHROPIC_API_KEY
      keyopt=()
      [ -n "$ANTHROPIC_API_KEY" ] && keyopt=(-e ANTHROPIC_API_KEY)
      container run -d --progress none --name fly-learner -p "$port:8080" -m "''${FLY_MEMORY:-4G}" \
        -v "$DEVENV_ROOT/data:/app/data" "''${keyopt[@]}" "$image" >/dev/null
      for _ in $(seq 1 60); do
        if curl -s -o /dev/null "http://localhost:$port/"; then
          echo "fly-learner: http://localhost:$port ($image; fly-logs, fly-down)"
          exit 0
        fi
        container ls -q | grep -qx fly-learner || break
        sleep 1
      done
      echo "fly-up: it did not come up; its log:" >&2
      container logs -n 30 fly-learner >&2
      exit 1
    '';
  };
  scripts.fly-down = {
    description = "Stop the learner's container (fly-up)";
    exec = ''
      container stop fly-learner >/dev/null 2>&1 || true
      container rm fly-learner >/dev/null 2>&1 && echo "fly-learner stopped" || echo "fly-learner is not running"
    '';
  };
  scripts.fly-logs = {
    description = "Follow the learner container's log";
    exec = ''exec container logs -f fly-learner'';
  };

  # `devenv up`: nREPL (backend + shadow-cljs middleware); start things from the REPL.
  processes.repl.exec = "bb dev";

  enterShell = ''
    clj-nrepl-eval --help >/dev/null 2>&1 || echo "warning: could not fetch clojure-mcp-light"
  '';

  enterTest = ''
    bb test
  '';
}
