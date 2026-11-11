#!/bin/sh
# Run the learner as `ubuntu`, not root. A new volume may come root's (Apple's
# container, some bind mounts): started as root, give /app/data to `ubuntu` first.
# A folder shared from the host may refuse the chown (macOS: it keeps its owner, and
# the share gives access as the host's user), so what decides is whether `ubuntu`
# can write there; if it cannot, the app runs as root rather than not at all.
set -e
jar=/app/fly-learner.jar
if [ "$(id -u)" = 0 ]; then
  as_ubuntu="setpriv --reuid=ubuntu --regid=ubuntu --init-groups"
  [ "$(stat -c %U /app/data)" = ubuntu ] || chown -R ubuntu:ubuntu /app/data 2>/dev/null || true
  if $as_ubuntu test -w /app/data; then
    exec $as_ubuntu java -jar "$jar" "$@"
  fi
  echo "fly-learner: /app/data is not writable by ubuntu, running as root" >&2
fi
exec java -jar "$jar" "$@"
