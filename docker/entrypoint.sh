#!/bin/sh
# Run the learner as `ubuntu`, not root. A new volume may come root's (Apple's
# container, some bind mounts): started as root, give /app/data to `ubuntu` first.
set -e
if [ "$(id -u)" = 0 ]; then
  [ "$(stat -c %U /app/data)" = ubuntu ] || chown -R ubuntu:ubuntu /app/data
  exec setpriv --reuid=ubuntu --regid=ubuntu --init-groups java -jar /app/fly-learner.jar "$@"
fi
exec java -jar /app/fly-learner.jar "$@"
