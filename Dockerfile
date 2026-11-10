# The whole learner as an OCI image (docker, podman, Apple's container):
#
#   docker build -t fly-learner .            # or: bb image
#   docker run -d -p 8080:8080 -v fly-learner:/app/data fly-learner
#
# Everything it keeps (the db, settings and keys, uploaded sources, your prompts) is
# under /app/data: keep that volume and an upgrade is a new image on the same volume.

# --- build: the jar, the same on every platform, so it is built once on the builder's
FROM --platform=$BUILDPLATFORM node:22-bookworm-slim AS node
FROM --platform=$BUILDPLATFORM babashka/babashka:1.13.225 AS bb

FROM --platform=$BUILDPLATFORM clojure:temurin-21-tools-deps-bookworm-slim AS build
COPY --from=node /usr/local/bin/node /usr/local/bin/node
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s ../lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
 && ln -s ../lib/node_modules/npm/bin/npx-cli.js /usr/local/bin/npx
COPY --from=bb /usr/local/bin/bb /usr/local/bin/bb

WORKDIR /src
# the dependencies first: a change to the code alone reuses this layer
COPY deps.edn bb.edn shadow-cljs.edn package.json package-lock.json build.clj ./
COPY bb ./bb
RUN npm ci && clojure -P -M:cljs && clojure -P -T:build

COPY src ./src
COPY resources ./resources
RUN bb jar

# --- run: a JRE on glibc (Datalevin's native library needs glibc 2.34+, so no Alpine)
FROM eclipse-temurin:21-jre-noble
WORKDIR /app
RUN mkdir -p /app/data && chown ubuntu:ubuntu /app/data
COPY --from=build /src/target/fly-learner.jar /app/fly-learner.jar
# starts as root only to hand a new volume to `ubuntu`, then runs the app as it
COPY --chmod=755 docker/entrypoint.sh /app/entrypoint.sh
# Datalevin's flags (in the jar's manifest too) for any `java` run here, e.g. the cli:
#   docker exec fly-learner java -cp /app/fly-learner.jar clojure.main -m robertluo.fly-learner.cli --outline <path>
ENV JDK_JAVA_OPTIONS="--add-opens=java.base/java.nio=ALL-UNNAMED --add-opens=java.base/sun.nio.ch=ALL-UNNAMED --enable-native-access=ALL-UNNAMED -XX:MaxRAMPercentage=75"
VOLUME /app/data
EXPOSE 8080
ENTRYPOINT ["/app/entrypoint.sh"]
