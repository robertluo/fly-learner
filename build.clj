(ns build
  "The whole learner as one jar: `clojure -T:build uber` (`bb jar` builds the
  frontend first). target/fly-learner.jar runs with `java -jar`; the Datalevin
  flags are in its manifest. It reads and writes data/ under the working
  directory, as the dev setup does."
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def jar-file "target/fly-learner.jar")
(def main 'robertluo.fly-learner.server.system)

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber
  "The release jar. Expects the release frontend in resources/public/js and the
  vendored pdf.js and KaTeX (`bb jar` makes both)."
  [_]
  (when-not (.exists (java.io.File. "resources/public/vendor/pdfjs/pdf.min.mjs"))
    (throw (ex-info "No vendored pdf.js: run `bb install` first" {})))
  (clean nil)
  (let [basis (b/create-basis {:project "deps.edn"})]
    (b/copy-dir {:src-dirs ["src/clj" "src/cljc" "resources"] :target-dir class-dir})
    ;; the dev build's files beside the release main.js
    (b/delete {:path (str class-dir "/public/js/cljs-runtime")})
    (b/delete {:path (str class-dir "/public/js/manifest.edn")})
    ;; ahead of time, so the server starts in seconds; the cli for ingesting by hand
    (b/compile-clj {:basis basis :class-dir class-dir
                    :ns-compile [main 'robertluo.fly-learner.cli]
                    :java-opts ["--add-opens=java.base/java.nio=ALL-UNNAMED"
                                "--add-opens=java.base/sun.nio.ch=ALL-UNNAMED"
                                "--enable-native-access=ALL-UNNAMED"]})
    (b/uber {:class-dir class-dir :uber-file jar-file :basis basis :main main
             :manifest {"Add-Opens" "java.base/java.nio java.base/sun.nio.ch"
                        "Enable-Native-Access" "ALL-UNNAMED"}})
    (println "Built" jar-file)))
