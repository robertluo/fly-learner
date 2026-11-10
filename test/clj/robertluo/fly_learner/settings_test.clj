(ns robertluo.fly-learner.settings-test
  "Model settings edited in the app, over config.edn: saved, in force, keys
  kept apart and never shown."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [robertluo.fly-learner.server.settings :as settings]))

(deftest settings-edited-in-the-app
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir") (str "fly-settings-" (random-uuid))) .mkdirs)
        cfg {:llm {:providers {:anthropic {:type :anthropic :keychain {:service "nope" :account "nope"}}
                               :local     {:type :openai :base-url "http://localhost:8000/v1" :body {:temperature 0.2}}}
                   :default   :anthropic
                   :models    {:strong "claude-sonnet-5-5" :common "local/q"}}}]
    (with-redefs [settings/settings-file (str (io/file dir "settings.edn"))
                  settings/secrets-file  (str (io/file dir "secrets.edn"))]
      (try
        (testing "before any edit: config.edn's"
          (is (= {:strong "claude-sonnet-5-5" :common "local/q"} (:models (settings/view cfg))))
          (is (false? (:saved? (settings/view cfg)))))
        (let [v (settings/save! cfg {:providers [{:name "anthropic" :type :anthropic :api-key "  sk-ant-secret-9876 "}
                                                 {:name "local" :type :openai :base-url "http://localhost:8000/v1"
                                                  :concurrency 4 :api-key "omlx-key-1234"}]
                                     :default   "anthropic"
                                     :models    {:strong "anthropic/claude-sonnet-5-5" :common "local/qwen"}})]
          (testing "the page sees where a key comes from, never the key"
            (is (= {:from :saved :hint "…9876"} (:key (first (:providers v)))))
            (is (not (re-find #"sk-ant-secret|omlx-key" (pr-str v)))))
          (testing "in force: the key typed wins over the keychain; what the page does not edit is kept"
            (let [{:keys [providers models]} (settings/llm cfg)]
              (is (= "sk-ant-secret-9876" (get-in providers [:anthropic :api-key])))
              (is (= {:temperature 0.2} (get-in providers [:local :body])))
              (is (= 4 (get-in providers [:local :concurrency])))
              (is (= "local/qwen" (:common models)))))
          (testing "keys kept apart, readable by their owner only"
            (is (not (re-find #"secret" (slurp settings/settings-file))))
            (is (= "rw-------" (java.nio.file.attribute.PosixFilePermissions/toString
                                (java.nio.file.Files/getPosixFilePermissions
                                 (.toPath (io/file settings/secrets-file)) (make-array java.nio.file.LinkOption 0)))))))
        (testing "a key forgotten; a provider dropped takes its key along"
          (settings/save! cfg {:providers [{:name "anthropic" :type :anthropic :forget-key? true}]
                               :default "anthropic" :models {:strong "x" :common "y"}})
          (is (= {} (read-string (slurp settings/secrets-file)))))
        (testing "an edit that cannot work is refused"
          (is (= {:error "The default provider must be one of them"}
                 (settings/save! cfg {:providers [{:name "a" :type :openai}] :default "b" :models {:strong "x" :common "y"}}))))
        (finally (doseq [f (reverse (file-seq dir))] (.delete ^java.io.File f)))))))
