(ns robertluo.fly-learner.ingests-test
  "Adding sources from the settings page: zips unpacked safely, what they
  hold, jobs and their logs (the pipeline stubbed: no model)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [robertluo.fly-learner.server.ingests :as ingests]
            [robertluo.fly-learner.server.pipeline :as pipeline])
  (:import (java.util.zip ZipEntry ZipOutputStream)))

(defn- tmp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "fly-ingests-" (random-uuid))) .mkdirs))

(defn- zip! [f entries]
  (with-open [z (ZipOutputStream. (io/output-stream f))]
    (doseq [[n text] entries]
      (.putNextEntry z (ZipEntry. ^String n))
      (.write z (.getBytes ^String text "UTF-8"))
      (.closeEntry z)))
  f)

(defn- delete! [f] (doseq [x (reverse (file-seq (io/file f)))] (.delete ^java.io.File x)))

(deftest zips-unpacked-safely
  (let [dir (tmp-dir)]
    (try
      (testing "a folder of Markdown is one source; books in it are sources of their own"
        (let [out (ingests/unzip! (zip! (io/file dir "a.zip")
                                        {"repo/docs/index.md"      "# Docs"
                                         "repo/docs/guide/one.md"  "# One"
                                         "repo/papers/p.pdf"       "%PDF-"
                                         "__MACOSX/repo/._x.md"    "junk"
                                         "repo/.DS_Store"          "junk"})
                                  (io/file dir "a"))
              paths (ingests/candidates out)]
          (is (= 2 (count paths)))
          (is (re-find #"a/repo/docs$" (first paths)) "the deepest folder holding all the Markdown")
          (is (re-find #"p\.pdf$" (second paths)))
          (is (not (.exists (io/file out "__MACOSX"))))))
      (testing "an entry outside its folder refuses the zip"
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"outside its folder"
                              (ingests/unzip! (zip! (io/file dir "b.zip") {"../../escape.md" "# x"}) (io/file dir "b")))))
      (finally (delete! dir)))))

(deftest a-job-runs-in-the-background-with-its-log
  (with-redefs [pipeline/run! (fn [_ _ paths opts]
                                (println "Ingesting" (first paths))
                                (doseq [i (range 1 4)] (println (str "folded " i "/3")))
                                (print "Done") (flush) (println ".")
                                {:sources [{:source-id :s1}] :cost 0.5 :opts opts})]
    (let [{:ingest/keys [id status]} (ingests/start! nil (delay :client) {:common "m"} {:paths ["x.pdf"] :skip ["Reference"]})]
      (is (#{:queued :running :done} status))
      (loop [n 0] (when (and (< n 100) (not= :done (:ingest/status (ingests/job id)))) (Thread/sleep 20) (recur (inc n))))
      (let [j (ingests/job id)]
        (is (= :done (:ingest/status j)))
        (is (= ["Ingesting x.pdf" "folded 3/3" "Done."] (:ingest/log j)) "a fold's progress replaces the one before")
        (is (= [:s1] (:ingest/sources j)))
        (is (true? (ingests/forget! id)))
        (is (nil? (ingests/job id)))))))
