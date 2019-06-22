(defproject sme-classifier "0.1.0-SNAPSHOT"
  :description "Simple downloader and classifier of articles from sme.sk"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [clj-tagsoup "0.3.0"]]
  :main sme-classifier.core
  :source-paths ["src/clojure"]
  :java-source-paths ["src/java"]
  :resource-paths ["src/resources"])
