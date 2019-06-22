(ns sme-classifier.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [pl.danieljanus.tagsoup :as tagsoup :refer [tag attributes children parse]])
  (:import [cdb query]))

;=== helper functions ================

(defn content
  "Return the content of the element (without html elements)."
  [e]
  (string/join
    (filter string? (rest (rest e)))))

(defn tag?
  "Return whether the specified html is (just one) element."
  [html]
  (keyword? (first html)))

(defn get-all
  "Returns seq of all elements satisfying the specified predicate."
  [html pred]
  (if (keyword? (first html))
    (get-all (seq (vector html)) pred)
    (when (seq html)
      (concat (filter pred html)
              (get-all (mapcat #(rest (rest %)) (remove string? html)) pred)))))

(defn get-classes
  "Returns seq of all elements having the specified class."
  [html c]
  (get-all html #(= c (:class (second %)))))

(defn get-ids
  "Returns seq of all elements having the specified id."
  [html i]
  (get-all html #(= i (:id (second %)))))

(defn get-elements
  "Returns seq of all elements having the specified element
  (must be keyword - e.g. :div, :table, ...)."
  [html e]
  (get-all html #(= e (first %))))

(defn get-attributes
  "Returns seq of all elements having the attribute with the specified value
  (attr must be keyword)."
  ([html attr]
    (get-all html #(not (nil? (attr (second %))))))
  ([html attr value]
    (get-all html #(= value (attr (second %))))))

;=== extracting functions ================

;urls and names of TOCs of article categories
(def domace {:url "http://www.sme.sk/rubrika.asp?rub=online_zdom&st=" :name "domace"})
(def zahranicne {:url "http://www.sme.sk/rubrika.asp?rub=online_zahr&st=" :name "zahranicne"})
(def kultura {:url "http://kultura.sme.sk/hs/?st=" :name "kultura"})
(def sport {:url "http://sport.sme.sk/hs/?st=" :name "sport"})
(def pocitace {:url "http://pocitace.sme.sk/hs/?st=" :name "pocitace"})
(def veda {:url "http://veda.sme.sk/hs/?st=" :name "veda"})

(def all [domace zahranicne kultura #_ sport #_ pocitace #_ veda])

(defn baseaddr
  "Returns the base address of the html page."
  [html]
  (or (-> html (get-ids "headerw") (get-elements :a) first attributes :href)
      "http://sme.sk"))

(defn article-links
  "Returns the seq containing urls of the articles from the given TOC page."
  [html]
  (let [baseaddr (baseaddr html)]
    (map (fn [u] (if (.startsWith u "/c/") (str baseaddr u) u))
         (remove #(or (= baseaddr %)
                      (.startsWith % "/r/")
                      (.startsWith % (str baseaddr "/r/")))
                  (map #(-> % attributes :href) (-> html (get-ids "contentw") (get-elements :h3) (get-elements :a)))))))

(defn under-piano?
  "Returns whether this article is locked under the piano paywall."
  [html]
  (not (empty? (get-classes html "art-perex-piano"))))

(defn title
  "Returns the title string of the article."
  [html]
  (-> html (get-elements :h1) first content string/trim))

(defn perex
  "Returns the perex string of the article."
  [html]
  (-> html (get-ids "itext_content") (get-elements :strong) first content))

(defn text
  "Returns the text string of the article."
  [html]
  (string/join (map content (-> html (get-ids "itext_content") (get-elements :p)))))

(defn download
  "Downloads articles from various categories, crawling the specified range
  of pages of TOC given by the specified category.
  Category is the map with keys :url and :name. Returns seq of maps,
  representing the articles and containing the keys :title, :perex, :text,
  :url and :category."
  ([category]
    (download category 50))
  ([category to]
    (download category 1 to))
  ([category from to]
    (doall
      (let [nums (range from (inc to))]
        (apply concat
          (for [id nums]
            (let [toc-url (str (:url category) id)
                  links (article-links (parse toc-url))]
              (keep identity (for [article-url links]
                                (do (println "parsing" article-url "...")
                                 (let [article (parse article-url)]
                                   (when-not (under-piano? article)
                                     (-> (zipmap [:title :perex :text] ((juxt title perex text) article))
                                       (assoc :url article-url)
                                       (assoc :category (:name category)))))))))))))))

(defn persist
  "Persists value to the given file."
  [file value]
  (spit file value))

(defn read-back
  "Reads persisted value from the given file and returns it."
  [file]
  (binding [*read-eval* false]
    (read-string (slurp file))))

(defn download-articles
  ""
  []
  (doseq [cat all]
    (let [filename (str "../articles/sme-" (:name cat) ".map")]
      (persist filename (into [] (download cat 1))))))

;=== text processing functions ================

(defn lemmatize
  "Returns the lemma of the given slovak word usng the specified database,
  or nil if lemma not found."
  ([word]
    (lemmatize (.getPath (io/resource "form2lemma.cdb")) word))
  ([db-path word]
    (let [lemma (query/get db-path word)]
      (when-not (empty? lemma)
        lemma))))

(defn prefix
  "Returns the prefix and the rest of the word, or nil if prefix not found."
  [prefix word]
  (next (re-find (re-pattern (str "^(" prefix ")(.+)")) word)))

(defn split-prefixes
  "Returns vector of all possible pairs of [prefix rest-of-word]."
  ([word]
    (split-prefixes word ["bez" "cez" "do" "na" "nad" "naj" "ne" "o" "ob" "od"
                         "po" "pod" "pre" "pred" "pri" "proti" "roz" "spolu"
                         "u" "vy" "vz" "za"]))
  ([word prefixes]
    (loop [found [] todo [["" word]]]
      (if (empty? todo)
        found
        (let [[pref body] (first todo)
              matches (keep #(prefix % body) prefixes)]
          (if-not (empty? matches)
            (let [matched (mapv vector (mapv #(str pref (first %)) matches) (mapv second matches))]
              (recur (into found matched) (into (rest todo) matched)))
              (recur found (rest todo))))))))

(defn lemmatize-with-prefixes
  "Like lemmatize, but also deals with common verb prefixes."
  [word]
  (if-let [lemmatized (lemmatize word)]
    lemmatized
    (let [pref-lemmatize (fn [[p w]] (when-let [l (lemmatize w)] (str p l)))]
      (loop [to-try (split-prefixes word)]
        (when-not (empty? to-try)
          (or (pref-lemmatize (first to-try))
              (recur (rest to-try))))))))

(comment
(def y (let [x (->>
          (read-back "../articles/large/sme-veda.map")
          (map :text)
          string/join
          (.toLowerCase))
      s (reduce #(string/replace %1 %2 " ") x ["." "-" "," ")" "(" "\""])]
  (re-seq #"[^ ]+" s)))


;(comment

(def g (doall (map #(or (lemmatize %) (str % "---")) y)))
(pprint (keep #(and (re-find #"---" %) %) g))
(def non (filter #(re-find #"---" %) g))
)

;=== naive bayes ================

(defn init-bayes
  "articles is seq of maps with keys :tokens and :category."
  [articles]
  (loop [articles-total 0
         categories {}
         words {}
         words-per-cat {}
         todo articles]
    (if-not (seq todo)
      {:articles-total articles-total
       :categories categories
       :words words
       :words-per-cat words-per-cat
       :vocab-size (count (set (mapcat keys (vals words))))}
      (let [{:keys [category tokens]} (first todo)]
        (recur (inc articles-total)
               (update-in categories [category] (fnil inc 0))
               (reduce #(update-in %1 [category %2] (fnil inc 0)) words tokens)
               (update-in words-per-cat [category] (fnil (partial + (count tokens)) 0))
               (rest todo))))))

(defn predict-bayes
  "init is output of init-bayes and tokens is sequence of strings."
  [tokens init]
  (let [{:keys [articles-total categories words words-per-cat vocab-size]} init]
    (key (apply max-key val
           (zipmap (keys categories)
             (for [c (keys categories)]
               (double
                 (reduce *
                   (/ (categories c) articles-total)
                   (map #(/ (+ 1 (get-in words [c %] 0))
                            (+ vocab-size (words-per-cat c)))
                         tokens)))))))))

(defn predict-bayes-log
  "init is output of init-bayes and tokens is sequence of strings."
  [tokens init]
  (let [{:keys [articles-total categories words words-per-cat vocab-size]} init]
    (key (apply max-key val
           (zipmap (keys categories)
             (for [c (keys categories)]
                 (reduce +
                   (Math/log (/ (categories c) articles-total))
                   (map #(Math/log (/ (+ 1 (get-in words [c %] 0))
                                      (+ vocab-size (words-per-cat c))))
                         tokens))))))))

;=== sample set processing functions ================

(defn load-samples
  ""
  []
  (let [sport (read-back "../articles/large/sme-sport.map")
        domace (read-back "../articles/large/sme-domace.map")
        kultura (read-back "../articles/large/sme-kultura.map")
        zahranicne (read-back "../articles/large/sme-zahranicne.map")
        veda (read-back "../articles/large/sme-veda.map")
        pocitace (read-back "../articles/large/sme-pocitace.map")]
    (shuffle (concat sport domace kultura zahranicne veda pocitace))))

(defn partition-samples
  "samples is vector."
  [samples train cv test]
  {:pre [(== 1 (+ train cv test)) (vector? samples)]}
  (let [size (count samples)
        train-from 0
        train-to   (int (* train size))
        cv-from    (inc train-to)
        cv-to      (+ (int (* cv size)) cv-from)
        test-from  (inc cv-to)
        test-to    size]
    {:train (subvec samples train-from train-to)
     :cv    (subvec samples cv-from    cv-to)
     :test  (subvec samples test-from  test-to)}))

(defn save-sets
  ""
  ([partitions]
    (save-sets partitions ""))
  ([partitions prefix]
    (persist (str "../articles/samples/sets/" prefix "train.vec") (:train partitions))
    (persist (str "../articles/samples/sets/" prefix "cv.vec") (:cv partitions))
    (persist (str "../articles/samples/sets/" prefix "test.vec") (:test partitions))))

(defn load-sets
  ""
  ([]
    (load-sets ""))
  ([prefix]
    {:train (read-back (str "../articles/samples/sets/" prefix "train.vec"))
     :cv    (read-back (str "../articles/samples/sets/" prefix "cv.vec"))
     :test  (read-back (str "../articles/samples/sets/" prefix "test.vec"))}))

(defn prepare-samples
  "tokenize-f has 3 arguments: title, perex and text.
  lemmatize-fn has one argument - the word."
  [samples tokenize-fn lemmatize-fn]
  (->>
    samples
    (mapv #(assoc % :tokens (map (fn [w] (or (lemmatize-fn w) w))
                                 (tokenize-fn (:title %) (:perex %) (:text %)))))
    (mapv #(select-keys % [:category :tokens]))))

(defn tokenize
  ""
  [text]
  (remove empty?
          (-> text
              (.toLowerCase)
              (string/split (re-pattern "[-–−….,‚?!;:/\\\\_()<>\"'\\s ­ „“”‘’`´§%°‡˙²〜\u200b\ufeff€#@&=~⋌‰±×®\\[\\]\\{\\}\\$\\^\\*\\+\\|]")))))

(defn tokenize-title
  ""
  [title perex text]
  (tokenize title))

(defn tokenize-title-perex
  ""
  [title perex text]
  (tokenize (string/join \space [title perex])))

(defn tokenize-all
  ""
  [title perex text]
  (tokenize (string/join \space [title perex text])))

(defn remove-short
  "Removes all tokens with lower-or-equal then n characters."
  [tokens n]
  (remove #(<= (.length %) n) tokens))

(defn load-stopwords
  ""
  ([]
    (load-stopwords ""))
  ([prefix]
    (read-back (str "../articles/samples/" prefix "stopwords.vec"))))

(defn remove-stopwords
  "Removes all tokens that are stopwords."
  [tokens stopwords]
  (remove #(some #{%} stopwords) tokens))

(defn remove-numbers
  "Removes all tokens that are numbers."
  [tokens]
  (remove #(re-matches (re-pattern "[-\\d\\.,]+") %) tokens))

;=== training and evaluation ================

(defn prepare-and-evaluate
  "tokenize-f has 3 arguments: title, perex and text.
  lemmatize-fn has one argument - the word."
  [samples tokenize-fn lemmatize-fn]
  (let [prepared (prepare-samples samples tokenize-fn lemmatize-fn)
        {:keys [train cv test]} (partition-samples prepared 0.6 0.2 0.2)
        trained (init-bayes train)
        predicted (map #(hash-map :actual (:category %) :predicted (predict-bayes-log (:tokens %) trained)) test)
        right (count (filter #(= (:actual %) (:predicted %)) predicted))]
    {:accuracy (double (/ right (count predicted)))}))

(defn prepare-and-save
  ""
  ([samples tokenize-fn lemmatize-fn]
    (prepare-and-save samples tokenize-fn lemmatize-fn ""))
  ([samples tokenize-fn lemmatize-fn prefix]
    (let [prepared (prepare-samples samples tokenize-fn lemmatize-fn)
          {:keys [train cv test] :as sets} (partition-samples prepared 0.6 0.2 0.2)
          trained (init-bayes train)]
      (save-sets sets prefix))))

(defn evaluate
  "tokenize-f has 3 arguments: title, perex and text.
  lemmatize-fn has one argument - the word."
  [train test]
  (let [trained (init-bayes train)
        predicted (map #(hash-map :actual (:category %) :predicted (predict-bayes-log (:tokens %) trained)) test)
        right (count (filter #(= (:actual %) (:predicted %)) predicted))]
    {:accuracy (double (/ right (count predicted)))}))

;=== the main function ================

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))



