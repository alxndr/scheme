(ns twitterbot.core
    )

(defn to-str
  "convenience fn to concat & call str"
  [& seqs]
  (apply str (clojure.core/concat seqs)))

(defn trim-head
  "if text starts with head (optionally surrounded by whitespace), strip it off"
  [& {:keys [text head]}]
  (let [head-regex (re-pattern (to-str "^ *@" head " +"))]
    (clojure.string/replace text head-regex "")))

(defn rewrite-tweet
  "reformat tweet-text by writer which mentions reader"
  [tweet-text writer reader]
  (let [trimmed-tweet (trim-head :text tweet-text :head (to-str reader))]
    (to-str "{@" writer "} " trimmed-tweet)))

(defn -main []
  ; auth with twitter
  ; poll for "@oberlin"
  ; post modified tweet
  )
