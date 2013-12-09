(ns twamplifier.core
    (:use
      [twitter.oauth]
      [twitter.utils]
      ))

(defn to-str
  "convenience fn to concat & call str"
  [& seqs]
  (apply str (clojure.core/concat seqs)))

(defn trim-head
  "if text starts with @head, strip it off"
  [& {:keys [text head]}]
  (let [head-regex (re-pattern (to-str "^ *@" head " +"))]
    (clojure.string/replace text head-regex "")))

(defn rewrite-tweet
  "reformat tweet-text by writer which mentions reader"
  [tweet-text writer reader]
  (let [trimmed-tweet (trim-head :text tweet-text :head (to-str reader))]
    (to-str "[@" writer "] " trimmed-tweet)))

(def my-creds
     (make-oauth-creds
       "HM2trIrnxPZozvzBpdLhiw" ;*app-consumer-key*
       "1rLC7gkLsUmlLL5tAwodjiu7bDhFLbaQTN6dn2Igsyw" ;*app-consumer-secret*
       "15149293-eU5cuv7hPc2GOXfs1Jlx7e63SO2OqluBO7ZIyjKTa" ;*user-access-token*
       "CkPIq5ZrsFxHzA87UZSPlsbHaQFUIPLAxnRWGrf7VmnIl" ;*user-access-token-secret*
       ))

(defn go-for-it
  []
  (users-show :oauth-creds my-creds :params {:screen-name "AdamJWynne"})
  )

(defn -main []
  ; auth with twitter
  ; poll for "@oberlin"
  ; post modified tweet
  )
