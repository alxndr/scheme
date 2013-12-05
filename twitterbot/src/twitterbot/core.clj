(ns twitterbot.core)

(defn trim-tweet-text [tweet-text]
  (if (re-find #"^\s*@oberlin\s+" tweet-text)
    (clojure.string/replace tweet-text #"^\s*@oberlin\s+" "")
    tweet-text
  )
)

(defn rewrite-tweet [tweet-text username]
  ; if starts with "@oberlin ", strip it off
  (apply str (clojure.core/concat
               "{" username "} "
               (trim-tweet-text tweet-text))))

(defn -main []
  ; auth with twitter
  ; poll for "@oberlin"
  ; post modified tweet
)
