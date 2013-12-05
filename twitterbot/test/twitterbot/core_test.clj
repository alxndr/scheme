(ns twitterbot.core-test
  (:require [clojure.test :refer :all]
            [twitterbot.core :refer :all]))

(deftest rewrite-tweet-test

  (testing
    "prepends writer's name"
    (is (= "{@writer} foo bar"
           (rewrite-tweet "@reader foo bar" "writer" "reader"))))

  (testing
    "ignores leading whitespace"
    (is (= "{@writer} foo bar"
           (rewrite-tweet " @reader foo bar" "writer" "reader"))))

  (testing
    "ignores non-leading @reader"
    (is (= "{@writer} foo @reader bar"
           (rewrite-tweet "foo @reader bar" "writer" "reader")))
    (is (= "{@writer} foo @reader bar"
           (rewrite-tweet "@reader foo @reader bar" "writer" "reader"))))

)

