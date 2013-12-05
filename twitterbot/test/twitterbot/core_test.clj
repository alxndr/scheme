(ns twitterbot.core-test
  (:require [clojure.test :refer :all]
            [twitterbot.core :refer :all]))

(deftest rewrite-tweet-test
  (testing "prepends username"
           (is (= "{user foo bar")
               (rewrite-tweet "foo bar" "user")))
  (testing "removes leading @oberlin"
           (is (= "{user} foo bar" 
                  (rewrite-tweet "@oberlin foo bar" "user"))))
)

