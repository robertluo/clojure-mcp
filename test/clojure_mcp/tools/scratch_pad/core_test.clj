(ns clojure-mcp.tools.scratch-pad.core-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.scratch-pad.core :as core]))

(deftest test-dissoc-in-data
  (testing "Removing values at paths"
    (let [data {"a" {"b" 2 "c" 3}}]
      (is (= {"a" {"c" 3}} (core/dissoc-in-data data ["a" "b"])))
      (is (= {} (core/dissoc-in-data data ["a"])))
      (is (= data (core/dissoc-in-data data [])))
      (is (= {"a" {}} (core/dissoc-in-data {"a" {"b" 2}} ["a" "b"]))))))

(deftest test-tree-view
  (testing "Tree view generation"
    (is (= "Empty scratch pad" (core/tree-view {})))
    (testing "Pretty printing simple data"
      (let [data {"a" 1}
            result (core/tree-view data)]
        (is (string? result))
        (is (.contains result "{\"a\" 1}"))))
    (testing "Pretty printing nested data"
      (let [nested {"a" {"b" {"c" 1}}}
            view (core/tree-view nested)]
        (is (.contains view "{\"a\" {\"b\" {\"c\" 1}}}")))))) ; Should truncate at depth 2
