(ns clojure-mcp.tools.scratch-pad.core-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.scratch-pad.core :as core]))

(deftest test-parse-value
  (testing "EDN parsing with fallback"
    (is (= 42 (core/parse-value "42")))
    (is (= "hello" (core/parse-value "hello")))
    (is (= {:a 1} (core/parse-value "{:a 1}")))
    (is (= [1 2 3] (core/parse-value "[1 2 3]")))
    (is (= :keyword (core/parse-value ":keyword")))
    (is (= "not-valid-edn{" (core/parse-value "not-valid-edn{")))))

(deftest test-assoc-in-data
  (testing "Associating values at paths"
    (let [data {}]
      (is (= {"a" 1} (core/assoc-in-data data ["a"] 1)))
      (is (= {"a" {"b" 2}} (core/assoc-in-data data ["a" "b"] 2)))
      (is (= {"a" {"b" {:c 3}}} (core/assoc-in-data data ["a" "b"] {:c 3}))))))

(deftest test-get-in-data
  (testing "Getting values from paths"
    (let [data {"a" {"b" 2}}]
      (is (= {"b" 2} (core/get-in-data data ["a"])))
      (is (= 2 (core/get-in-data data ["a" "b"])))
      (is (nil? (core/get-in-data data ["x" "y"]))))))

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
