(ns clojure-mcp.tools.glob-files.core-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.glob-files.core :as sut]
            [clojure.java.io :as io]))

(deftest glob-files-test
  (testing "Basic glob patterns with existing directory"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/*.clj" :max-results 5)]
      (is (not (:error result)) "Should not return an error")
      (is (vector? (:filenames result)) "Should return filenames as a vector")
      (is (<= (count (:filenames result)) 5) "Should limit results to max-results")
      (is (= (count (:filenames result)) (:numFiles result)) "numFiles should match filenames count")
      (is (boolean? (:truncated result)) "truncated should be a boolean")
      (is (number? (:durationMs result)) "durationMs should be a number")))

  (testing "Filtering with specific pattern"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/glob_files/**/*.clj")]
      (is (not (:error result)) "Should not return an error")
      (is (vector? (:filenames result)) "Should return filenames as a vector")
      (is (every? #(.contains % "glob_files") (:filenames result))
          "All files should contain glob_files in the path")))

  (testing "Handling non-existent directory"
    (let [result (sut/glob-files "/nonexistent/directory" "*.clj")]
      (is (:error result) "Should return an error")
      (is (string? (:error result)) "Error should be a string")))

  (testing "Handling invalid glob pattern"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "[invalid-glob-pattern")]
      (is (not (:error result)) "May not return an error for invalid pattern")
      (is (= 0 (:numFiles result)) "Should find 0 files for invalid pattern")))

  (testing "Result truncation with small max-results"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/*.clj" :max-results 1)]
      (is (not (:error result)) "Should not return an error")
      (is (= 1 (count (:filenames result))) "Should return exactly 1 result")
      (is (:truncated result) "Should indicate results were truncated"))))
