(ns clojure-mcp.tools.grep.core-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.grep.core :as sut]
            [clojure.java.io :as io]))

(deftest grep-files-test
  (testing "grep-files delegates correctly to fs-grep/grep-files"
    (with-redefs [clojure-mcp.repl-tools.filesystem.grep/grep-files
                  (fn [path pattern & {:keys [include max-results]}]
                    {:path path
                     :pattern pattern
                     :include include
                     :max-results max-results})]

      (testing "with required parameters only"
        (let [result (sut/grep-files "/some/path" "pattern")]
          (is (= {:path "/some/path"
                  :pattern "pattern"
                  :include nil
                  :max-results 1000} result))))

      (testing "with all parameters"
        (let [result (sut/grep-files "/some/path" "pattern"
                                     :include "*.clj"
                                     :max-results 500)]
          (is (= {:path "/some/path"
                  :pattern "pattern"
                  :include "*.clj"
                  :max-results 500} result)))))))