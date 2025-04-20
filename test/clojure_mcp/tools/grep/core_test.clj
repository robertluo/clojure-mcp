(ns clojure-mcp.tools.grep.core-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.grep.core :as sut]
            [clojure.java.io :as io]))

(deftest grep-files-with-test-directory
  (testing "grep-files with controlled test directory"
    (let [temp-dir (io/file (System/getProperty "java.io.tmpdir") "grep-files-test")]

      ;; Set up test directory structure
      (.mkdirs temp-dir)
      (let [file1 (io/file temp-dir "file1.clj")
            file2 (io/file temp-dir "file2.clj")
            file3 (io/file temp-dir "file3.txt")
            subdir (io/file temp-dir "subdir")
            file4 (io/file subdir "file4.clj")]

        (try
          ;; Create test files with specific content
          (.mkdirs subdir)
          (spit file1 "(defn test-function [x] (+ x 1))")
          (spit file2 "(defn another-function [y] (* y 2))")
          (spit file3 "This is a text file with no functions")
          (spit file4 "(defn nested-function [z] (- z 3))")

          (testing "find pattern across all files"
            (let [result (sut/grep-files (.getAbsolutePath temp-dir) "defn")]
              (is (map? result))
              (is (vector? (:filenames result)))
              (is (= 3 (:numFiles result)))
              (is (some #(.contains % "file1.clj") (:filenames result)))
              (is (some #(.contains % "file2.clj") (:filenames result)))
              (is (some #(.contains % "file4.clj") (:filenames result)))
              (is (not (some #(.contains % "file3.txt") (:filenames result))))))

          (testing "find pattern with file extension filter"
            (let [result (sut/grep-files (.getAbsolutePath temp-dir) "defn" :include "*.clj")]
              (is (= 3 (:numFiles result)))
              (is (some #(.contains % "file1.clj") (:filenames result)))
              (is (some #(.contains % "file2.clj") (:filenames result)))
              (is (some #(.contains % "file4.clj") (:filenames result)))))

          (testing "find pattern with text file extension"
            (let [result (sut/grep-files (.getAbsolutePath temp-dir) "text file" :include "*.txt")]
              (is (= 1 (:numFiles result)))
              (is (some #(.contains % "file3.txt") (:filenames result)))))

          (testing "no results for non-existent pattern"
            (let [result (sut/grep-files (.getAbsolutePath temp-dir) "THIS_PATTERN_DOESNT_EXIST")]
              (is (= 0 (:numFiles result)))
              (is (empty? (:filenames result)))))

          (testing "max results parameter"
            (let [result (sut/grep-files (.getAbsolutePath temp-dir) "defn" :max-results 2)]
              (is (<= (:numFiles result) 2))
              (is (<= (count (:filenames result)) 2))))

          (finally
            ;; Clean up
            (io/delete-file file1 true)
            (io/delete-file file2 true)
            (io/delete-file file3 true)
            (io/delete-file file4 true)
            (io/delete-file subdir true)
            (io/delete-file temp-dir true)))))))

(deftest grep-files-with-project-directory
  (testing "grep-files with the actual project directory"
    (let [project-dir "/Users/bruce/workspace/llempty/clojure-mcp"]

      (testing "find tool-system multimethod usage"
        (let [result (sut/grep-files project-dir "defmethod tool-system" :include "*.clj")]
          (is (> (:numFiles result) 0))
          (is (some #(.contains % "tool.clj") (:filenames result)))))

      (testing "find core functions"
        (let [result (sut/grep-files (str project-dir "/src") "defn grep-" :include "*.clj")]
          (is (> (:numFiles result) 0))
          (is (some #(.contains % "grep") (:filenames result)))))

      (testing "find with multiple file extensions using brace expansion"
        (let [result (sut/grep-files project-dir "project.clj" :include "*.{md,clj}")]
          (is (> (:numFiles result) 0))))

      (testing "with complex regex pattern"
        (let [result (sut/grep-files (str project-dir "/src") "defn\\s+[a-z\\-]+\\s*\\[" :include "*.clj")]
          (is (> (:numFiles result) 0))
          (is (> (:durationMs result) 0))))

      (testing "with max-results limitation"
        (let [result (sut/grep-files project-dir "defn" :include "*.clj" :max-results 5)]
          (is (<= (:numFiles result) 5))
          (is (<= (count (:filenames result)) 5))))

      (testing "filenames are sorted by modification time (newest first)"
        (let [result (sut/grep-files project-dir "defn" :max-results 10)]
          (when (> (count (:filenames result)) 1)
            (let [files (mapv io/file (:filenames result))
                  mod-times (mapv #(.lastModified %) files)]
              (is (apply >= mod-times)))))))))

(deftest grep-files-error-handling
  (testing "error handling in grep-files"
    (testing "non-existent directory"
      (let [result (sut/grep-files "/path/that/does/not/exist" "pattern")]
        (is (contains? result :error))
        (is (string? (:error result)))
        (is (.contains (:error result) "not a valid directory"))))

    (testing "invalid regex pattern handling"
      (let [temp-dir (io/file (System/getProperty "java.io.tmpdir") "grep-error-test")]
        (try
          (.mkdirs temp-dir)
          (spit (io/file temp-dir "test.txt") "test content")

          (let [result (sut/grep-files (.getAbsolutePath temp-dir) "[invalid regex)]")]
            ;; Either it has :error or it has :filenames (depending on if using grep or Java implementation)
            (is (map? result))
            (is (contains? result :durationMs))
            (is (or
                 ;; Command line grep might return an error
                 (contains? result :error)
                 ;; Java implementation might handle it and return empty results
                 (and (contains? result :filenames)
                      (zero? (:numFiles result))))))

          (finally
            (io/delete-file (io/file temp-dir "test.txt") true)
            (io/delete-file temp-dir true)))))))
