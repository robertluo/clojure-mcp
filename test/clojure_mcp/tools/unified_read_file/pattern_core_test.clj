(ns clojure-mcp.tools.unified-read-file.pattern-core-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.unified-read-file.pattern-core :as pattern-core]
   [rewrite-clj.zip :as z]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Test fixture setup
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)

(def test-file-content
  "(ns test.example
  \"Test namespace for pattern matching\"
  (:require [clojure.string :as str]))

;; Test validation function
(defn validate-input
  \"Validates user input\"
  [input]
  (when (empty? input)
    (throw (ex-info \"Input cannot be empty\" {:input input})))
  input)

;; Function with error handling
(defn process-data
  \"Processes data with error handling\"
  [data]
  (try
    (validate-input data)
    (str/upper-case data)
    (catch Exception e
      (str \"Error: \" (.getMessage e)))))

;; TODO: Add more validation functions

(def config {:max-retries 3})

(defmethod handle-request :json
  [request]
  (println \"Handling JSON request\")
  {:status 200})

(comment
  \"This is a test comment block\"
  (validate-input \"test\")
  (process-data \"\")
  (+ 1 2))
")

(defn setup-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-pattern-test")
        test-file (io/file test-dir "test-file.clj")]

    ;; Create test directory and files
    (.mkdirs test-dir)

    ;; Create Clojure test file with functions matching different patterns
    (spit test-file test-file-content)

    ;; Run test with fixtures bound
    (binding [*test-dir* test-dir
              *test-file* test-file]
      (try
        (f)
        (finally
          ;; Clean up
          (when (.exists test-file)
            (.delete test-file))
          (when (.exists test-dir)
            (.delete test-dir)))))))

(use-fixtures :each setup-test-files-fixture)

(deftest test-valid-form-to-include
  (testing "Valid forms are included"
    (let [zloc (z/of-string "(defn example [x] x)")]
      (is (pattern-core/valid-form-to-include? zloc false)))))

(deftest test-extract-form-name
  (testing "Extract name from defn"
    (is (= "example" (pattern-core/extract-form-name '(defn example [x] x)))))

  (testing "Extract name from def"
    (is (= "config" (pattern-core/extract-form-name '(def config {:a 1})))))

  (testing "Extract name from defmethod"
    (is (= "handle-request" (pattern-core/extract-form-name '(defmethod handle-request :json [x] x)))))

  (testing "Extract name from ns"
    (is (= "test.namespace" (pattern-core/extract-form-name '(ns test.namespace)))))

  (testing "Returns nil for non-forms"
    (is (nil? (pattern-core/extract-form-name '(+ 1 2)))))

  (testing "Handles edge cases"
    (is (nil? (pattern-core/extract-form-name nil)))
    (is (nil? (pattern-core/extract-form-name '())))
    (is (nil? (pattern-core/extract-form-name '(defn))))))

(deftest test-collect-top-level-forms
  (testing "Collects all forms excluding comments"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)]
      (is (= 5 (count forms)))
      (is (= #{"test.example" "validate-input" "process-data" "config" "handle-request"}
             (set (map :name forms))))
      (is (contains? (frequencies (map :type forms)) "defn")))))

(deftest test-filter-forms-by-pattern
  (testing "Filter by name pattern"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms "validate.*" nil)]
      (is (= ["validate-input"] (:matches result)))
      (is (= 1 (get-in result [:pattern-info :match-count])))))

  (testing "Filter by content pattern"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms nil "throw")]
      (is (= ["validate-input"] (:matches result)))
      (is (= 1 (get-in result [:pattern-info :match-count])))))

  (testing "Filter by both patterns"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms "process.*" "catch")]
      (is (= ["process-data"] (:matches result)))
      (is (= 1 (get-in result [:pattern-info :match-count])))))

  (testing "No matches returns empty result"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms "nonexistent" nil)]
      (is (empty? (:matches result)))
      (is (= 0 (get-in result [:pattern-info :match-count])))))

  (testing "Multiple matches return all matching forms"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms ".*data.*" nil)]
      (is (= ["process-data"] (:matches result)))
      (is (= 1 (get-in result [:pattern-info :match-count])))))

  (testing "Match by form type"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms nil "defmethod")]
      (is (= ["handle-request"] (:matches result)))
      (is (= 1 (get-in result [:pattern-info :match-count])))))

  (testing "Empty pattern returns no matches"
    (let [forms (pattern-core/collect-top-level-forms (.getAbsolutePath *test-file*) false)
          result (pattern-core/filter-forms-by-pattern forms "" nil)]
      (is (empty? (:matches result)))
      (is (= 0 (get-in result [:pattern-info :match-count]))))))

(deftest test-generate-pattern-based-file-view
  (testing "Generate collapsed view with name pattern"
    (let [result (pattern-core/generate-pattern-based-file-view
                  (.getAbsolutePath *test-file*) "validate.*" nil true false)]
      (is (sequential? (:matches result)))
      (is (map? (:pattern-info result)))
      (is (= "validate.*" (get-in result [:pattern-info :name-pattern])))))

  (testing "Generate collapsed view with content pattern"
    (let [result (pattern-core/generate-pattern-based-file-view
                  (.getAbsolutePath *test-file*) nil "try|catch" true false)]
      (is (sequential? (:matches result)))
      (is (= "try|catch" (get-in result [:pattern-info :content-pattern])))))

  (testing "Generate raw view when collapsed is false"
    (let [result (pattern-core/generate-pattern-based-file-view
                  (.getAbsolutePath *test-file*) nil nil false false)]
      (is (= :raw (:mode result)))
      (is (string? (:content result))))))

(deftest test-error-handling
  (testing "Invalid name pattern regex throws meaningful exception"
    (let [forms [{:name "example" :content "(defn example [x] x)"}]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid name pattern regex"
           (pattern-core/filter-forms-by-pattern forms "[" nil)))))

  (testing "Invalid content pattern regex throws meaningful exception"
    (let [forms [{:name "example" :content "(defn example [x] x)"}]]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid content pattern regex"
           (pattern-core/filter-forms-by-pattern forms nil "("))))))

(deftest test-edge-cases
  (testing "Empty file handling"
    (let [empty-file-path (.getAbsolutePath (io/file *test-dir* "empty.clj"))]
      (try
        (spit empty-file-path "")
        (let [forms (pattern-core/collect-top-level-forms empty-file-path false)]
          (is (empty? forms)))
        (finally
          (io/delete-file empty-file-path true)))))

  (testing "Malformed Clojure code throws exception"
    (let [malformed-file-path (.getAbsolutePath (io/file *test-dir* "malformed.clj"))]
      (try
        (spit malformed-file-path "(defn broken [")
        (is (thrown? Exception
                     (pattern-core/collect-top-level-forms malformed-file-path false)))
        (finally
          (io/delete-file malformed-file-path true))))))


