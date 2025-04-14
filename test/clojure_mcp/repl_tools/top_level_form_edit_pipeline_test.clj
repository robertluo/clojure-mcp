(ns clojure-mcp.repl-tools.top-level-form-edit-pipeline-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as pipeline]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)))

(defn create-temp-file [content]
  (let [temp-file (File/createTempFile "test-edit" ".clj")]
    (.deleteOnExit temp-file)
    (spit temp-file content)
    temp-file))

(deftest test-load-source
  (testing "Load source from existing file"
    (let [content "(defn test-fn [x] x)"
          temp-file (create-temp-file content)
          result (pipeline/load-source {::pipeline/file-path (.getPath temp-file)})]
      (is (= content (::pipeline/source result)))
      (is (nil? (::pipeline/error result)))))
  
  (testing "Load source from non-existent file"
    (let [result (pipeline/load-source {::pipeline/file-path "/non-existent-file.clj"})]
      (is (= :file-not-found (::pipeline/error result)))
      (is (string? (::pipeline/message result))))))

(deftest test-lint-code
  (testing "Lint valid code"
    (let [valid-code "(defn test-fn [x] x)"
          result (pipeline/lint-code {::pipeline/new-source-code valid-code})]
      (is (nil? (::pipeline/error result)))
      (is (nil? (::pipeline/lint-result result)))))
  
  (testing "Lint invalid code"
    (let [invalid-code "(defn test-fn [x] (let [y 1] y)"  ; Missing closing paren
          result (pipeline/lint-code {::pipeline/new-source-code invalid-code})]
      (is (= :lint-failure (::pipeline/error result)))
      (is (string? (::pipeline/message result))))))

(deftest test-find-form
  (testing "Find existing form"
    (let [content "(defn test-fn [x] x)\n\n(def test-var 42)"
          ctx {::pipeline/source content
               ::pipeline/top-level-def-name "test-fn"
               ::pipeline/top-level-def-type "defn"}
          result (pipeline/find-form ctx)]
      (is (nil? (::pipeline/error result)))
      (is (some? (::pipeline/zloc result)))))
  
  (testing "Find non-existent form"
    (let [content "(defn test-fn [x] x)\n\n(def test-var 42)"
          ctx {::pipeline/source content
               ::pipeline/top-level-def-name "non-existent"
               ::pipeline/top-level-def-type "defn"}
          result (pipeline/find-form ctx)]
      (is (= :form-not-found (::pipeline/error result)))
      (is (string? (::pipeline/message result))))))

(deftest test-thread-ctx
  (testing "Thread context with no errors"
    (let [ctx {:key1 "value1"}
          f1 (fn [c] (assoc c :key2 "value2"))
          f2 (fn [c] (assoc c :key3 "value3"))
          result (pipeline/thread-ctx ctx f1 f2)]
      (is (= "value1" (:key1 result)))
      (is (= "value2" (:key2 result)))
      (is (= "value3" (:key3 result)))))
  
  (testing "Thread context with error"
    (let [ctx {:key1 "value1"}
          f1 (fn [c] (assoc c ::pipeline/error :test-error ::pipeline/message "Error message"))
          f2 (fn [c] (assoc c :key2 "should not be reached"))
          result (pipeline/thread-ctx ctx f1 f2)]
      (is (= "value1" (:key1 result)))
      (is (= :test-error (::pipeline/error result)))
      (is (= "Error message" (::pipeline/message result)))
      (is (nil? (:key2 result))))))

(deftest test-format-result
  (testing "Format successful result"
    (let [ctx {::pipeline/offsets [10 20]}
          result (pipeline/format-result ctx)]
      (is (false? (:error result)))
      (is (= [10 20] (:offsets result)))))
  
  (testing "Format error result"
    (let [ctx {::pipeline/error :test-error
               ::pipeline/message "Error message"}
          result (pipeline/format-result ctx)]
      (is (true? (:error result)))
      (is (= "Error message" (:message result))))))

(deftest test-edit-form-in-file
  (testing "Edit existing form"
    (let [content "(defn test-fn [x] x)\n\n(def test-var 42)"
          temp-file (create-temp-file content)
          new-impl "(defn test-fn [x y] (+ x y))"
          result (pipeline/edit-form-in-file "test-fn" (.getPath temp-file) "defn" new-impl :replace)]
      (is (vector? result))
      (is (= 2 (count result)))
      (let [updated-content (slurp temp-file)]
        (is (str/includes? updated-content new-impl)))))
  
  (testing "Edit non-existent form"
    (let [content "(defn test-fn [x] x)\n\n(def test-var 42)"
          temp-file (create-temp-file content)
          new-impl "(defn non-existent [x y] (+ x y))"
          result (pipeline/edit-form-in-file "non-existent" (.getPath temp-file) "defn" new-impl :replace)]
      (is (map? result))
      (is (:error result))
      (is (string? (:message result))))))

(comment
  ;; Run all tests
  (clojure.test/run-tests 'clojure-mcp.repl-tools.top-level-form-edit-pipeline-test)
  
  ;; Run a specific test
  (clojure.test/test-vars [#'test-load-source])
)
