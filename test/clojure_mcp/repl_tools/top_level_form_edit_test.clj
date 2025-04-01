(ns clojure-mcp.repl-tools.top-level-form-edit-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.repl-tools.top-level-form-edit :as form-edit]
            [rewrite-clj.zip :as z]
            [clojure.java.io :as io]))

;; Sample code for testing
(def sample-code "(ns test.namespace
  (:require [clojure.string :as str]))

(defn test-function
  \"A test function\"
  [x y]
  (+ x y))

(def test-var 42)

(defn ^:private meta-function
  \"A function with metadata\"
  [a]
  (* a a))

(defn #_\"discarded docstring\" weird-case
  [a b]
  (+ a b))")

(deftest test-is-top-level-form?
  (testing "Basic matching of top-level forms"
    (let [zloc (z/of-string sample-code)]
      (is (not (form-edit/is-top-level-form? zloc "defn" "test-function")))
      
      ;; Move to the namespace form
      (is (form-edit/is-top-level-form? zloc "ns" "test.namespace"))
      
      ;; Move to the test-function form
      (let [defn-loc (-> zloc z/right)]
        (is (form-edit/is-top-level-form? defn-loc "defn" "test-function")))
      
      ;; Move to the test-var form
      (let [def-loc (-> zloc z/right z/right)]
        (is (form-edit/is-top-level-form? def-loc "def" "test-var")))
      
      ;; Move to the meta-function form
      (let [meta-loc (-> zloc z/right z/right z/right)]
        (is (form-edit/is-top-level-form? meta-loc "defn" "meta-function")))
      
      ;; Move to the weird-case form
      (let [weird-loc (-> zloc z/right z/right z/right z/right)]
        (is (form-edit/is-top-level-form? weird-loc "defn" "weird-case"))))))

(deftest test-find-top-level-form
  (testing "Finding top-level forms by name and tag"
    (let [zloc (z/of-string sample-code)]
      ;; Test finding each form
      (is (form-edit/find-top-level-form zloc "ns" "test.namespace"))
      (is (form-edit/find-top-level-form zloc "defn" "test-function"))
      (is (form-edit/find-top-level-form zloc "def" "test-var"))
      (is (form-edit/find-top-level-form zloc "defn" "meta-function"))
      (is (form-edit/find-top-level-form zloc "defn" "weird-case"))
      
      ;; Test not finding non-existent forms
      (is (nil? (form-edit/find-top-level-form zloc "def" "nonexistent")))
      (is (nil? (form-edit/find-top-level-form zloc "defmacro" "test-function"))))))

(deftest test-replace-top-level-form
  (testing "Replacing top-level forms"
    (let [zloc (z/of-string sample-code)
          ;; Replace the test-function
          new-fn-str "(defn test-function [a b c] (+ a b c))"
          replaced-zloc (form-edit/replace-top-level-form zloc "defn" "test-function" new-fn-str)
          ;; Replace the test-var
          new-var-str "(def test-var :keyword-value)"
          replaced-zloc2 (form-edit/replace-top-level-form replaced-zloc "def" "test-var" new-var-str)]
      
      ;; Verify the replacements
      (let [result-str (z/root-string replaced-zloc2)]
        (is (re-find #"\(defn test-function \[a b c\] \(\+ a b c\)\)" result-str))
        (is (re-find #"\(def test-var :keyword-value\)" result-str))))))

;; Helper function for temporary files
(defn with-temp-file [content f]
  (let [temp-file (java.io.File/createTempFile "test" ".clj")]
    (try
      (spit temp-file content)
      (f (.getPath temp-file))
      (finally
        (.delete temp-file)))))

(deftest test-edit-form-in-file
  (testing "Editing a form in a file without modifying the file"
    (with-temp-file sample-code
      (fn [file-path]
        (let [;; Test editing a function
              new-fn-str "(defn test-function [x y z] (* x y z))"
              edited-content (form-edit/edit-form-in-file file-path "defn" "test-function" new-fn-str)]
          (is (re-find #"\(defn test-function \[x y z\] \(\* x y z\)\)" edited-content))
          ;; Verify the file content hasn't changed
          (is (re-find #"\(defn test-function\s+\"A test function\"\s+\[x y\]\s+\(\+ x y\)\)" (slurp file-path))))))))

(deftest test-replace-form-in-file
  (testing "Replacing a form in a file"
    (with-temp-file sample-code
      (fn [file-path]
        ;; Test replacing a variable
        (let [new-var-str "(def test-var [1 2 3])"
              result (form-edit/replace-form-in-file "test-var" file-path "def" new-var-str)]
          (is result)
          ;; Verify the file has been updated
          (is (re-find #"\(def test-var \[1 2 3\]\)" (slurp file-path))))
        
        ;; Test replacing the namespace
        (let [new-ns-str "(ns test.namespace (:require [clojure.string :as str] [clojure.set :as set]))"
              result (form-edit/replace-form-in-file "test.namespace" file-path "ns" new-ns-str)]
          (is result)
          ;; Verify the file has been updated
          (is (re-find #"\(:require \[clojure\.string :as str\] \[clojure\.set :as set\]\)" (slurp file-path))))))))

