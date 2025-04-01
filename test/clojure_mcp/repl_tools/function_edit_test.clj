(ns clojure-mcp.repl-tools.function-edit-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.repl-tools.function-edit :refer [replace-function-in-file]]
            [clojure.java.io :as io]
            [rewrite-clj.zip :as z]))

(def ^:dynamic *test-file-path* "temp_test_file.clj")

(defn setup-test-file [content]
  (spit *test-file-path* content))

(defn cleanup-test-file [f]
  (try
    (f)
    (finally
      (io/delete-file *test-file-path* true))))

(use-fixtures :each cleanup-test-file)

(deftest replace-function-in-file-test
  (testing "Replacing a defn in the middle of a file"
    (let [initial-content "(ns temp-test)\n\n(defn foo [x] (+ x 1))\n\n(defn bar [y] (* y 2))\n"
          new-foo-text "(defn foo\n  \"New docstring.\"\n  [x]\n  (* x 10))"
          expected-content "(ns temp-test)\n\n(defn foo\n  \"New docstring.\"\n  [x]\n  (* x 10))\n\n(defn bar [y] (* y 2))\n"]
      (setup-test-file initial-content)
      (is (true? (replace-function-in-file "foo" *test-file-path* new-foo-text)))
      (is (= expected-content (slurp *test-file-path*)))))

  (testing "Replacing a def at the start of a file"
    (let [initial-content "(def my-var 123)\n\n(defn baz [] :hello)"
          new-var-text "(def my-var\n  ;; New comment\n  456)"
          expected-content "(def my-var\n  ;; New comment\n  456)\n\n(defn baz [] :hello)"] ; Note: rewrite-clj might add/remove trailing newline
      (setup-test-file initial-content)
      (is (true? (replace-function-in-file 'my-var *test-file-path* new-var-text))) ; Test with symbol
      ;; Normalize potential trailing newline differences
      (is (= (clojure.string/trim expected-content) (clojure.string/trim (slurp *test-file-path*))))))

  (testing "Replacing a defn at the end of a file"
    (let [initial-content "(ns end-test)\n\n(defn first-fn [])\n\n(defn last-fn [a b]\n  (+ a b))"
          new-last-fn-text "(defn last-fn\n  [a b c]\n  (* a b c))"
          expected-content "(ns end-test)\n\n(defn first-fn [])\n\n(defn last-fn\n  [a b c]\n  (* a b c))"]
      (setup-test-file initial-content)
      (is (true? (replace-function-in-file "last-fn" *test-file-path* new-last-fn-text)))
      (is (= (clojure.string/trim expected-content) (clojure.string/trim (slurp *test-file-path*))))))

  (testing "Function not found"
    (let [initial-content "(defn only-fn [])"]
      (setup-test-file initial-content)
      (is (false? (replace-function-in-file "non-existent-fn" *test-file-path* "(defn non-existent-fn [])")))
      ;; Ensure file content is unchanged
      (is (= initial-content (slurp *test-file-path*)))))

  (testing "File not found"
    (is (false? (replace-function-in-file "any-fn" "non-existent-file.clj" "(defn any-fn [])"))))

  (testing "Replacing function with comments and different formatting"
    (let [initial-content "(ns format-test)\n\n;; Comment before\n(defn func-to-replace \n [arg1]\n ;; body comment\n (inc arg1))\n\n;; Comment after\n(def another-var)"
          new-func-text "(defn func-to-replace\n  [arg1 arg2]\n  (+ arg1 arg2))"
          expected-content "(ns format-test)\n\n;; Comment before\n(defn func-to-replace\n  [arg1 arg2]\n  (+ arg1 arg2))\n\n;; Comment after\n(def another-var)"]
      (setup-test-file initial-content)
      (is (true? (replace-function-in-file "func-to-replace" *test-file-path* new-func-text)))
      (is (= (clojure.string/trim expected-content) (clojure.string/trim (slurp *test-file-path*))))))

  (testing "Error parsing new function text"
    (let [initial-content "(defn good-fn [])"
          invalid-new-text "(defn bad-fn ["] ; Missing closing bracket
      (setup-test-file initial-content)
      ;; Expect false because the new text cannot be parsed by rewrite-clj
      (is (false? (replace-function-in-file "good-fn" *test-file-path* invalid-new-text)))
      ;; Ensure original file is untouched on parse error
      (is (= initial-content (slurp *test-file-path*)))))
)
