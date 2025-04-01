(ns clojure-mcp.repl-tools.eval-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-mcp.repl-tools.test-utils :refer [*nrepl-client-atom* make-test-tool]]
   [clojure-mcp.repl-tools :as repl-tools]
   [clojure.string :as str]))

;; Setup test fixtures
(clojure-mcp.repl-tools.test-utils/apply-fixtures *ns*)

(deftest eval-code-test
  (testing "Basic evaluation"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(+ 1 2)"})]
      (is (false? (:error? result)))
      (is (= ["=> 3"] (:res result))))) ;; Check for prefixed value

  (testing "Evaluation with output"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(println \"hello\")"})
          res (:res result)]
      (is (false? (:error? result)))
      ;; Check for the combined output and value string
      (is (= ["hello\n=> nil"] res))))

  (testing "Evaluation with error"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(throw (Exception. \"test error\"))"})
          res (:res result)]
      (is (true? (:error? result)))
      ;; Check the single result string for error content and the marker
      (is (= 1 (count res))) ;; Should be one string
      (is (str/includes? (first res) "test error"))
      (is (str/includes? (first res) "Evaluation failed"))))

  (testing "Evaluation resulting in nil"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(identity nil)"})]
      (is (false? (:error? result)))
      (is (= ["=> nil"] (:res result))))) ;; Check for prefixed value

  (testing "Evaluation with linter warning"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          ;; Use let with unused binding to trigger linter
          result (eval-tool {"expression" "(let [unused 1] (+ 2 3))"})
          res (:res result)]
      (is (false? (:error? result))) ;; Linter warning is not a tool error
      ;; Check the single result string for linter warning and value
      (is (= 1 (count res))) ;; Should be one string
      (is (str/includes? (first res) "unused binding")) ;; Check content of lint
      (is (str/includes? (first res) "=> 5")))) ;; Check for correct value

  (testing "Evaluation with linter error"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          ;; Use an invalid form to trigger linter error
          result (eval-tool {"expression" "(def ^:dynamic 1)"})
          res (:res result)]
      ;; The tool now returns error? true if the linter finds an error
      (is (true? (:error? result)))
      (is (= 1 (count res))) ;; Should be one string containing only linter output
      ;; Check for the actual clj-kondo parsing error message
      (is (str/includes? (first res) "Can't parse <stdin>"))
      ;; Evaluation should not proceed if linter finds an error
      (is (not-any? #(str/starts-with? % "=> ") res)))))