(ns clojure-mcp.repl-tools.history-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-mcp.repl-tools.test-utils :refer [*nrepl-client-atom* make-test-tool]]
   [clojure-mcp.repl-tools :as repl-tools]
   [clojure-mcp.nrepl :as nrepl]))

;; Setup test fixtures
(clojure-mcp.repl-tools.test-utils/apply-fixtures *ns*)

(deftest eval-history-test
  (testing "Get eval history"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          history-tool (make-test-tool (repl-tools/eval-history *nrepl-client-atom*))]
      ;; Clear history (or ensure it's clean) - easiest is to eval something known
      (repl-tools/eval-history-reset (::nrepl/state @*nrepl-client-atom*))
      (eval-tool {"expression" "(+ 1 1)"}) ;; Eval 1
      (eval-tool {"expression" "(+ 2 2)"}) ;; Eval 2
      (eval-tool {"expression" "(+ 3 3)"}) ;; Eval 3

      (testing "Fetch last 2 items"
        (let [result (history-tool {"number-to-fetch" "2"})]
          (is (false? (:error? result)))
          (is (= ["(+ 3 3)" "(+ 2 2)"] (:res result)))))

      (testing "Fetch more items than available"
        (let [result (history-tool {"number-to-fetch" "10"})]
          (is (false? (:error? result)))
          ;; Should return all available items if n > count
          (is (= ["(+ 3 3)" "(+ 2 2)" "(+ 1 1)"] (:res result)))))

      (testing "Fetch 0 items"
        (let [result (history-tool {"number-to-fetch" "0"})]
          (is (false? (:error? result)))
          (is (= [] (:res result)))))

      (testing "Invalid number-to-fetch"
        (let [result (history-tool {"number-to-fetch" "abc"})]
          (is (true? (:error? result)))
          (is (re-find #"Invalid 'number-to-fetch'" (first (:res result))))))

      (testing "Missing number-to-fetch"
        ;; Assuming the schema requires it, this might error differently depending on MCP impl,
        ;; but the tool function itself would likely throw if it got nil.
        ;; Let's test the tool function's try-catch for parsing.
        (let [result (history-tool {"number-to-fetch" nil})] ;; Simulate missing/invalid arg reaching fn
          (is (true? (:error? result)))
          (is (re-find #"Invalid 'number-to-fetch'" (first (:res result)))))))))
