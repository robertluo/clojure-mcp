(ns clojure-mcp.repl-tools.namespace-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-mcp.repl-tools.test-utils :refer [*nrepl-client-atom* make-test-tool]]
   [clojure-mcp.repl-tools :as repl-tools]
   [clojure.string :as str]))

;; Setup test fixtures
(clojure-mcp.repl-tools.test-utils/apply-fixtures *ns*)

(deftest current-namespace-test
  (testing "Get current namespace"
    (let [ns-tool (make-test-tool (repl-tools/current-namespace *nrepl-client-atom*))
          result (ns-tool {})]
      ;; Initial namespace should be 'user'
      (is (= {:res ["user"] :error? false} result)))))

(deftest list-namespaces-test
  (testing "List currently loaded namespaces"
    (let [list-ns-tool (make-test-tool (repl-tools/list-namespaces *nrepl-client-atom*))
          result (list-ns-tool {})]
      (is (false? (:error? result)))
      (is (vector? (:res result)))
      ;; Check for essential namespaces that should always be loaded
      (is (some #(= % "clojure.core") (:res result)))
      (is (some #(= % "clojure.repl") (:res result))) ; Required by our setup
      (is (some #(= % "user") (:res result)))
      ;; Check that test namespace is loaded
      (is (some #(= % "clojure-mcp.repl-tools.namespace-test") (:res result))))))

(deftest list-vars-in-namespace-test
  (testing "List vars in a known namespace (clojure.string)"
    (let [list-vars-tool (make-test-tool (repl-tools/list-vars-in-namespace *nrepl-client-atom*))
          result (list-vars-tool {"namespace" "clojure.string"})]
      (is (false? (:error? result)))
      (is (vector? (:res result)))
      ;; Check the structure for a known var ('trim')
      (let [trim-var-info (->> (:res result)
                               (map read-string) ;; Read strings back to maps
                               (filter #(= 'trim (:name %)))
                               first)]
        (is (map? trim-var-info))
        (is (= 'trim (:name trim-var-info)))
        (is (= "clojure.string" (:ns trim-var-info)))
        (is (string? (:doc trim-var-info)))
        (is (list? (:arglists trim-var-info)))))) ;; Check arglists type

  (testing "List vars in a non-existent namespace"
    (let [list-vars-tool (make-test-tool (repl-tools/list-vars-in-namespace *nrepl-client-atom*))
          result (list-vars-tool {"namespace" "non-existent.namespace.foo"})]
      ;; Expecting an error because the namespace doesn't exist
      (is (true? (:error? result)))
      (is (re-find #"not found"
                   (first (:res result))))))

  
  (testing "List vars in namespace with no public vars (create one)"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          list-vars-tool (make-test-tool (repl-tools/list-vars-in-namespace *nrepl-client-atom*))]
      ;; Create an empty namespace
      (eval-tool {"expression" "(create-ns 'empty.test.ns)"})
      ;; List its vars
      (let [result (list-vars-tool {"namespace" "empty.test.ns"})]
        (is (false? (:error? result))) ;; Should not be an error, just empty
        (is (= [] (:res result)))))))