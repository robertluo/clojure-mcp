(ns clojure-mcp.repl-tools-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.repl-tools :as repl-tools]
   [nrepl.server :as nrepl-server]
   [clojure.string :as str]))
) ;; <- Added closing parenthesis

(defonce ^:dynamic *nrepl-server* nil)
(defonce ^:dynamic *nrepl-client-atom* nil)

(defn test-nrepl-fixture [f]
  (let [server (nrepl-server/start-server :port 0) ; Use port 0 for dynamic port assignment
        port (:port server)
        client (nrepl/create {:port port})
        client-atom (atom client)]
    (nrepl/start-polling client)
    (nrepl/eval-code client "(require 'clojure.repl)" identity)
    (binding [*nrepl-server* server
              *nrepl-client-atom* client-atom]
      (try
        (f)
        (finally
          (nrepl/stop-polling client)
          (nrepl-server/stop-server server))))))

(use-fixtures :once test-nrepl-fixture)

;; Helper to invoke tool functions more easily in tests
(defn make-test-tool [{:keys [tool-fn] :as _tool-map}]
  (fn [arg-map]
    (let [prom (promise)]
      (tool-fn nil arg-map
               (fn [res error?]
                 (deliver prom {:res res :error? error?})))
      @prom)))

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
      ;; Check for specific prefixed outputs
      (is (some #(str/starts-with? % "OUT: hello") res)) ;; Check stdout
      (is (some #(= % "=> nil") res)))) ;; Check return value

  (testing "Evaluation with error"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(throw (Exception. \"test error\"))"})
          res (:res result)]
      (is (true? (:error? result)))
      ;; Check for ERR prefix and the error marker added by the tool
      (is (some #(str/starts-with? % "ERR: ") res))
      (is (some #(str/includes? % "test error") res))
      (is (some #(= % "ERROR: Evaluation failed") res))))

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
      (is (some #(str/starts-with? % "LINTER: ") res)) ;; Check for linter output
      (is (some #(str/includes? % "unused binding") res)) ;; Check content of lint
      (is (some #(= % "=> 5") res)))) ;; Check for correct value

  (testing "Evaluation with linter error"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          ;; Use an invalid form to trigger linter error
          result (eval-tool {"expression" "(def ^:dynamic 1)"})
          res (:res result)]
      ;; The tool now returns error? true if the linter finds an error
      (is (true? (:error? result)))
      (is (some #(str/starts-with? % "LINTER: ") res)) ;; Check for linter output
      (is (some #(str/includes? % "Expected symbol, found") res)) ;; Check content of lint error
      ;; Evaluation should not proceed if linter finds an error
      (is (not-any? #(str/starts-with? % "=> ") res))))

(deftest current-namespace-test
  (testing "Get current namespace"
    (let [ns-tool (make-test-tool (repl-tools/current-namespace *nrepl-client-atom*))
          result (ns-tool {})]
      ;; Initial namespace should be 'user'
      (is (= {:res ["user"] :error? false} result)))))

(deftest symbol-completions-test
  (testing "Get completions for a prefix"
    (let [comp-tool (make-test-tool (repl-tools/symbol-completions *nrepl-client-atom*))
          result (comp-tool {"prefix" "ma"})]
      (is (false? (:error? result)))
      (is (vector? (:res result)))
      ;; Check if common completions are present (represented as strings)
      (is (some #(= % "map") (:res result))) ;; Expect plain string
      (is (some #(= % "mapcat") (:res result))) ;; Expect plain string
      (is (some #(= % "max") (:res result)))))) ;; Expect plain string

(deftest symbol-metadata-test
  (testing "Get metadata for a core symbol"
    (let [meta-tool (make-test-tool (repl-tools/symbol-metadata *nrepl-client-atom*))
          result (meta-tool {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= 1 (count (:res result))))
      (let [metadata-str (first (:res result))]
        (is (string? metadata-str))
        (is (str/includes? metadata-str ":name \"map\""))
        (is (str/includes? metadata-str ":ns \"clojure.core\""))
        (is (str/includes? metadata-str ":arglists"))
        (is (str/includes? metadata-str ":doc")))))

  (testing "Get metadata for non-existent symbol"
    (let [meta-tool (make-test-tool (repl-tools/symbol-metadata *nrepl-client-atom*))
          result (meta-tool {"symbol" "non-existent-symbol-blah"})]
      (is (= {:res ["nil"] :error? false} result)))))

(deftest symbol-documentation-test
  (testing "Get documentation for a core symbol"
    (let [doc-tool (make-test-tool (repl-tools/symbol-documentation *nrepl-client-atom*))
          result (doc-tool {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= 1 (count (:res result))))
      (let [doc-str (first (:res result))]
        (is (string? doc-str))
        ;; Check for arglists and part of the docstring
        (is (str/includes? doc-str "([f] [f coll] [f c1 c2]")) ; Check part of arglists
        (is (str/includes? doc-str "Returns a lazy sequence consisting of the result"))))) ; Check part of doc

  (testing "Get documentation for non-existent symbol"
    (let [doc-tool (make-test-tool (repl-tools/symbol-documentation *nrepl-client-atom*))
          result (doc-tool {"symbol" "non-existent-symbol-blah"})]
      (is (= {:res ["nil"] :error? false} result)))))

(deftest source-code-test
  (testing "Get source code for a core symbol"
    (let [src-tool (make-test-tool (repl-tools/source-code *nrepl-client-atom*))
          result (src-tool {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= 1 (count (:res result))))
      (let [src-str (first (:res result))]
        (is (string? src-str))
        (is (str/starts-with? src-str "(defn map")))))

  (testing "Get source for non-existent symbol"
    (let [src-tool (make-test-tool (repl-tools/source-code *nrepl-client-atom*))
          result (src-tool {"symbol" "non-existent-symbol-blah"})]
      (is (= {:res ["nil"] :error? false} result)))))

(deftest symbol-search-test
  (testing "Search for symbols using apropos"
    (let [search-tool (make-test-tool (repl-tools/symbol-search *nrepl-client-atom*))
          result (search-tool {"search-str" "map"})] ; Search for symbols containing "map"
      (is (false? (:error? result)))
      (is (vector? (:res result)))
      ;; Check if common symbols containing "map" are found
      (is (some #(= % "clojure.core/map") (:res result)))
      (is (some #(= % "clojure.core/mapcat") (:res result)))
      (is (some #(= % "clojure.core/pmap") (:res result)))
      ;; Ensure cider symbols are filtered out (as per implementation)
      (is (not-any? #(str/starts-with? % "cider.") (:res result)))))

  (testing "Search with no results"
    (let [search-tool (make-test-tool (repl-tools/symbol-search *nrepl-client-atom*))
          result (search-tool {"search-str" "xyz-non-existent-string-pattern"})]
      (is (= {:res ["No Matches Found"] :error? false} result)))))

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
      (is (some #(= % "clojure-mcp.repl-tools-test") (:res result))))))

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
        (is (= [] (:res result))))))) ;; Expect an empty vector


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
