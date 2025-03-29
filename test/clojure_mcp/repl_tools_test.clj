(ns clojure-mcp.repl-tools-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.repl-tools :as repl-tools]
   [nrepl.server :as nrepl-server]
   [clojure.string :as str]))

(defonce ^:dynamic *nrepl-server* nil)
(defonce ^:dynamic *nrepl-client-atom* nil)

(defn test-nrepl-fixture [f]
  (let [server (nrepl-server/start-server :port 0) ; Use port 0 for dynamic port assignment
        port (:port server)
        client (nrepl/create {:port port})
        client-atom (atom client)]
    (nrepl/start-polling client)
    ;; Ensure clojure.repl is loaded for apropos/source-fn used in tools
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
      (is (= {:res ["3"] :error? false} result))))

  (testing "Evaluation with output"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(println \"hello\")"})]
      ;; Output might vary slightly (e.g., newline), check contains
      (is (false? (:error? result)))
      (is (= ["nil"] (->> result :res (filter #(not (str/starts-with? % "OUT:"))) vec)))
      (is (some #(str/includes? % "OUT: hello") (:res result)))))

  (testing "Evaluation with error"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(throw (Exception. \"test error\"))"})]
      (is (true? (:error? result)))
      ;; Error message might be complex, just check it contains the core message
      (is (some #(str/includes? % "ERR: ") (:res result)))
      (is (some #(str/includes? % "test error") (:res result)))))

  (testing "Evaluation resulting in nil"
    (let [eval-tool (make-test-tool (repl-tools/eval-code *nrepl-client-atom*))
          result (eval-tool {"expression" "(identity nil)"})]
      (is (= {:res ["nil"] :error? false} result)))))

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
      (is (some #(= % "\"map\"") (:res result)))
      (is (some #(= % "\"mapcat\"") (:res result)))
      (is (some #(= % "\"max\"") (:res result))))))

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
