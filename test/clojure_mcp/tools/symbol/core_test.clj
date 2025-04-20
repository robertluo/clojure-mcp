(ns clojure-mcp.tools.symbol.core-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.symbol.core :as sut]
   [clojure-mcp.nrepl :as nrepl]
   [nrepl.server :as nrepl-server]
   [clojure.string :as str]))

;; Use the common test utils for nREPL server setup
(defonce ^:dynamic *nrepl-server* nil)
(defonce ^:dynamic *nrepl-client-atom* nil)

(defn test-nrepl-fixture [f]
  (let [server (nrepl.server/start-server :port 0)
        port (:port server)
        client (clojure-mcp.nrepl/create {:port port})
        client-atom (atom client)]
    (clojure-mcp.nrepl/start-polling client)
    ;; Load some namespaces for testing
    (clojure-mcp.nrepl/eval-code client "(require 'clojure.repl)" identity)
    (clojure-mcp.nrepl/eval-code client "(require 'clojure.string)" identity)
    (binding [*nrepl-server* server
              *nrepl-client-atom* client-atom]
      (try
        (f)
        (finally
          (clojure-mcp.nrepl/stop-polling client)
          (nrepl.server/stop-server server))))))

(use-fixtures :once test-nrepl-fixture)

;; Helper function to evaluate code for tests
(defn eval-helper [client code]
  (nrepl/tool-eval-code client code))

;; Mock the completions-fn and lookup-fn for testing
(defn prepare-test-client [client]
  (let [test-completions [{:candidate "map"} {:candidate "mapv"} {:candidate "mapcat"}]
        test-metadata {:name 'map
                       :doc "Returns a lazy sequence..."
                       :arglists '([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])}]
    (-> client
        (assoc :completions-fn (fn [_] test-completions))
        (assoc :lookup-fn (fn [_] test-metadata)))))

(deftest get-symbol-completions-test
  (testing "get-symbol-completions returns completions"
    (let [test-client (prepare-test-client @*nrepl-client-atom*)
          result (sut/get-symbol-completions test-client "ma")]
      (is (map? result))
      (is (vector? (:completions result)))
      (is (= ["map" "mapv" "mapcat"] (:completions result)))
      (is (false? (:error result)))))
  
  (testing "get-symbol-completions handles errors"
    (let [error-client (assoc @*nrepl-client-atom* :completions-fn (fn [_] (throw (Exception. "Test error"))))
          result (sut/get-symbol-completions error-client "ma")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "Test error")))))

(deftest get-symbol-metadata-test
  (testing "get-symbol-metadata returns metadata for a valid symbol"
    (let [test-client (prepare-test-client @*nrepl-client-atom*)
          result (sut/get-symbol-metadata test-client "map")]
      (is (map? result))
      (is (= (:name (:metadata result)) 'map))
      (is (false? (:error result)))))
  
  (testing "get-symbol-metadata handles missing symbols"
    (let [not-found-client (assoc @*nrepl-client-atom* :lookup-fn (fn [_] nil))
          result (sut/get-symbol-metadata not-found-client "nonexistent")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "not found")))))

(deftest get-symbol-documentation-test
  (testing "get-symbol-documentation returns doc and arglists for a valid symbol"
    (let [test-client (prepare-test-client @*nrepl-client-atom*)
          result (sut/get-symbol-documentation test-client "map")]
      (is (map? result))
      (is (= (:arglists result) '([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])))
      (is (= (:doc result) "Returns a lazy sequence..."))
      (is (false? (:error result)))))
  
  (testing "get-symbol-documentation handles missing symbols"
    (let [not-found-client (assoc @*nrepl-client-atom* :lookup-fn (fn [_] nil))
          result (sut/get-symbol-documentation not-found-client "nonexistent")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "not found")))))

(deftest get-source-code-test
  (testing "get-source-code returns source for a valid symbol"
    ;; Define a mock eval-fn that returns mock source code
    (let [mock-eval-fn (fn [_ _] "\"(defn test-fn [x] (* x x))\"")
          result (sut/get-source-code @*nrepl-client-atom* mock-eval-fn "test-fn")]
      (is (map? result))
      (is (= (:source result) "(defn test-fn [x] (* x x))"))
      (is (false? (:error result)))))
  
  (testing "get-source-code handles missing source"
    (let [mock-eval-fn (fn [_ _] "nil")
          result (sut/get-source-code @*nrepl-client-atom* mock-eval-fn "nonexistent")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "Source not found")))))

(deftest search-symbols-test
  (testing "search-symbols returns matches for a valid search"
    ;; Define a mock eval-fn that returns mock search results
    (let [mock-eval-fn (fn [_ _] "(clojure.core/map clojure.core/mapv)")
          result (sut/search-symbols @*nrepl-client-atom* mock-eval-fn "map")]
      (is (map? result))
      (is (vector? (:matches result)))
      (is (= ["clojure.core/map" "clojure.core/mapv"] (:matches result)))
      (is (false? (:error result)))))
  
  (testing "search-symbols handles empty results"
    (let [mock-eval-fn (fn [_ _] "()")
          result (sut/search-symbols @*nrepl-client-atom* mock-eval-fn "nonexistent")]
      (is (map? result))
      (is (vector? (:matches result)))
      (is (= ["No matches found"] (:matches result)))
      (is (false? (:error result)))))