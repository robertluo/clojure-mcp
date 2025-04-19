(ns clojure-mcp.tools.test-utils
  "Utility functions for testing the tool-system based tools."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [nrepl.server :as nrepl-server]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.test :refer [use-fixtures]]))

(defonce ^:dynamic *nrepl-server* nil)
(defonce ^:dynamic *nrepl-client-atom* nil)
(def ^:dynamic *test-file-path* "test_function_edit.clj")

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

(defn cleanup-test-file [f]
  (try
    (f)
    (finally
      #_(io/delete-file *test-file-path* true))))

;; Helper to invoke full tool function directly using the tool registration map
(defn make-tool-tester [tool-instance]
  "Takes a tool instance and returns a function that executes the tool directly.
   The returned function takes a map of tool inputs and returns a map with:
   {:result result :error? error-flag}"
  (let [reg-map (tool-system/registration-map tool-instance)
        tool-fn (:tool-fn reg-map)]
    (fn [inputs]
      (let [prom (promise)]
        (tool-fn nil inputs
                 (fn [res error?]
                   (deliver prom {:result res :error? error?})))
        @prom))))

;; Helper to test individual multimethod pipeline steps
(defn test-pipeline-steps [tool-instance inputs]
  "Executes the validation, execution, and formatting steps of the tool pipeline
   and returns the formatted result."
  (let [validated (tool-system/validate-inputs tool-instance inputs)
        execution-result (tool-system/execute-tool tool-instance validated)
        formatted-result (tool-system/format-results tool-instance execution-result)]
    formatted-result))

;; Apply fixtures in each test namespace
(defn apply-fixtures [test-namespace]
  (use-fixtures :once test-nrepl-fixture)
  (use-fixtures :each cleanup-test-file))