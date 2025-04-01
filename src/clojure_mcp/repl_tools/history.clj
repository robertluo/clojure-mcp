(ns clojure-mcp.repl-tools.history
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.data.json :as json]))

(defn eval-history [service-atom]
  {:name "clojure_eval_history"
   :description "Returns the last N evaluated expressions from the REPL history. This is useful when you need to look at a previous evaluation."
   :schema (json/write-str {:type :object
                            :properties {:number-to-fetch {:type "integer"
                                                           :description "The number of history items to retrieve from the end."}}
                            :required [:number-to-fetch]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (try
                (let [n-str (get arg-map "number-to-fetch")
                      n (Integer/parseInt n-str)
                      history (get @(::nrepl/state @service-atom) :clojure-mcp.repl-tools/eval-history [])
                      items-to-return (take n history)]
                  (clj-result-k (vec items-to-return) false))
                (catch NumberFormatException _
                  (clj-result-k ["Invalid 'number-to-fetch'. Please provide an integer."] true))
                (catch Exception e
                  (clj-result-k [(str "Error fetching history: " (ex-message e))] true))))})

(comment
  ;; === Examples of using the history tools ===
  
  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  
  ;; Test helper function
  (defn make-test-tool [{:keys [tool-fn] :as _tool-map}]
    (fn [arg-map]
      (let [prom (promise)]
        (tool-fn nil arg-map 
                 (fn [res error]
                   (deliver prom {:res res :error error})))
        @prom)))
  
  ;; First add some history by evaluating expressions
  (require '[clojure-mcp.repl-tools.eval :as eval-tools])
  (def eval-tester (make-test-tool (eval-tools/eval-code client-atom)))
  (eval-tester {"expression" "(+ 1 1)"})
  (eval-tester {"expression" "(+ 2 2)"})
  (eval-tester {"expression" "(+ 3 3)"})
  
  ;; Now test fetching history
  (def history-tester (make-test-tool (eval-history client-atom)))
  
  ;; Fetch last 2 items
  (history-tester {"number-to-fetch" "2"})
  
  ;; Fetch more items than available
  (history-tester {"number-to-fetch" "10"})
  
  ;; Test with invalid input
  (history-tester {"number-to-fetch" "abc"})
  
  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom)
)