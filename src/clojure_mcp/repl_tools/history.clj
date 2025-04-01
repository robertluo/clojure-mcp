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