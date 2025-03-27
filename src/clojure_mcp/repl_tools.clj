(ns clojure-mcp.repl-tools
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.data.json :as json]))

(defn eval-code [service]
  {:name "clojure_eval"
   :description "Takes a Clojure Expression and evaluates it in the 'user namespace. 
For example: provide \"(+ 1 2)\" and this will evaluate that and return 3"
   :schema (json/write-str {:type :object
                            :properties {:expression {:type :string}}
                            :required [:expression]})
   ;; The eval-code tool-fn takes an exchange and arg-map
   ;; Arguments 
   ;; * exchange - ignored
   ;; * arg-map - map with string keys representing the mcp tool call args
   ;; * clj-result-k - a function that takes a vector of strings and boolean
   ;;                  that represents wether an error occured during evaluation
   :tool-fn (fn tool-fn
              [_ arg-map clj-result-k]
              (let [data (atom {:result []
                                :error false})
                    form-str (get arg-map "expression")
                    finish (fn [_]
                             (clj-result-k
                              (cond-> (:result @data)
                                (:out @data) (conj (str "OUT: " (:out @data)))
                                (:err @data) (conj (str "ERR: " (:err @data))))
                              (:error @data)))]
                (nrepl/eval-code-help service form-str
                                      (->> identity
                                           (nrepl/out-err
                                            #(swap! data update :out (fn [x] (str % x)))
                                            #(swap! data update :err (fn [x] (str % x))))
                                           ;; TODO we need to limit the size here
                                           (nrepl/value #(swap! data update :result conj %))
                                           (nrepl/done finish)
                                           (nrepl/error (fn [_]
                                                          (swap! data assoc :error true)
                                                          (finish _)))))))})


