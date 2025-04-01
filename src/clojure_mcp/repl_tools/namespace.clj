(ns clojure-mcp.repl-tools.namespace
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.data.json :as json]
   [clojure.string :as string]))

(defn current-namespace [service-atom]
  {:name "current_namespace"
   :description "Returns the current namespace. This tool is intended for the LLM agent (Yellow Lamb) to check the current evaluation context. Use this tool when the agent is uncertain about which namespace is active and needs to verify the context before proceeding with further code evaluations."
   :schema (json/write-str {:type :object})
   :tool-fn (fn [_ _ clj-result-k]
              (let [res (some-> @service-atom :clojure-mcp.nrepl/state deref :current-ns)]
                (clj-result-k
                 (if res [res] ["No current namespace found"])
                 (if res false true))))})

(defn list-namespaces [service-atom]
  {:name "clojure_list_namespaces"
   :description "Returns a list of all currently loaded namespaces as strings. Use this to see what libraries are loaded and search their capabilities."
   :schema (json/write-str {:type :object}) ;; No arguments needed
   :tool-fn (fn [_ _ clj-result-k]
              (let [code "(map str (sort (map ns-name (all-ns))))"
                    result-str (nrepl/tool-eval-code @service-atom code)
                    result-val (when result-str (read-string result-str))]
                (clj-result-k
                 (if result-val
                   (vec result-val) ;; Ensure result is a vector
                   ["Error retrieving namespaces"]) ;; Fallback message
                 (nil? result-val))))})

(defn list-vars-in-namespace [service-atom]
  {:name "clojure_list_vars_in_namespace"
   :description "Returns a list of maps, each containing metadata (:arglists, :doc, :name, :ns) for public vars defined in a given namespace. This can give you an overview of the functions that a namespace providesand how ot use them."
   :schema (json/write-str {:type :object
                            :properties {:namespace {:type :string
                                                     :description "The fully qualified name of the namespace (e.g. clojure.string)."}}
                            :required [:namespace]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [ns-str (some-> (get arg-map "namespace") string/trim)
                    code (pr-str `(when-let [ns-obj# (find-ns (symbol ~ns-str))]
                                    (->> (ns-publics ns-obj#)
                                         vals ;; Get the var objects
                                         (map meta) ;; Get metadata for each var
                                         (map #(select-keys % [:arglists :doc :name :ns])) ;; Select desired keys
                                         (map #(update % :ns str))
                                         (sort-by :name) ;; Sort by name for consistent order
                                         vec))) ;; Convert to vector
                    result-str (nrepl/tool-eval-code @service-atom code)
                    result-val (try
                                 (when result-str (read-string result-str)) ;; Read the string back into Clojure data
                                 (catch Exception _ nil))] ;; Handle potential read-string errors
                (cond
                  ;; Case 1: nREPL evaluation failed entirely
                  (nil? result-str)
                  (clj-result-k ["Error evaluating code to list vars."] true)

                  ;; Case 2: Namespace not found OR read-string failed
                  (nil? result-val)
                  (clj-result-k [(str "Namespace '" ns-str "' not found or failed to parse result.")] true)

                  ;; Case 3: Success (namespace found, potentially empty list of vars)
                  :else
                  (clj-result-k (mapv pr-str result-val) false))))})