(ns clojure-mcp.repl-tools
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.linting :as linting]
   [clojure.data.json :as json]
   [clojure.string :as string]))

(defn eval-history-push
  "Pushes a form string to the evaluation history.
   Safely handles nil state-atom."
  [state-atom form-str]
  (when state-atom
    (swap! state-atom update ::eval-history conj form-str)))

(defn eval-history-reset 
  "Resets the evaluation history.
   Safely handles nil state-atom."
  [state-atom]
  (when state-atom
    (swap! state-atom assoc ::eval-history nil)))


;; Eval results formatting
;; The goal is to make it clear for the LLM to understand

;; EXAMPLE OUTPUT

;;
;; Testing mixed content types:
;; => nil
;; *===============================================*
;; => {:tag :div,
;;  :attrs {},
;;  :content
;;  ["Some text"
;;   {:tag :br, :attrs {}, :content []}
;;   123
;;   {:tag :span, :attrs {}, :content ["More text"]}]}
;; *===============================================*
;; => nil
;;

(defn partition-outputs [outputs]
  (when (not-empty outputs)
    (let [[non-val-parts [val & xs]] (split-with #(not= :value (first %)) outputs)]
      (cons (cond-> (vec non-val-parts)
              val (conj val))
            (partition-outputs xs)))))

(defn format-value [[k v]]
  (string/trim-newline
   (if (= k :value)
     (str "=> " v (if (<= nrepl/truncation-length (count v))
                    " ... RESULT TRUNCATED"
                    ""))
     v)))

(defn format-eval-outputs [outputs]
  (->> outputs
       (map format-value)
       (string/join "\n")))

(defn partition-and-format-outputs [outputs]
  (interpose "*===============================================*"
             (mapv format-eval-outputs (partition-outputs outputs))))

(defn eval-code [service-atom]
  {:name "clojure_eval"
   :description "Takes a Clojure Expression and evaluates it in the current namespace. For example, providing \"(+ 1 2)\" will evaluate to 3. 

This tool is intended to execute Clojure code. This is very helpful for verifying that code is working as expected. It's also helpful for REPL driven development.

If you send multiple expressions they will all be evaluated individually and their output will be clearly partitioned.

If the value that is returned is too long it will be truncated."
   :schema (json/write-str {:type :object
                            :properties {:expression {:type :string}}
                            :required [:expression]})
   ;; this is starting to look like we could make it a chain of calls
   ;; to add features like linting
   
   ;; The eval-code tool-fn takes an exchange and arg-map
   ;; Arguments 
   ;; * exchange - ignored
   ;; * arg-map - map with string keys representing the mcp tool call args
   ;; * clj-result-k - a function that takes a vector of strings and boolean
   ;;                  that represents wether an error occured during evaluation
   :tool-fn (fn tool-fn
              [_ arg-map clj-result-k]
              (let [outputs (atom []) ;; Atom to store prefixed output strings
                    error-occurred (atom false) ;; Atom to track if any error happened
                    form-str (get arg-map "expression")
                    [service-valid? service-error] (ensure-service-atom service-atom)
                    linted (linting/lint form-str)
                    add-output! (fn [prefix value] (swap! outputs conj [prefix value]))
                    finish (fn [_]
                             (clj-result-k
                              (partition-and-format-outputs @outputs)
                              @error-occurred))]

                ;; Check service connection first
                (when-not service-valid?
                  (reset! error-occurred true)
                  (add-output! :error service-error)
                  (finish nil)
                  (reduced nil))

                ;; Add linter output if present
                (when linted
                  (add-output! :lint (:report linted))
                  (when (:error? linted)
                    (reset! error-occurred true)))

                ;; If linter found critical errors or service not available, finish early
                (if @error-occurred
                  (finish nil)
                  (do
                    (eval-history-push (::nrepl/state @service-atom) form-str)
                    (nrepl/eval-code-help @service-atom form-str ;; Dereference the atom
                                          (->> identity
                                               (nrepl/out-err
                                                #(add-output! :out %)
                                                #(add-output! :err %))
                                               ;; TODO we need to limit the size here
                                               (nrepl/value #(add-output! :value %))
                                               (nrepl/done finish)
                                               (nrepl/error (fn [_]
                                                              (reset! error-occurred true)
                                                              ;; Optionally add an error marker to output
                                                              (add-output! "ERROR: " "Evaluation failed")
                                                              (finish _)))))))
                ))})

;; ?? we could proboably set the ns here given an optional ns argument?
(defn current-namespace [service-atom] ;; Correct parameter name in definition
  {:name "current_namespace"
   :description "Returns the current namespace. This tool is intended for the LLM agent (Yellow Lamb) to check the current evaluation context. Use this tool when the agent is uncertain about which namespace is active and needs to verify the context before proceeding with further code evaluations."
   :schema (json/write-str {:type :object})
   :tool-fn (fn [_ _ clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [res (some-> @service-atom :clojure-mcp.nrepl/state deref :current-ns)]
                    (clj-result-k
                     (if res [res] ["No current namespace found"])
                     (if res false true))))))})

;; ?? we could take an ns argument to scope the completions somewhere else
;; ?? we could return the arglist and doc specs along with this
(defn symbol-completions [service-atom]
  {:name "symbol_completions"
   :description "Provides a list of completion candidates for the current namespace.

For example if you provide the prefix of a symbol like map this will return a list of possible 
symbols that could start with that prefix in the `current_namespace`.

This is not an exhaustive list, some completions may be missing."
   :schema (json/write-str {:type :object
                            :properties {:prefix {:type :string}}
                            :required [:prefix]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [prefix (some-> (get arg-map "prefix") string/trim) ;; Trim input
                        completions-raw (nrepl/completions @service-atom prefix) ;; Dereference atom
                        ;; Extract just the candidate name
                        candidates (mapv :candidate completions-raw)] ;; Remove pr-str
                    (clj-result-k candidates false)))))})

(defn symbol-metadata [service-atom] ;; Changed parameter name
  {
   :name "symbol_metadata"
   :description "Returns the complete metadata for the symbol.

the most important data is likely to be the
 - :arglists that shows the shape of the arguments that the function takes
 - :doc which holds the docstring for the function/var

Example result:
   {:added \"1.0\",
 :ns \"clojure.core\",
 :name \"map\",
 :file
 \"jar:file:/Users/bozhidar/.m2/repository/org/clojure/clojure/1.10.1/clojure-1.10.1.jar!/clojure/core.clj\",
 :static true,
 :column 1,
 :line 2727,
 :arglists \"([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])\",
 :doc
 \"Returns a lazy sequence consisting of the result of applying f to\\n  the set of first items of each coll, followed by applying f to the\\n  set of second items in each coll, until any one of the colls is\\n  exhausted.  Any remaining items in other colls are ignored. Function\\n  f should accept number-of-colls arguments. Returns a transducer when\\n  no collection is provided.\"}"
   :schema (json/write-str {:type :object
                            :properties {:symbol {:type :string}}
                            :required [:symbol]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [sym (some-> (get arg-map "symbol") string/trim) ;; Trim input
                        res (nrepl/lookup @service-atom sym)] ;; Dereference atom
                    (clj-result-k
                     [(if res
                        (with-out-str (clojure.pprint/pprint res))
                        "nil")]
                     false)))))})

(defn symbol-documentation [service-atom]
  {:name "symbol_documentation"
   :description "Returns the documentation for the symbol. Extracts the doc string and includes the function's arglists from the symbol metadata."
   :schema (json/write-str {:type :object
                            :properties {:symbol {:type :string}}
                            :required [:symbol]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [sym (some-> (get arg-map "symbol") string/trim) ;; Trim input
                        res (nrepl/lookup @service-atom sym) ;; Dereference atom
                        arglists (:arglists res)
                        doc (:doc res)
                        combined (str arglists "\n" doc)]
                    (clj-result-k
                     (if res
                       [combined]
                       ["nil"])
                     false)))))})

(defn source-code [service-atom]
  {:name "source_code"
   :description "Returns the source code for a given symbol using clojure.repl/source.
Usage: Provide a string representing the symbol, e.g. \"map\" or \"clojure.core/map\".
The implementation calls `(clojure.repl/source-fn (symbol ~string))` as a hint for retrieving the source code."
   :schema (json/write-str {:type :object
                            :properties {:symbol {:type :string}}
                            :required [:symbol]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [sym-str (some-> (get arg-map "symbol") string/trim) ;; Trim input
                        result (nrepl/tool-eval-code
                                @service-atom ;; Dereference atom
                                (pr-str `(clojure.repl/source-fn (symbol ~sym-str))))
                        result-val (when result (read-string result))] ;; Handle nil result
                    (clj-result-k
                     [(if (nil? result-val)
                        "nil"
                        result-val)]
                     false)))))})

(defn symbol-search [service-atom]
  {:name "symbol-search"
   :description "Returns a sequence of all public definitions whose names contain the given search string in all currently loaded namespaces using clojure.repl/apropos.
Usage: Provide a search-string which would be a substring of the found definitions"
   :schema (json/write-str {:type :object
                            :properties {:search-str {:type :string}}
                            :required [:search-str]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [partial (some-> (get arg-map "search-str") string/trim) ;; Trim input
                        res (or
                             (some->> (nrepl/tool-eval-code
                                       @service-atom ;; Dereference atom
                                       (pr-str `(clojure.repl/apropos ~partial)))
                                      read-string
                                      (map str)
                                      (remove #(string/starts-with? % "cider.nrepl"))
                                      vec
                                      not-empty)
                             ["No Matches Found"])]
                    (clj-result-k
                     res
                     false)))))})

(defn list-namespaces [service-atom]
  {:name "clojure_list_namespaces"
   :description "Returns a list of all currently loaded namespaces as strings. Use this to see what libraries are loaded and search their capabilities."
   :schema (json/write-str {:type :object}) ;; No arguments needed
   :tool-fn (fn [_ _ clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (let [code "(map str (sort (map ns-name (all-ns))))"
                        result-str (nrepl/tool-eval-code @service-atom code)
                        result-val (when result-str (read-string result-str))]
                    (clj-result-k
                     (if result-val
                       (vec result-val) ;; Ensure result is a vector
                       ["Error retrieving namespaces"]) ;; Fallback message
                     (nil? result-val))))))})

(defn list-vars-in-namespace [service-atom]
  {:name "clojure_list_vars_in_namespace"
   :description "Returns a list of maps, each containing metadata (:arglists, :doc, :name, :ns) for public vars defined in a given namespace. This can give you an overview of the functions that a namespace providesand how ot use them."
   :schema (json/write-str {:type :object
                            :properties {:namespace {:type :string
                                                     :description "The fully qualified name of the namespace (e.g. clojure.string)."}}
                            :required [:namespace]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
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
                      (clj-result-k (mapv pr-str result-val) false))))))})

(defn eval-history [service-atom]
  {:name "clojure_eval_history"
   :description "Returns the last N evaluated expressions from the REPL history. This is useful when you need to look at a previous evaluation."
   :schema (json/write-str {:type :object
                            :properties {:number-to-fetch {:type "integer"
                                                           :description "The number of history items to retrieve from the end."}}
                            :required [:number-to-fetch]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [[service-valid? service-error] (ensure-service-atom service-atom)]
                (if-not service-valid?
                  (clj-result-k [service-error] true)
                  (try
                    (let [n-str (get arg-map "number-to-fetch")
                          n (Integer/parseInt n-str)
                          history (get @(::nrepl/state @service-atom) ::eval-history [])
                          items-to-return (take n history)]
                      (clj-result-k (vec items-to-return) false))
                    (catch NumberFormatException _
                      (clj-result-k ["Invalid 'number-to-fetch'. Please provide an integer."] true))
                    (catch Exception e
                      (clj-result-k [(str "Error fetching history: " (ex-message e))] true))))))})


(comment
  (def client-atom (atom (nrepl/create {:port 7888})))
  (nrepl/start-polling @client-atom)
  (nrepl/stop-polling @client-atom)
   
  (defn make-test-tool [{:keys [tool-fn] :as tool-map}]
    (fn [arg-map]
      (let [prom (promise)]
        (tool-fn nil arg-map 
                 (fn [res error]
                   (deliver prom {:res res :error error})))
        @prom)))


  ;; Testing the eval-code tool
  (def eval-tester (make-test-tool (eval-code client-atom))) ;; Pass the atom

  (eval-tester {"expression" (pr-str '(do
                                        (require 'nrepl.util.print)
                                        #_(set! nrepl.middleware.print/*print-fn* nrepl.util.print/pprint)))})
  (eval-tester {"expression" (str
                              (pr-str  '(map #(range %) (range 50)))
                              (pr-str '(println 55))
                              (pr-str '(+ 1 2))(pr-str '(+ 1 2))(pr-str '(+ 1 2)))})

   
  )
