(ns clojure-mcp.repl-tools
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.data.json :as json]
   [clojure.string :as string]))

(defn eval-code [service-atom]
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
                (nrepl/eval-code-help @service-atom form-str ;; Dereference the atom
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

;; ?? we could proboably set the ns here given an optional ns argument?
(defn current-namespace [service-atom] ;; Correct parameter name in definition
  {:name "current_namespace"
   :description "Returns the current namespace.

This can be helpful to verify which namespace future evaluations are going to occur in.

Whenever, you use Clojure eval the evaluation occurs in the current namespace.
The inital namespace is 'user. 
 
When you alter the namespaces with (ns example.core) or (in-ns
'example.utils) you change the state of the REPL. And further evaluations will
now take place in that namespace."
   :schema (json/write-str {:type :object})
   :tool-fn (fn [_ _ clj-result-k]
              (let [res (some-> service :clojure-mcp.nrepl/state deref :current-ns)]
                (clj-result-k
                 (if res [res] [])
                 (if res false true))))})

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
              (let [res (nrepl/completions @service-atom (get arg-map "prefix"))] ;; Dereference atom
                (clj-result-k (mapv pr-str res) false)))})

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
              (let [res (nrepl/lookup @service-atom (get arg-map "symbol"))] ;; Dereference atom
                (clj-result-k
                 [(if res
                    (with-out-str (clojure.pprint/pprint res))
                    "nil")]
                 false)))})

(defn symbol-documentation [service-atom]
  {:name "symbol_documentation"
   :description "Returns the documentation for the symbol. Extracts the doc string and includes the function's arglists from the symbol metadata."
   :schema (json/write-str {:type :object
                            :properties {:symbol {:type :string}}
                            :required [:symbol]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [res (nrepl/lookup @service-atom (get arg-map "symbol")) ;; Dereference atom
                    arglists (:arglists res)
                    doc (:doc res)
                    combined (str arglists "\n" doc)]
                (clj-result-k
                 (if res
                   [combined]
                   ["nil"])
                 false)))})

(defn source-code [service-atom]
  {:name "source_code"
   :description "Returns the source code for a given symbol using clojure.repl/source.
Usage: Provide a string representing the symbol, e.g. \"map\" or \"clojure.core/map\".
The implementation calls `(clojure.repl/source-fn (symbol ~string))` as a hint for retrieving the source code."
   :schema (json/write-str {:type :object
                            :properties {:symbol {:type :string}}
                            :required [:symbol]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [sym-str (get arg-map "symbol")
                    result (nrepl/tool-eval-code
                            @service-atom ;; Dereference atom
                            (pr-str `(clojure.repl/source-fn (symbol ~sym-str))))
                    result-val (read-string result)]
                (clj-result-k
                 [(if (nil? result-val)
                    "nil"
                    result-val)]
                 false)))})

(defn symbol-search [service-atom]
  {:name "symbol-search"
   :description "Returns a sequence of all public definitions whose names contain the given search string in all currently loaded namespaces using clojure.repl/apropos.
Usage: Provide a search-string which would be a substring of the found definitions"
   :schema (json/write-str {:type :object
                            :properties {:search-str {:type :string}}
                            :required [:search-str]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [partial (get arg-map "search-str")
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
                 false)))})

(comment
  (def client-atom (atom (nrepl/create {:port 54171}))) ;; Use an atom here for consistency in testing
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

  (eval-tester {"expression" (pr-str '(+ 1 2))})

   
  )
