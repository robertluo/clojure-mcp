(ns clojure-mcp.repl-tools.symbol
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.data.json :as json]
   [clojure.string :as string]))

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
              (let [prefix (some-> (get arg-map "prefix") string/trim) ;; Trim input
                    completions-raw (nrepl/completions @service-atom prefix) ;; Dereference atom
                    ;; Extract just the candidate name
                    candidates (mapv :candidate completions-raw)] ;; Remove pr-str
                (clj-result-k candidates false)))})

(defn symbol-metadata [service-atom]
  {:name "symbol_metadata"
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
              (let [sym (some-> (get arg-map "symbol") string/trim) ;; Trim input
                    res (nrepl/lookup @service-atom sym)] ;; Dereference atom
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
              (let [sym (some-> (get arg-map "symbol") string/trim) ;; Trim input
                    res (nrepl/lookup @service-atom sym) ;; Dereference atom
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
              (let [sym-str (some-> (get arg-map "symbol") string/trim) ;; Trim input
                    result (nrepl/tool-eval-code
                            @service-atom ;; Dereference atom
                            (pr-str `(clojure.repl/source-fn (symbol ~sym-str))))
                    result-val (when result (read-string result))] ;; Handle nil result
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
                 false)))})