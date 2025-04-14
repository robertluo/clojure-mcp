(ns clojure-mcp.repl-tools.top-level-form-edit
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure-mcp.linting :as linting]
   [clojure-mcp.utils.emacs-integration :as emacs]))

(defn is-top-level-form?
  "Check if a sexp is a top-level definition with a specific tag and name.
   
   Arguments:
   - zloc: The zipper location to check
   - tag: The definition tag (e.g., 'defn, 'def)
   - dname: The name of the definition
   
   Returns true if the sexp matches the pattern (tag dname ...)"
  [zloc tag dname]
  (try
    (let [sexp (z/sexpr zloc)]
      (and (list? sexp)
           (= (first sexp) (symbol tag))
           (= (second sexp) (symbol dname))))
    (catch Exception _ false)))

(defn find-top-level-form
  "Find a top-level form with a specific tag and name in a zipper.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The tag name as a string (e.g., \"defn\", \"def\", \"ns\")
   - dname: The name of the definition as a string
   
   Returns the zipper location of the matched form, or nil if not found."
  [zloc tag dname]
  (loop [loc zloc]
    (cond
      (nil? loc) nil
      (is-top-level-form? loc tag dname) loc
      :else (recur (z/right loc)))))

(defn replace-top-level-form
  "Replace a top-level form with a new string.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def, 'ns)
   - name: The name of the form
   - replacement-str: The string with the new form definition
   
   Returns the updated zipper, or nil if the form was not found."
  [zloc tag name replacement-str]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (z/replace form-zloc (p/parse-string replacement-str))))

(defn row-col->offset [s target-row target-col]
  (loop [lines (clojure.string/split-lines s)
         current-row 1
         offset 0]
    (if (or (empty? lines) (>= current-row target-row))
      (+ offset target-col) ; Add (col - 1) for 0-based index
      (recur (next lines)
             (inc current-row)
             (+ offset (count (first lines)) 1)))))

(defn zloc-offsets [source-str positions]
  (mapv (fn [[row col]] (row-col->offset source-str row col))
        positions))

(defn replace-form-in-file
  "Replace a top-level form in a file with a new implementation.
   
   Arguments:
   - name: Name of the form to replace (string or symbol)
   - file-path: Path to the file
   - tag: The type of the form (defn, def, ns, etc.)
   - new-form-str: String with the new form implementation
   
   Returns:
   - [start end] offsets if the form was replaced successfully
   - false if the form was not found
   - A map with :error and :message keys if there were errors"
  [name file-path tag new-form-str]
  (try
    (let [name (if (string? name) name (str name))
          tag (if (string? tag) tag (str tag))
          ;; Lint the code first
          lint-result (linting/lint new-form-str)]
      ;; Fail early if there are linting issues
      (if lint-result
        {:error :lint-failure 
         :message (str "Linting issues in replacement form:\n" (:report lint-result))}
        
        ;; Continue with replacement if no linting issues
        (try
          (let [file-content (slurp file-path)
                zloc (z/of-string file-content {:track-position? true})
                updated-zloc (replace-top-level-form zloc tag name new-form-str)]
            (if updated-zloc
              (let [positions (z/position-span updated-zloc)
                    final-source (z/root-string updated-zloc)
                    offsets (zloc-offsets final-source positions)]
                (spit file-path (z/root-string updated-zloc))
                offsets)
              false))
          (catch java.io.FileNotFoundException _
            {:error :file-not-found
             :message (str "File not found: " file-path)})
          (catch java.io.IOException e
            {:error :io-error
             :message (str "IO error while reading/writing file: " (.getMessage e))}))))
    (catch Exception e
      {:error :unknown-error
       :message (str "Error: " (.getMessage e) "\n" (ex-data e))})))

(defn top-level-form-edit-tool [_service-atom]
  {:name "top_level_form_edit"
   :description "Edits any top-level form in a Clojure file, replacing it with a new implementation.
   
This tool allows you to modify any top-level form (def, defn, ns, deftest etc.) 
in source files without manually editing the files. It preserves formatting and whitespace 
in the rest of the file."
   :schema
   (json/write-str
    {:type :object
     :properties
     {:form_name {:type :string
                  :description "The name of the form to edit (e.g., function name, var name, namespace name)"}
      :file_path {:type :string
                  :description "Path to the file containing the form"}
      :form_type {:type :string
                  :description "The type of form (e.g., 'defn', 'def', 'ns', 'deftest' ...). Required."}
      :new_implementation {:type :string
                           :description "String with the new form implementation"}}
     :required [:form_name :file_path :form_type :new_implementation]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [form-name (get arg-map "form_name")
                    file-path (get arg-map "file_path")
                    form-type (get arg-map "form_type")
                    new-impl (get arg-map "new_implementation")]
                (if-let [lint-result (linting/lint new-impl)]
                  (let [formatted-report
                        (str "Cannot update form '" form-name "' of type '"
                             form-type "':\n\n" 
                             (linting/format-lint-warnings lint-result) 
                             "\n\nPlease fix these issues before updating the form.")]
                    (clj-result-k [formatted-report] true))
                  (let [result (replace-form-in-file form-name file-path form-type new-impl)]
                    (cond
                      (vector? result)
                      (do
                        (try
                          (emacs/highlight-region file-path (first result) (second result))
                          (catch Exception e
                            (println "Warning: Failed to highlight form in Emacs:" (.getMessage e))))
                        
                        (clj-result-k [(str "Successfully updated form '" form-name "' in file " file-path)] 
                                    false))
                      
                      ;; Form not found
                      (false? result)
                      (clj-result-k
                       [(str "Could not find form '" form-name "' of type '" form-type "' in file " file-path)]
                       true)
                      
                      ;; Map result indicates specific error
                      (map? result)
                      (let [error-type (name (:error result))
                            error-msg (:message result)
                            formatted-error (str "Error (" error-type ") updating form '" form-name "': " error-msg)]
                        (clj-result-k [formatted-error] true))
                      
                      ;; Other error (string result means error from older implementation)
                      :else
                      (clj-result-k [(str "Error updating form: " result)]
                                    true))))))})


#_(let [result (replace-form-in-file "square-plus"
                            "/Users/bruce/workspace/llempty/clojure-mcp/src/user.clj"
                            "defn"
                            "(defn square-plus [n consta & [multiplier]]
  (+ (* n n (or multiplier 1)) consta))"
                            )
      ]

  (if (vector? result)
      (emacs/highlight-region "/Users/bruce/workspace/llempty/clojure-mcp/src/user.clj"
                              (first result)
                              (second result))
      result)
  
  )

(comment
  ;; Example usage


  (count   "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))
")
  (def sample "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

(def test-var 42)

(ns test.namespace
  (:require [clojure.string :as str]))")

  ;; Test finding a function
  (z/position-span (find-top-level-form (z/of-string user/sample {:track-position? true}) "def" "test-var"))

  (z/position-span
   (replace-top-level-form (z/of-string user/sample {:track-position? true}) "def" "test-var"

                           "(defn)"))





  (row-col->offset user/sample 5 1)

  ;; Test replacing a function
  (z/root-string 
   (replace-top-level-form 
    (z/of-string sample) 
    "defn"
    "foo"
    "(defn foo [] 'updated-value)"))
  
  ;; Test replacing a def
  (z/root-string 
   (replace-top-level-form 
    (z/of-string sample) 
    "def"
    "test-var"
    "(def test-var :new-value)"))
  
  ;; Test replacing a namespace
  (z/root-string 
   (replace-top-level-form 
    (z/of-string sample) 
    "ns"
    "test.namespace"
    "(ns test.namespace (:require [clojure.string :as str] [clojure.set :as set]))"))
  
  ;; === Examples of using the top-level form edit tool ===
  
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
  
  ;; Testing the top-level form edit tool
  (def edit-tester (make-test-tool (top-level-form-edit-tool client-atom)))
  
  ;; Edit a function in a file
  (edit-tester {"form_name" "example-function"
                "file_path" "/path/to/file.clj"
                "form_type" "defn"
                "new_implementation" "(defn example-function [x y]\n  (+ x y))"})
  
  ;; Edit a namespace declaration
  (edit-tester {"form_name" "my.project.core"
                "file_path" "/path/to/core.clj"
                "form_type" "ns"
                "new_implementation" "(ns my.project.core\n  (:require [clojure.string :as str]))"})
  
  ;; Edit a variable definition
  (edit-tester {"form_name" "config"
                "file_path" "/path/to/config.clj"
                "form_type" "def"
                "new_implementation" "(def config {:port 8080 :host \"localhost\"})"})
  
  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom)
  )
