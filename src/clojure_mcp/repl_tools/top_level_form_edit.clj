(ns clojure-mcp.repl-tools.top-level-form-edit
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure-mcp.linting :as linting]))

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

(defn edit-form-in-file
  "Edit a top-level form in a file, replacing it with a new implementation.
   
   Arguments:
   - file-path: Path to the file containing the form
   - tag: The type of the form (defn, def, ns, etc.)
   - name: Name of the form to replace
   - new-form-str: String with the new form implementation
   
   Returns the updated file content as a string."
  [file-path tag name new-form-str]
  (let [file-content (slurp file-path)
        zloc (z/of-string file-content)
        updated-zloc (replace-top-level-form zloc tag name new-form-str)]
    (if updated-zloc
      (z/root-string updated-zloc)
      file-content)))


(defn replace-form-in-file
  "Replace a top-level form in a file with a new implementation.
   
   Arguments:
   - name: Name of the form to replace (string or symbol)
   - file-path: Path to the file
   - tag: The type of the form (defn, def, ns, etc.)
   - new-form-str: String with the new form implementation
   
   Returns:
   - true if the form was replaced successfully
   - false if the form was not found
   - A string with the linting error message if there were linting issues"
  [name file-path tag new-form-str]
  (try
    (let [name (if (string? name) name (str name))
          tag (if (string? tag) tag (str tag))
          ;; Lint the code first
          lint-result (linting/lint new-form-str)]
      
      ;; Fail early if there are linting issues
      (if lint-result
        (str "Linting issues in replacement form:\n" (:report lint-result))
        
        ;; Continue with replacement if no linting issues
        (let [file-content (slurp file-path)
              zloc (z/of-string file-content)
              updated-zloc (replace-top-level-form zloc tag name new-form-str)]
          (if updated-zloc
            (do
              (spit file-path (z/root-string updated-zloc))
              true)
            false))))
    (catch Exception e
      (str "Error: " (.getMessage e)))))

(defn top-level-form-edit-tool [_service-atom]
  {:name "top_level_form_edit"
   :description "Edits any top-level form in a Clojure file, replacing it with a new implementation.
   
This tool allows you to modify any top-level form (def, defn, ns, deftest etc.) 
in source files without manually editing the files. It preserves formatting and whitespace 
in the rest of the file."
   :schema (json/write-str {:type :object
                            :properties {:form_name {:type :string
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
                    new-impl (get arg-map "new_implementation")
                    ;; Perform linting first
                    lint-result (linting/lint new-impl)]
                
                ;; Check for linting issues before proceeding
                (if lint-result
                  ;; If there are linting issues, fail with error message
                  (let [lint-report (:report lint-result)
                        formatted-report (str "Cannot update form '" form-name "' due to linting issues:\n\n" 
                                             lint-report 
                                             "\n\nPlease fix these issues before updating the form.")]
                    (clj-result-k [formatted-report] true))
                  
                  ;; If no linting issues, proceed with the update
                  (let [result (replace-form-in-file
                                 form-name
                                 file-path
                                 form-type
                                 new-impl)]
                    (cond
                      ;; Success case
                      (true? result)
                      (clj-result-k [(str "Successfully updated form '" form-name "' in file " file-path)] 
                                   false)
                      
                      ;; Form not found
                      (false? result)
                      (clj-result-k [(str "Could not find form '" form-name "' of type '" form-type "' in file " file-path)]
                                   true)
                      
                      ;; Other error (string result means error)
                      :else
                      (clj-result-k [(str "Error updating form: " result)]
                                   true))))))})

(comment
  ;; Example usage
  
(def sample "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

(def test-var 42)

(ns test.namespace
  (:require [clojure.string :as str]))")

;; Test finding a function
(find-top-level-form (z/of-string sample) "defn" "foo")
  
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
