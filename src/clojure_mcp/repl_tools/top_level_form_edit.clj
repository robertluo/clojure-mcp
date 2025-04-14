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

(defn edit-top-level-form
  "Edit a top-level form by replacing it or inserting content before or after.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def, 'ns)
   - name: The name of the form
   - content-str: The string to insert or replace with
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   
   Returns the updated zipper, or nil if the form was not found."
  [zloc tag name content-str edit-type]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (case edit-type
      :replace (z/replace form-zloc (p/parse-string content-str))
      :before  (-> form-zloc
                   (z/insert-left (p/parse-string-all (str content-str "\n\n")))
                   z/up)
      :after   (-> form-zloc
                   (z/insert-right (p/parse-string-all (str "\n\n" content-str)))
                   z/up))))

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

(defn edit-form-in-file
  "Edits a top-level form in a file by replacing it or inserting content before or after.
   
   Arguments:
   - name: Name of the form (string or symbol)
   - file-path: Path to the file
   - tag: The type of the form (defn, def, ns, etc.)
   - content-str: String with the content to insert or replace with
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   
   Returns:
   - [start end] offsets if the edit was successful
   - false if the form was not found
   - A map with :error and :message keys if there were errors"
  [name file-path tag content-str edit-type]
  (try
    (let [name (if (string? name) name (str name))
          tag (if (string? tag) tag (str tag))
          ;; Lint the code first to ensure it's valid Clojure code
          lint-result (linting/lint content-str)]
      ;; Fail early if there are linting issues
      (if lint-result
        {:error :lint-failure 
         :message (str "Linting issues in " (name edit-type) " content:\n" (:report lint-result))}
        
        ;; Continue with edit if no linting issues
        (try
          (let [file-content (slurp file-path)
                zloc (z/of-string file-content {:track-position? true})
                updated-zloc (edit-top-level-form zloc tag name content-str edit-type)]
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

;; Configuration for different edit types
(def edit-type-config
  {:replace 
   {:tool-name "top_level_form_edit"
    :short-description "Edits any top-level form in a Clojure file, replacing it with a new implementation."
    :intro "This tool allows you to modify any top-level form (def, defn, ns, deftest etc.) in source files without manually editing the files."
    :param-name "form_name"
    :param-description "The name of the form to edit (e.g., function name, var name, namespace name)"
    :content-param-name "new_implementation"
    :content-description "String with the new form implementation"
    :action-prefix ""
    :success-message "Successfully updated form '%s' in file %s"}
   
   :before
   {:tool-name "insert_before_top_level_form"
    :short-description "Inserts new content before a top-level form in a Clojure file."
    :intro "This tool allows you to insert new code (such as a new function or definition) before an existing top-level form (def, defn, ns, deftest etc.) in source files."
    :param-name "before_form_name"
    :param-description "The name of the form before which to insert (e.g., function name, var name, namespace name)"
    :content-param-name "new_form_str"
    :content-description "String with the new form to insert"
    :action-prefix "before "
    :success-message "Successfully inserted form before '%s' in file %s"}
   
   :after
   {:tool-name "insert_after_top_level_form"
    :short-description "Inserts new content after a top-level form in a Clojure file."
    :intro "This tool allows you to insert new code (such as a new function or definition) after an existing top-level form (def, defn, ns, deftest etc.) in source files."
    :param-name "after_form_name"
    :param-description "The name of the form after which to insert (e.g., function name, var name, namespace name)"
    :content-param-name "new_form_str"
    :content-description "String with the new form to insert"
    :action-prefix "after "
    :success-message "Successfully inserted form after '%s' in file %s"}})



(defn create-form-edit-tool 
  "Creates a tool function for the specified edit type.
   
   Arguments:
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   - service-atom: Service atom
   
   Returns a tool map with name, description, schema and tool-fn."
  [edit-type service-atom]
  (let [config (get edit-type-config edit-type)
        {:keys [tool-name short-description intro param-name param-description
                content-param-name content-description action-prefix success-message]} config
        long-description (str short-description "\n   \n" intro
                             " It preserves formatting and whitespace in the rest of the file.")]
    
    {:name tool-name
     :description long-description
     :schema
     (json/write-str
      {:type :object
       :properties
       {param-name {:type :string
                    :description param-description}
        :file_path {:type :string
                    :description "Path to the file containing the form"}
        :form_type {:type :string
                    :description "The type of form (e.g., 'defn', 'def', 'ns', 'deftest' ...). Required."}
        content-param-name {:type :string
                            :description content-description}}
       :required [param-name :file_path :form_type content-param-name]})
     :tool-fn (fn [_ arg-map clj-result-k]
                (let [form-name (get arg-map param-name)
                      file-path (get arg-map "file_path")
                      form-type (get arg-map "form_type")
                      content (get arg-map content-param-name)]
                  (if-let [lint-result (linting/lint content)]
                    (let [formatted-report
                          (str "Cannot " (name edit-type) " form " action-prefix
                               "'" form-name "' of type '" form-type "':\n\n" 
                               (linting/format-lint-warnings lint-result) 
                               "\n\nPlease fix these issues before proceeding.")]
                      (clj-result-k [formatted-report] true))
                    (let [result (edit-form-in-file form-name file-path form-type content edit-type)]
                      (cond
                        (vector? result)
                        (do
                          (try
                            (emacs/temporary-highlight file-path (first result) (second result) 2.0)
                            (catch Exception e
                              (println "Warning: Failed to highlight form in Emacs:" (.getMessage e))))
                          
                          (clj-result-k [(format success-message form-name file-path)] 
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
                              formatted-error (str "Error (" error-type ") " (name edit-type) "ing form " 
                                                  action-prefix "'" form-name "': " error-msg)]
                          (clj-result-k [formatted-error] true))
                        
                        ;; Other error
                        :else
                        (clj-result-k [(str "Error " (name edit-type) "ing form: " result)]
                                      true))))))}))

;; Define the individual tool functions using the factory function
(defn top-level-form-edit-tool [service-atom]
  (create-form-edit-tool :replace service-atom))

(defn top-level-form-insert-before-tool [service-atom]
  (create-form-edit-tool :before service-atom))

(defn top-level-form-insert-after-tool [service-atom]
  (create-form-edit-tool :after service-atom))

(comment
  ;; Example usage
  (def sample "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

(def test-var 42)

(ns test.namespace
  (:require [clojure.string :as str]))")

  ;; Test finding a function
  (z/position-span (find-top-level-form (z/of-string sample {:track-position? true}) "def" "test-var"))

  ;; Test the different edit types
  (z/root-string 
   (edit-top-level-form 
    (z/of-string sample) 
    "defn"
    "foo"
    "(defn foo [] 'updated-value)"
    :replace))
  
  (z/root-string 
   (edit-top-level-form 
    (z/of-string sample) 
    "def"
    "test-var"
    "(defn helper [x] (* x x))"
    :before))
  
  (z/root-string 
   (edit-top-level-form 
    (z/of-string sample) 
    "def"
    "test-var"
    "(def another-var :new-value)"
    :after))
  
  ;; === Examples of using the form edit tools ===
  
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
  
  ;; Testing the different tools
  (def replace-tester (make-test-tool (top-level-form-edit-tool client-atom)))
  (def before-tester (make-test-tool (top-level-form-insert-before-tool client-atom)))
  (def after-tester (make-test-tool (top-level-form-insert-after-tool client-atom)))
  
  ;; Edit a function
  (replace-tester {"form_name" "example-function"
                   "file_path" "/path/to/file.clj"
                   "form_type" "defn"
                   "new_implementation" "(defn example-function [x y]\n  (+ x y))"})
  
  ;; Insert before a namespace
  (before-tester {"before_form_name" "my.project.core"
                  "file_path" "/path/to/core.clj"
                  "form_type" "ns"
                  "new_form_str" ";; Core namespace for the project\n;; Author: Developer Name"})
  
  ;; Insert after a variable
  (after-tester {"after_form_name" "config"
                 "file_path" "/path/to/config.clj"
                 "form_type" "def"
                 "new_form_str" "(def expanded-config\n  (assoc config :timeout 30000))"})
  
  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom)
)
