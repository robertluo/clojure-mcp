(ns clojure-mcp.repl-tools.top-level-form-edit-pipeline
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.spec.alpha :as s]
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


;; Base spec for our context map
(s/def ::file-path string?)
(s/def ::source string?)
(s/def ::new-source-code string?)
(s/def ::top-level-def-name string?)
(s/def ::top-level-def-type string?)
(s/def ::edit-type #{:replace :before :after})
(s/def ::error keyword?)
(s/def ::message string?)
(s/def ::zloc any?) ;; zloc is opaque, so we'll use any?
(s/def ::offsets (s/tuple int? int?))
(s/def ::lint-result (s/nilable map?))

;; Context map that flows through the pipeline
(s/def ::context
  (s/keys :req [::file-path]
          :opt [::source ::new-source-code ::top-level-def-name 
                ::top-level-def-type ::edit-type ::error ::message 
                ::zloc ::offsets ::lint-result]))

;; Function specs
(s/fdef load-source
  :args (s/cat :ctx (s/keys :req [::file-path]))
  :ret (s/or :success (s/keys :req [::file-path ::source])
             :failure (s/keys :req [::error ::message])))

(s/fdef lint-code
  :args (s/cat :ctx (s/keys :req [::new-source-code]))
  :ret (s/or :success (s/keys :req [::new-source-code ::lint-result])
             :failure (s/keys :req [::error ::message])))

(s/fdef find-form
  :args (s/cat :ctx (s/keys :req [::source ::top-level-def-name ::top-level-def-type]))
  :ret (s/or :success (s/keys :req [::zloc])
             :failure (s/keys :req [::error ::message])))

(s/fdef edit-form
  :args (s/cat :ctx (s/keys :req [::zloc ::new-source-code ::edit-type]))
  :ret (s/or :success (s/keys :req [::zloc])
             :failure (s/keys :req [::error ::message])))

(s/fdef save-file
  :args (s/cat :ctx (s/keys :req [::file-path ::zloc]))
  :ret (s/or :success (s/keys :req [::offsets])
             :failure (s/keys :req [::error ::message])))

(s/fdef highlight-form
  :args (s/cat :ctx (s/keys :req [::file-path ::offsets]))
  :ret (s/or :success (s/keys :req [::file-path ::offsets])
             :failure (s/keys :req [::error ::message])))

;; Pipeline Implementation

(defn load-source
  "Loads the content of a file into the context map.
   Adds ::source to the context."
  [ctx]
  (try
    (assoc ctx ::source (slurp (::file-path ctx)))
    (catch java.io.FileNotFoundException _
      {::error :file-not-found
       ::message (str "File not found: " (::file-path ctx))})
    (catch java.io.IOException e
      {::error :io-error
       ::message (str "IO error while reading file: " (.getMessage e))})))

(defn lint-code
  "Lints the new source code to be inserted.
   Adds ::lint-result to the context."
  [ctx]
  (let [lint-result (linting/lint (::new-source-code ctx))]
    (if (and lint-result (:error? lint-result))
      {::error :lint-failure
       ::message (str "Linting issues in code:\n" (:report lint-result))}
      (assoc ctx ::lint-result lint-result))))

(defn find-form
  "Finds a top-level form in the source code.
   Adds ::zloc to the context with the zipper location."
  [ctx]
  (let [zloc (z/of-string (::source ctx) {:track-position? true})
        form-zloc
        (find-top-level-form zloc 
                                 (::top-level-def-type ctx) 
                                 (::top-level-def-name ctx))]
    (if form-zloc
      (assoc ctx ::zloc form-zloc)
      {::error :form-not-found
       ::message (str "Could not find form '" (::top-level-def-name ctx) 
                      "' of type '" (::top-level-def-type ctx) 
                      "' in file " (::file-path ctx))})))

(defn edit-form
  "Edits the form according to the specified edit type.
   Updates ::zloc in the context with the updated zipper."
  [ctx]
  (let [edit-type (::edit-type ctx)
        form-zloc (::zloc ctx)
        content-str (::new-source-code ctx)
        updated-zloc (edit-top-level-form 
                       form-zloc
                       (::top-level-def-type ctx)
                       (::top-level-def-name ctx)
                       content-str
                       edit-type)]
    (if updated-zloc
      (assoc ctx ::zloc updated-zloc)
      {::error :edit-failed
       ::message (str "Failed to " (name edit-type) " form.")})))

(defn save-file
  "Saves the updated source to the file and calculates offsets.
   Adds ::offsets to the context."
  [ctx]
  (try
    (let [updated-zloc (::zloc ctx)
          positions (z/position-span updated-zloc)
          final-source (z/root-string updated-zloc)
          offsets (zloc-offsets final-source positions)]
      (spit (::file-path ctx) final-source)
      (assoc ctx ::offsets offsets))
    (catch Exception e
      {::error :save-failed
       ::message (str "Failed to save file: " (.getMessage e))})))

(defn highlight-form
  "Highlights the edited form in Emacs.
   Returns the context unchanged or with error information."
  [ctx]
  (try
    (let [[start end] (::offsets ctx)]
      (emacs/temporary-highlight 
       (::file-path ctx) start end 2.0)
      ctx)
    (catch Exception e
      (assoc ctx ::highlight-error (.getMessage e)))))

;; Helper for threading functions with error handling
(defn thread-ctx
  "Thread a context map through a series of functions, short-circuiting on error.
   
   Arguments:
   - ctx: The initial context map
   - fns: A sequence of functions to apply to the context
   
   Returns:
   - The final context, possibly containing an error"
  [ctx & fns]
  (reduce (fn [c f]
            (if (::error c)
              (reduced c)  ; Short-circuit on error using reduced
              (f c)))
          ctx
          fns))

;; Full pipeline function
(defn edit-top-level-form-pipeline
  "Complete pipeline for editing a top-level form.
   
   Arguments:
   - name: Name of the form
   - file-path: Path to the file
   - tag: Type of form (e.g., 'defn', 'def')
   - content-str: New content to insert or replace with
   - edit-type: Keyword indicating edit type (:replace, :before, or :after)
   
   Returns:
   - A context map with the result or error information"
  [name file-path tag content-str edit-type]
  (thread-ctx 
   {::file-path file-path
    ::top-level-def-name name
    ::top-level-def-type tag
    ::new-source-code content-str
    ::edit-type edit-type}
   load-source
   lint-code
   find-form
   edit-form
   save-file
   highlight-form))

;; Format result for tool consumption
(defn format-result
  "Format the result of the pipeline for tool consumption.
   
   Arguments:
   - ctx: The final context map from the pipeline
   
   Returns:
   - A map with :error, :message, and possibly :offsets keys"
  [ctx]
  (if (::error ctx)
    {:error true
     :message (::message ctx)}
    {:error false
     :offsets (::offsets ctx)}))

;; Combining the pipeline with result formatting
(defn edit-form-in-file
  "Edit a top-level form in a file using the pipeline pattern.
   
   Arguments:
   - name: Name of the form
   - file-path: Path to the file
   - tag: Type of form (e.g., 'defn', 'def')
   - content-str: New content to insert or replace with
   - edit-type: Keyword indicating edit type (:replace, :before, or :after)
   
   Returns:
   - [start end] offsets if the edit was successful
   - false if the form was not found
   - A map with :error and :message keys if there were errors"
  [name file-path tag content-str edit-type]
  (let [result (edit-top-level-form-pipeline name file-path tag content-str edit-type)
        formatted (format-result result)]
    (cond
      (:error formatted) {:error (:error result)
                          :message (::message result)}
      (:offsets formatted) (::offsets result)
      :else false)))


(defn top-level-form-edit-tool 
  "Returns a tool map for replacing top-level forms in Clojure files.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_edit_replace_form"
   :description
   (str "Replaces a top-level Clojure form with new implementation. Use this to modify existing functions, vars, "
        "or other forms while preserving surrounding code formatting. Example: Update a function's implementation "
        "or signature without rewriting the entire file.\n\n"
        "Tip: To effectively remove a form, replace it with a comment like \";; Removed form-name\" - "
        "this maintains file structure while making the intention clear.\n\n"
        "Tip: Use clojure_file_outline before and after editing to confirm the structure of the file "
        "and verify your changes appear in the expected location.\n\n"
        "# Example:\n"
        "# clojure_edit_replace_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   form_name: \"process-data\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_implementation: \"(defn process-data [x] (+ x 10))\"\n"
        "# )\n\n"
        "# Example (removing a form):\n"
        "# clojure_edit_replace_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   form_name: \"obsolete-function\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_implementation: \";; Removed obsolete-function\"\n"
        "# )")
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
                    content (get arg-map "new_implementation")
                    result (edit-top-level-form-pipeline 
                            form-name file-path form-type content :replace)
                    formatted (format-result result)]
                (if (:error formatted)
                  (clj-result-k [(::message result)] true)
                  (let [[start end] (::offsets result)]
                    (try
                      (emacs/temporary-highlight file-path start end 2.0)
                      (catch Exception _
                        ;; Ignore highlight errors
                        nil))
                    (clj-result-k 
                     [(format "Successfully updated form '%s' in file %s" form-name file-path)]
                     false)))))})

(defn top-level-form-insert-before-tool 
  "Returns a tool map for inserting before top-level forms in Clojure files.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_edit_insert_before_form"
   :description
   (str "Inserts new Clojure code immediately before a specified top-level form. Perfect for adding helper functions, "
        "imports, or configuration before existing code. Maintains proper formatting and whitespace in the file.\n\n"
        "Tip: Use clojure_file_outline before and after inserting to confirm the order of forms "
        "and ensure your new code appears in the expected location.\n\n"
        "# Example:\n"
        "# clojure_edit_insert_before_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   before_form_name: \"main-function\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_form_str: \"(defn helper-function [x] (str x))\"\n"
        "# )")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:before_form_name {:type :string
                  :description "The name of the form before which to insert (e.g., function name, var name, namespace name)"}
      :file_path {:type :string
                  :description "Path to the file containing the form"}
      :form_type {:type :string
                  :description "The type of form (e.g., 'defn', 'def', 'ns', 'deftest' ...). Required."}
      :new_form_str {:type :string
                 :description "String with the new form to insert"}}
     :required [:before_form_name :file_path :form_type :new_form_str]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [form-name (get arg-map "before_form_name")
                    file-path (get arg-map "file_path")
                    form-type (get arg-map "form_type")
                    content (get arg-map "new_form_str")
                    result (edit-top-level-form-pipeline 
                            form-name file-path form-type content :before)
                    formatted (format-result result)]
                (if (:error formatted)
                  (clj-result-k [(::message result)] true)
                  (let [[start end] (::offsets result)]
                    (try
                      (emacs/temporary-highlight file-path start end 2.0)
                      (catch Exception _
                        ;; Ignore highlight errors
                        nil))
                    (clj-result-k 
                     [(format "Successfully inserted form before '%s' in file %s" form-name file-path)]
                     false)))))})

(defn top-level-form-insert-after-tool 
  "Returns a tool map for inserting after top-level forms in Clojure files.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_edit_insert_after_form"
   :description
   (str "Inserts new Clojure code immediately after a specified top-level form. Ideal for extending existing functionality "
        "with new functions or adding dependent code. Preserves all formatting in the file.\n\n"
        "Tip: Use clojure_file_outline before and after inserting to visualize the structure of the file "
        "and confirm your new code appears in the correct position.\n\n"
        "# Example:\n"
        "# clojure_edit_insert_after_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   after_form_name: \"config\",\n"
        "#   form_type: \"def\",\n"
        "#   new_form_str: \"(def extended-config (merge config {:new-key 'value}))\"\n"
        "# )")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:after_form_name {:type :string
                  :description "The name of the form after which to insert (e.g., function name, var name, namespace name)"}
      :file_path {:type :string
                  :description "Path to the file containing the form"}
      :form_type {:type :string
                  :description "The type of form (e.g., 'defn', 'def', 'ns', 'deftest' ...). Required."}
      :new_form_str {:type :string
                 :description "String with the new form to insert"}}
     :required [:after_form_name :file_path :form_type :new_form_str]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [form-name (get arg-map "after_form_name")
                    file-path (get arg-map "file_path")
                    form-type (get arg-map "form_type")
                    content (get arg-map "new_form_str")
                    result (edit-top-level-form-pipeline 
                            form-name file-path form-type content :after)
                    formatted (format-result result)]
                (if (:error formatted)
                  (clj-result-k [(::message result)] true)
                  (let [[start end] (::offsets result)]
                    (try
                      (emacs/temporary-highlight file-path start end 2.0)
                      (catch Exception _
                        ;; Ignore highlight errors
                        nil))
                    (clj-result-k 
                     [(format "Successfully inserted form after '%s' in file %s" form-name file-path)]
                     false)))))}) 

(defn get-form-summary
  "Get a summarized representation of a Clojure form showing only up to the argument list"
  [zloc]
  (try
    (let [sexpr (z/sexpr zloc)]
      (when (and (seq? sexpr) (symbol? (first sexpr)))
        (let [form-type (name (first sexpr))
              form-name (when (> (count sexpr) 1)
                          (let [second-item (second sexpr)]
                            (when (symbol? second-item)
                              (name second-item))))]
          
          (case form-type
            "defn" (let [vector-pos (count (take-while #(not (vector? %)) sexpr))
                         args (when (> (count sexpr) vector-pos)
                                (nth sexpr vector-pos))]
                     (str "(defn " form-name " " args " ...)"))
            
            "defmacro" (let [vector-pos (count (take-while #(not (vector? %)) sexpr))
                             args (when (> (count sexpr) vector-pos)
                                    (nth sexpr vector-pos))]
                         (str "(defmacro " form-name " " args " ...)"))
            
            "def" (str "(def " form-name " ...)")
            "deftest" (str "(deftest " form-name " ...)")
            "ns" (z/string zloc) ; Always show the full namespace
            (str "(" form-type " " (or form-name "") " ...)")))))
    (catch Exception e
      (println "Error in get-form-summary:" (.getMessage e))
      nil)))

(defn generate-collapsed-file-view
  "Generates a collapsed view of all top-level forms in a Clojure file.
   
   Arguments:
   - file-path: Path to the Clojure file
   - expand-symbols: Optional sequence of symbol names to show in expanded form
   
   Returns:
   - A string containing the collapsed representation of the file"
  [file-path expand-symbols]
  (try
    (let [file-content (slurp file-path)
          zloc (z/of-string file-content)
          expand-set (set (map name expand-symbols))]
      
      (loop [loc zloc
             forms []]
        (if (nil? loc)
          ;; Return the final string with all forms
          (str/join "\n\n" forms)
          
          ;; Process current form
          (let [current-sexpr (try (z/sexpr loc) (catch Exception _ nil))
                next-loc (try (z/right loc) (catch Exception _ nil))
                form-name (when (and (seq? current-sexpr) 
                                    (> (count current-sexpr) 1)
                                    (symbol? (second current-sexpr)))
                           (name (second current-sexpr)))
                should-expand (contains? expand-set form-name)
                form-str (if should-expand
                          (z/string loc)
                          (get-form-summary loc))]
            (if form-str
              (recur next-loc (conj forms form-str))
              (recur next-loc forms))))))
    (catch java.io.FileNotFoundException _
      (str "Error: File not found: " file-path))
    (catch Exception e
      (str "Error generating file view: " (.getMessage e))))) 

(defn clojure-file-outline-tool
  "Returns a tool map for generating a collapsed file outline.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_file_outline"
   :description
   (str "Generates a collapsed outline view of a Clojure file with only top-level function/var names and argument lists. "
        "This tool helps you understand the structure of a file without being overwhelmed by implementation details.\n\n"
        "Use this tool when:\n"
        "- You want to get a quick overview of a large or unfamiliar file\n"
        "- You need to locate specific functions or variables within a file\n"
        "- You want to selectively examine implementations of specific forms while keeping others collapsed\n\n"
        "The outline preserves function signatures, making it easy to understand the API without diving into implementations.\n\n"
        "# Example:\n"
        "# clojure_file_outline(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   expand: [\"main-function\", \"process-data\"] # Optional list of forms to show expanded\n"
        "# )")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:file_path {:type :string
                  :description "Path to the Clojure file to outline"}
      :expand {:type :array
               :description "Optional list of form names to show in expanded form"
               :items {:type :string}}}
     :required [:file_path]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [file-path (get arg-map "file_path")
                    expand-forms (or (get arg-map "expand") [])
                    result (generate-collapsed-file-view file-path expand-forms)]
                (if (str/starts-with? result "Error")
                  (clj-result-k [result] true)
                  (clj-result-k [result] false))))})

(comment
  ;; Examples of using the pipeline
  (def result (edit-top-level-form-pipeline
               "example-function"
               "/path/to/file.clj"
               "defn"
               "(defn example-function [x y]\n  (+ x y))"
               :replace))
  
  ;; Check for errors
  (::error result)
  (::message result)
  
  ;; Get formatted result
  (format-result result)
  
  ;; Example of tool creation with pipeline
  (def edit-tool (top-level-form-edit-tool (atom {})))
  (def before-tool (top-level-form-insert-before-tool (atom {})))
  (def after-tool (top-level-form-insert-after-tool (atom {})))
)
