(ns clojure-mcp.repl-tools.top-level-form-edit-pipeline
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.spec.alpha :as s]
   [clojure-mcp.linting :as linting]
   [clojure-mcp.utils.emacs-integration :as emacs]
   [clojure-mcp.repl-tools.top-level-form-edit :as tfe]))

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
        form-zloc (tfe/find-top-level-form zloc 
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
        updated-zloc (tfe/edit-top-level-form 
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
          offsets (tfe/zloc-offsets final-source positions)]
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
        "# Example:\n"
        "# clojure_edit_replace_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   form_name: \"process-data\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_implementation: \"(defn process-data [x] (+ x 10))\"\n"
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
