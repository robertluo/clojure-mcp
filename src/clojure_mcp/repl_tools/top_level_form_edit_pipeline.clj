(ns clojure-mcp.repl-tools.top-level-form-edit-pipeline
  (:require
   [rewrite-clj.zip :as z]
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [cljfmt.core :as fmt]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clojure-mcp.linting :as linting]
   [clojure-mcp.repl-tools.utils :as utils]
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
   - content-str: The string to insert or replace with (can contain multiple forms)
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   
   Returns the updated zipper, or nil if the form was not found."
  [zloc tag name content-str edit-type]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (case edit-type
      :replace (z/replace form-zloc (p/parse-string-all content-str))
      ;; it would be nice if this handled comments immediately preceeding the form
      :before (-> form-zloc
                  (z/insert-left (p/parse-string-all "\n\n"))
                  z/left
                  (z/insert-left (p/parse-string-all content-str))
                  z/left)
      :after (-> form-zloc
                 (z/insert-right (p/parse-string-all "\n\n"))
                 z/right
                 (z/insert-right (p/parse-string-all content-str))
                 z/right))))

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
(s/def ::output-source string?)
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

(s/fdef emacs-set-auto-revert
  :args (s/cat :ctx (s/keys :req [::file-path]))
  :ret (s/or :success (s/keys :req [::file-path])
             :failure (s/keys :req [::error ::message])))

(s/fdef save-file
  :args (s/cat :ctx (s/keys :req [::file-path ::zloc ::output-source]))
  :ret (s/or :success (s/keys :req [::offsets])
             :failure (s/keys :req [::error ::message])))

(s/fdef highlight-form
  :args (s/cat :ctx (s/keys :req [::file-path ::offsets]))
  :ret (s/or :success (s/keys :req [::file-path ::offsets])
             :failure (s/keys :req [::error ::message])))

(s/fdef format-source
  :args (s/cat :ctx (s/keys :req [::zloc]))
  :ret (s/or :success (s/keys :req [::zloc ::output-source])
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

;; Removed aggressive-whitespace-cleanup function as we're only using cljfmt

(defn format-source
  "Formats the entire source string using cljfmt with comprehensive formatting options.
   
   Arguments:
   - ctx: The context map with the zipper location in ::zloc
   
   Returns:
   - Updated context with a reformatted ::zloc, or error map if failed"
  [ctx]
  (try
    (let [zloc (::zloc ctx)
          root-str (z/root-string zloc)
          ;; Use all available cljfmt formatting options
          formatting-options {:indentation? true
                              :remove-surrounding-whitespace? true
                              :remove-trailing-whitespace? true
                              :insert-missing-whitespace? true
                              :remove-consecutive-blank-lines? true
                              :remove-multiple-non-indenting-spaces? true
                              :split-keypairs-over-multiple-lines? false
                              :sort-ns-references? false
                              :function-arguments-indentation :community
                              :indents fmt/default-indents}
          formatted-out (fmt/reformat-string root-str formatting-options)]
      (assoc ctx ::output-source formatted-out))
    (catch Exception e
      {::error :format-failed
       ::message (str "Failed to format source: " (.getMessage e))})))

(defn emacs-set-auto-revert
  "Ensures that the file is open in Emacs with auto-revert-mode enabled.
   Uses the emacs-integration/ensure-auto-revert utility.
   
   This is a critical step in the pipeline to ensure that changes made to the file
   are immediately visible in Emacs without requiring manual buffer refreshes.
   It should be called before saving the file to ensure auto-revert is ready.
   
   Adds no new values to the context."
  [ctx]
  (try
    (let [file-path (::file-path ctx)
          result (emacs/ensure-auto-revert file-path)]
      (if (map? result)
        ;; Error was returned
        {::error :auto-revert-failed
         ::message (str "Failed to set auto-revert mode: " (:message result))}
        ;; Success - return context unchanged
        ctx))
    (catch Exception e
      {::error :auto-revert-exception
       ::message (str "Exception setting auto-revert mode: " (.getMessage e))})))

(defn save-file
  "Saves the updated source to the file and calculates offsets.
   Adds ::offsets to the context."
  [{:keys [::output-source ::zloc ::file-path] :as ctx}]
  (try
    (let [updated-zloc zloc
          positions (z/position-span updated-zloc)
          offsets (zloc-offsets output-source positions)]
      (spit file-path output-source)
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
              (reduced c) ; Short-circuit on error using reduced
              (f c)))
          ctx
          fns))

;; Full pipeline function
(defn find-docstring
  "Finds the docstring node of a top-level form.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def)
   - name: The name of the form
   
   Returns the zipper positioned at the docstring node, or nil if:
   - The form was not found
   - The form doesn't have a docstring"
  [zloc tag name]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (let [tag-zloc (z/down form-zloc) ;; Move to the tag (defn, def, etc.)
          name-zloc (z/right tag-zloc) ;; Move to the name
          docstring-candidate (z/right name-zloc)] ;; Move to potential docstring
      (when (and docstring-candidate
                 ;; Check for both :token (single-line) and :multi-line (multi-line) tags
                 (contains? #{:token :multi-line} (z/tag docstring-candidate))
                 (string? (z/sexpr docstring-candidate)))
        docstring-candidate))))

(defn edit-docstring
  "Edit a docstring in a top-level form.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def)
   - name: The name of the form
   - new-docstring: The new docstring content
   
   Returns the updated zipper, or nil if the form or docstring was not found."
  [zloc tag name new-docstring]
  (when-let [docstring-zloc (find-docstring zloc tag name)]
    ;; Replace the docstring with the new one
    (z/replace docstring-zloc (n/string-node new-docstring))))

(defn docstring-edit-pipeline
  "Pipeline for editing docstrings in a file.
   
   Arguments:
   - name: Name of the form
   - file-path: Path to the file
   - tag: Type of form (e.g., 'defn', 'def')
   - new-docstring: New docstring content
   
   Returns a context map with the result of the operation."
  [name file-path tag new-docstring]
  (-> {::file-path file-path
       ::top-level-def-name name
       ::top-level-def-type tag
       ::new-content new-docstring
       ::edit-type :docstring}
      (thread-ctx
       load-source
       (fn [ctx]
         (if-let [source (::source ctx)]
           (if-let [zloc (z/of-string source {:track-position? true})]
             (if-let [updated-zloc (edit-docstring zloc tag name new-docstring)]
               (assoc ctx ::zloc updated-zloc)
               (assoc ctx ::error :docstring-not-found
                      ::message (format "No docstring found in form '%s' of type '%s' in file %s"
                                        name tag file-path)))
             (assoc ctx ::error :parse-error
                    ::message (format "Failed to parse source code in file %s" file-path)))
           ctx))
       format-source
       emacs-set-auto-revert
       save-file
       highlight-form)))

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
   format-source
   emacs-set-auto-revert ; New step to ensure auto-revert mode is enabled
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
        "or other forms while preserving surrounding code formatting. Supports replacing with multiple forms "
        "in a single operation. Example: Update a function's implementation or signature, "
        "or replace a single function with multiple helper functions.\n\n"
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
        "# Example (replacing with multiple forms):\n"
        "# clojure_edit_replace_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   form_name: \"process-data\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_implementation: \"(defn process-data [x] (+ x 10))\\n\\n(defn helper-fn [y] (* y 2))\"\n"
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
                           :description "String with the new form implementation (can contain multiple forms)"}}
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
        "imports, or configuration before existing code. Supports inserting multiple forms in a single operation. "
        "Maintains proper formatting and whitespace in the file.\n\n"
        "Tip: Use clojure_file_outline before and after inserting to confirm the order of forms "
        "and ensure your new code appears in the expected location.\n\n"
        "# Example:\n"
        "# clojure_edit_insert_before_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   before_form_name: \"main-function\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_form_str: \"(defn helper-function [x] (str x))\"\n"
        "# )\n\n"
        "# Example (inserting multiple forms):\n"
        "# clojure_edit_insert_before_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   before_form_name: \"main-function\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_form_str: \"(defn helper1 [x] (str x))\\n\\n(defn helper2 [y] (str y))\"\n"
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
                     :description "String with the new form to insert (can contain multiple forms)"}}
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
        "with new functions or adding dependent code. Supports inserting multiple forms in a single operation. "
        "Preserves all formatting in the file.\n\n"
        "Tip: Use clojure_file_outline before and after inserting to visualize the structure of the file "
        "and confirm your new code appears in the correct position.\n\n"
        "# Example:\n"
        "# clojure_edit_insert_after_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   after_form_name: \"config\",\n"
        "#   form_type: \"def\",\n"
        "#   new_form_str: \"(def extended-config (merge config {:new-key 'value}))\"\n"
        "# )\n\n"
        "# Example (inserting multiple forms):\n"
        "# clojure_edit_insert_after_form(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   after_form_name: \"config\",\n"
        "#   form_type: \"def\",\n"
        "#   new_form_str: \"(def config1 {:a 1})\\n\\n(def config2 {:b 2})\"\n"
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
                     :description "String with the new form to insert (can contain multiple forms)"}}
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
      (str "Error in get-form-summary: " (pr-str (z/sexpr zloc))
           " - " (.getMessage e))
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
  {:name "clojure_file_structure"
   :description
   (str
    "Creates a concise structural view of a Clojure file by showing only top-level forms with their signatures, collapsing implementation details. This provides a clear API overview that helps you understand how the file is organized.

The 'expand' parameter lets you selectively view full implementations of specific forms while keeping others collapsed, making it ideal for focused code review.

Use this tool when:
- You need a quick overview of a large or unfamiliar file's structure
- You want to locate specific functions or vars within a file
- You're planning refactoring and need to understand module organization
- You want to document a namespace's public API

Example output:
(ns example.core ...)
(defn process-data [input options] ...)
(def default-config ...)
(defn format-result [data] ...)

Tip: Use this tool before and after using other code editing tools to verify changes appear in the expected location.

# Example:
# clojure_file_structure(
#   file_path: \"src/example/core.clj\",
#   expand: [\"process-data\"]  # Optional: show full implementation of specific forms
# )")
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

(defn is-comment-form?
  "Check if a zloc is a (comment ...) form."
  [zloc]
  (try
    (and (z/seq? zloc)
         (let [first-child (z/down zloc)]
           (and first-child
                (= (z/sexpr first-child) 'comment))))
    (catch Exception _ false)))

(defn is-line-comment?
  "Check if a zloc is a line comment."
  [zloc]
  (try
    (= (-> zloc z/node n/tag) :comment)
    (catch Exception _ false)))

(defn find-comment-block
  "Find a comment block (either a 'comment' form or consecutive comment lines)
   that contains a specific substring.
   
   Arguments:
   - source: The source string to search in
   - comment-substring: The substring to look for
   
   Returns a map with :type, :start, :end, and :content keys,
   or nil if no matching comment block is found."
  [source comment-substring]
  (let [zloc (z/of-string source {:track-position? true})
        lines (str/split-lines source)]

    ;; First, try to find a comment form
    (loop [loc zloc]
      (cond
        ;; No more forms
        (nil? loc)
        (let [;; Find consecutive comment lines
              consecutive-comments
              (->> (map-indexed vector lines)
                   (reduce
                    (fn [[blocks current-block] [idx line]]
                      (cond
                        ;; If we're already tracking a block and the line is a comment
                        (and current-block
                             (str/starts-with? (str/trim line) ";;"))
                        [blocks (update current-block :lines conj line)]

                        ;; If we're tracking a block and hit a non-comment line
                        current-block
                        [(conj blocks (assoc current-block :end (dec idx))) nil]

                        ;; If this is a new comment line
                        (str/starts-with? (str/trim line) ";;")
                        [blocks {:start idx
                                 :lines [line]}]

                        ;; Otherwise, continue
                        :else
                        [blocks nil]))
                    [[] nil])
                   first)]

          ;; Find the first consecutive comment block containing the substring
          (when-let [matching-block
                     (first (filter #(some (fn [line]
                                             (str/includes? line comment-substring))
                                           (:lines %))
                                    consecutive-comments))]
            {:type :line-comments
             :start (:start matching-block)
             :end (or (:end matching-block) (+ (:start matching-block)
                                               (dec (count (:lines matching-block)))))
             :content (str/join "\n" (:lines matching-block))}))

        ;; Check if current form is a comment form
        (is-comment-form? loc)
        (let [comment-str (z/string loc)]
          (if (str/includes? comment-str comment-substring)
            (let [pos (z/position-span loc)]
              {:type :comment-form
               :start (first pos) ;; [row col]
               :end (second pos) ;; [row col]
               :content comment-str
               :zloc loc})
            (recur (z/right loc))))

        ;; Move to the next form
        :else (recur (z/right loc))))))

(defn edit-comment-block
  "Edit a comment block in the source code.
   
   Arguments:
   - source: The source string
   - comment-substring: Substring to identify the comment block
   - new-content: New content to replace the comment block with
   
   Returns the updated source code string, or the original if no matching block was found."
  [source comment-substring new-content]
  (let [block (find-comment-block source comment-substring)]
    (if (nil? block)
      source ;; No matching block found
      (let [lines (str/split-lines source)]
        (case (:type block)
          ;; For comment forms, use zloc to replace
          :comment-form
          (-> (:zloc block)
              (z/replace (p/parse-string new-content))
              z/root-string)

          ;; For line comments, replace the relevant lines
          :line-comments
          (let [start (:start block)
                end (:end block)
                new-lines (str/split-lines new-content)
                result (concat
                        (take start lines)
                        new-lines
                        (drop (inc end) lines))]
            (str/join "\n" result)))))))

(defn find-and-edit-comment
  "Find and edit a comment block in the source, working directly with zippers.
   
   Arguments:
   - ctx: Context map with ::source, ::comment-substring, and ::new-content
   
   Returns:
   - Updated context with ::zloc for the pipeline, or error map if comment not found"
  [ctx]
  (let [source (::source ctx)
        comment-substring (::comment-substring ctx)
        new-content (::new-content ctx)
        block (find-comment-block source comment-substring)]

    (if (nil? block)
      {::error :comment-not-found
       ::message (str "Could not find comment containing: " comment-substring)}

      (case (:type block)
        ;; For comment forms, use zipper replacement
        :comment-form
        (let [zloc (:zloc block)
              updated-zloc (z/replace zloc (p/parse-string new-content))]
          (assoc ctx ::zloc updated-zloc))

        ;; For line comments, create a new zipper with the edited content
        :line-comments
        (let [lines (str/split-lines source)
              start (:start block)
              end (:end block)
              new-lines (str/split-lines new-content)
              replaced-lines (concat
                              (take start lines)
                              new-lines
                              (drop (inc end) lines))
              updated-source (str/join "\n" replaced-lines)
              updated-zloc (z/of-string updated-source {:track-position? true})]
          (assoc ctx ::zloc updated-zloc))))))

(defn comment-block-edit-pipeline
  "Edit a comment block in a file, working directly with zippers.
   
   Arguments:
   - file-path: Path to the file
   - comment-substring: Substring to identify the comment block
   - new-content: New content to replace the comment block with
   
   Returns a context map with result information."
  [file-path comment-substring new-content]
  (thread-ctx
   {::file-path file-path
    ::comment-substring comment-substring
    ::new-content new-content}

   ;; Load the file content
   load-source

   ;; Find and edit the comment block, creating a zipper
   find-and-edit-comment

   ;; Format the source
   format-source

   ;; Ensure Emacs auto-revert is enabled
   emacs-set-auto-revert

   ;; Save the file
   save-file

   ;; Highlight the edited region
   highlight-form))

(defn comment-block-edit-tool
  "Returns a tool map for editing comment blocks in Clojure files.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_edit_comment_block"
   :description
   (str
    "Finds and replaces a TOP-LEVEL comment block in a Clojure file. Only works with:
 1. Top-level `(comment ...)` forms, or
 2. Top-level consecutive lines of comments starting with ';;'
 
 The comment block is identified by a substring that it contains. This tool will not modify comment blocks that are nested inside functions or other forms.
 
 IMPORTANT: This tool only modifies entire comment blocks at the top level of the file, not individual comments within other code structures."
    "\n\n"
    "Use this when you want to edit and change TOP-LEVEL comment blocks"
    "example code.\n\n"
    "# Example:\n"
    "# clojure_edit_comment_block(\n"
    "#   file_path: \"src/my_ns/core.clj\",\n"
    "#   comment_substring: \"Example usage:\",\n"
    "#   new_content: \";; Example usage:\\n;; (my-function 42)\"\n"
    "# )\n\n"
    "# Another example:\n"
    "# clojure_edit_comment_block(\n"
    "#   file_path: \"src/my_ns/core.clj\",\n"
    "#   comment_substring: \"Test with fixtures\",\n"
    "#   new_content: \"(comment\\n  (let [fixture {:id 1}]\\n    (test-fn fixture)))\"\n"
    "# )")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:file_path {:type :string
                  :description "Path to the file containing the comment block"}
      :comment_substring {:type :string
                          :description "A substring to identify the target comment block"}
      :new_content {:type :string
                    :description "New content to replace the comment block with"}}
     :required [:file_path :comment_substring :new_content]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [file-path (get arg-map "file_path")
                    comment-substring (get arg-map "comment_substring")
                    new-content (get arg-map "new_content")
                    result (comment-block-edit-pipeline
                            file-path comment-substring new-content)]
                (if (::error result)
                  (clj-result-k [(::message result)] true)
                  (clj-result-k
                   [(format "Successfully updated comment block in file %s" file-path)]
                   false))))})

(defn docstring-edit-tool
  "Returns a tool map for editing docstrings in Clojure files.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [_]
  {:name "clojure_edit_replace_docstring"
   :description
   (str "Replaces a docstring in a top-level Clojure form. This tool maintains all function "
        "parameters, implementation, and metadata while updating only the docstring.\n\n"
        "Use this tool when you want to update documentation without changing function behavior. "
        "Works with any top-level form that contains a docstring (defn, def, ns, etc.).\n\n"
        "# Example:\n"
        "# clojure_edit_replace_docstring(\n"
        "#   file_path: \"src/my_ns/core.clj\",\n"
        "#   form_name: \"process-data\",\n"
        "#   form_type: \"defn\",\n"
        "#   new_docstring: \"Takes a data map and processes it.\\nReturns the transformed result.\"\n"
        "# )\n"
        "# Note: To include newlines in docstrings, use escaped newlines (\\n)")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:form_name {:type :string
                  :description "The name of the form whose docstring to edit (e.g., function name, var name)"}
      :file_path {:type :string
                  :description "Path to the file containing the form"}
      :form_type {:type :string
                  :description "The type of form (e.g., 'defn', 'def', 'ns'). Required."}
      :new_docstring {:type :string
                      :description "String with the new docstring content"}}
     :required [:form_name :file_path :form_type :new_docstring]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [form-name (get arg-map "form_name")
                    file-path (get arg-map "file_path")
                    form-type (get arg-map "form_type")
                    new-docstring (get arg-map "new_docstring")
                    result (docstring-edit-pipeline
                            form-name file-path form-type new-docstring)
                    formatted (format-result result)]
                (if (:error formatted)
                  (clj-result-k [(:message formatted)] true)
                  (let [[start end] (::offsets result)]
                    (try
                      (emacs/temporary-highlight file-path start end 2.0)
                      (catch Exception _
                        ;; Ignore highlight errors
                        nil))
                    (clj-result-k
                     [(format "Successfully updated docstring for '%s' in file %s" form-name file-path)]
                     false)))))})

;; Removed generate-diff-string, now using utils/generate-diff-via-shell instead

(defn determine-file-type
  "Determine if the file operation is a create or update.
   
   Arguments:
   - ctx: Context map containing ::file-path
   
   Returns:
   - Updated context with ::type added"
  [ctx]
  (let [file-exists? (get ctx ::file-exists? (.exists (io/file (::file-path ctx))))]
    (assoc ctx ::type (if file-exists? "update" "create"))))

(s/fdef determine-file-type
  :args (s/cat :ctx (s/keys :req [::file-path]))
  :ret (s/keys :req [::file-path ::type]))

(defn generate-diff
  "Generate diff between old and new content as a pipeline function.
   Uses the shell-based diff generator from utils.clj.
   
   Arguments:
   - ctx: Context map containing ::old-content and ::output-source
   
   Returns:
   - Updated context with ::diff added"
  [ctx]
  (let [old-content (::old-content ctx)
        new-content (::output-source ctx)
        diff (if (= old-content new-content)
               "" ;; No diff if content is identical
               (try
                 ;; Use 3 lines of context
                 (utils/generate-diff-via-shell old-content new-content 3)
                 (catch Exception e
                   ;; If shell diff fails, return a fallback message
                   (str "Changes made, but diff generation failed: " (.getMessage e)))))]
    (assoc ctx ::diff diff)))

(s/fdef generate-diff
  :args (s/cat :ctx (s/keys :req [::old-content ::output-source]))
  :ret (s/keys :req [::old-content ::output-source ::diff]))


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
  (def after-tool (top-level-form-insert-after-tool (atom {}))))
