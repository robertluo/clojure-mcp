(ns clojure-mcp.tools.form-edit.tool
  "Implementation of form editing tools using the tool-system multimethod approach.
   Includes tools for:
   - Editing top-level forms (replace, insert before/after)
   - Editing docstrings
   - Editing comment blocks
   - Generating file outlines"
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.form-edit.core :as core]
   [clojure-mcp.tools.form-edit.pipeline :as pipeline]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.string :as str]
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]))

;; Factory functions to create the tool configurations

(defn create-edit-replace-form-tool
  "Creates the tool configuration for replacing forms in a file.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-replace-form
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defn create-edit-insert-before-form-tool
  "Creates the tool configuration for inserting content before forms.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-insert-before-form
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defn create-edit-insert-after-form-tool
  "Creates the tool configuration for inserting content after forms.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-insert-after-form
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defn create-edit-docstring-tool
  "Creates the tool configuration for editing docstrings.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-replace-docstring
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defn create-edit-comment-block-tool
  "Creates the tool configuration for editing comment blocks.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-comment-block
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defn create-file-structure-tool
  "Creates the tool configuration for generating file outlines.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-file-structure
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

;; Common validation functions

(defn validate-file-path
  "Validates that a file path is provided and within allowed directories"
  [inputs nrepl-client-atom]
  (let [{:keys [file_path]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not file_path
      (throw (ex-info "Missing required parameter: file_path"
                      {:inputs inputs})))
    ;; Use the utils/validate-path-with-client function to ensure path is valid
    (utils/validate-path-with-client file_path nrepl-client)))

;; Implement the required multimethods for the form replacement tool

(defmethod tool-system/tool-name :clojure-edit-replace-form [_]
  "clojure_edit_replace_definition")

(defmethod tool-system/tool-description :clojure-edit-replace-form [_]
  "Edits a top-level form in a Clojure file by fully replacing it.
   
   This tool can replace a specific top-level form (like a function, def, or namespace declaration) with new content. The form is identified by its type (defn, def, deftest, s/def, ns, defmethod etc.) and name.
   
   Example: Replace the implementation of a function named 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(defn example-fn [x] (* x 2))\"
   
   Note: For defmethod forms, you can specify either just the method name (\"area\") or 
   include the dispatch value (\"area :rectangle\"). When using just the method name, 
   the dispatch value will be automatically extracted from the replacement content.
   
   The tool will find the form, replace it with the new content, and format the result.
   It returns the updated file content, the location offsets, and a diff of the changes.")

(defmethod tool-system/tool-schema :clojure-edit-replace-form [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file containing the form to edit"}
                :form_name {:type :string
                            :description "Name of the form to edit (e.g., function name)"}
                :form_type {:type :string
                            :description "Type of the form (e.g., \"defn\", \"def\", \"ns\")"}
                :content {:type :string
                          :description "New content to replace the form with"}}
   :required [:file_path :form_name :form_type :content]})

(defmethod tool-system/validate-inputs :clojure-edit-replace-form [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [form_name form_type content]} inputs]
    (when-not form_name
      (throw (ex-info "Missing required parameter: form_name"
                      {:inputs inputs})))
    (when-not form_type
      (throw (ex-info "Missing required parameter: form_type"
                      {:inputs inputs})))
    (when-not content
      (throw (ex-info "Missing required parameter: content"
                      {:inputs inputs})))
    ;; Return validated inputs
    {:file_path file-path
     :form_name form_name
     :form_type form_type
     :content content}))

(defmethod tool-system/execute-tool :clojure-edit-replace-form [config inputs]
  (let [{:keys [file_path form_name form_type content]} inputs
        result (pipeline/edit-form-pipeline file_path form_name form_type content :replace config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-replace-form [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Implement the required multimethods for insert before form tool

(defmethod tool-system/tool-name :clojure-edit-insert-before-form [_]
  "clojure_edit_insert_before_definition")

(defmethod tool-system/tool-description :clojure-edit-insert-before-form [_]
  "Inserts content before a top-level form in a Clojure file.
   
   This tool adds new content before a specific top-level form (like a function or def)
   without modifying the form itself. The form is identified by its type and name.
   
   Example: Insert a new function before 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(defn helper-fn [x] (* x 2))\"
   
   Note: For defmethod forms, you can specify either just the method name (\"area\") or 
   include the dispatch value (\"area :rectangle\"). When using just the method name, 
   the dispatch value will be automatically extracted from the replacement content.
   
   The tool will find the form, insert the new content before it, and format the result.
   It returns the updated file content, the location offsets, and a diff of the changes.")

(defmethod tool-system/tool-schema :clojure-edit-insert-before-form [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file containing the form"}
                :form_name {:type :string
                            :description "Name of the form (e.g., function name)"}
                :form_type {:type :string
                            :description "Type of the form (e.g., \"defn\", \"def\", \"ns\")"}
                :content {:type :string
                          :description "Content to insert before the form"}}
   :required [:file_path :form_name :form_type :content]})

(defmethod tool-system/validate-inputs :clojure-edit-insert-before-form [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [form_name form_type content]} inputs]
    (when-not form_name
      (throw (ex-info "Missing required parameter: form_name"
                      {:inputs inputs})))
    (when-not form_type
      (throw (ex-info "Missing required parameter: form_type"
                      {:inputs inputs})))
    (when-not content
      (throw (ex-info "Missing required parameter: content"
                      {:inputs inputs})))
    ;; Return validated inputs
    {:file_path file-path
     :form_name form_name
     :form_type form_type
     :content content}))

(defmethod tool-system/execute-tool :clojure-edit-insert-before-form [config inputs]
  (let [{:keys [file_path form_name form_type content]} inputs
        result (pipeline/edit-form-pipeline file_path form_name form_type content :before config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-insert-before-form [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Implement the required multimethods for insert after form tool

(defmethod tool-system/tool-name :clojure-edit-insert-after-form [_]
  "clojure_edit_insert_after_definition")

(defmethod tool-system/tool-description :clojure-edit-insert-after-form [_]
  "Inserts content after a top-level form in a Clojure file.
   
   This tool adds new content after a specific top-level form (like a function or def)
   without modifying the form itself. The form is identified by its type and name.
   
   Example: Insert a test function after 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(deftest example-fn-test\n  (is (= 4 (example-fn 2))))\"
   
   Note: For defmethod forms, you can specify either just the method name (\"area\") or 
   include the dispatch value (\"area :rectangle\"). When using just the method name, 
   the dispatch value will be automatically extracted from the replacement content.
   
   The tool will find the form, insert the new content after it, and format the result.
   It returns the updated file content, the location offsets, and a diff of the changes.")

(defmethod tool-system/tool-schema :clojure-edit-insert-after-form [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file containing the form"}
                :form_name {:type :string
                            :description "Name of the form (e.g., function name)"}
                :form_type {:type :string
                            :description "Type of the form (e.g., \"defn\", \"def\", \"ns\")"}
                :content {:type :string
                          :description "Content to insert after the form"}}
   :required [:file_path :form_name :form_type :content]})

(defmethod tool-system/validate-inputs :clojure-edit-insert-after-form [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [form_name form_type content]} inputs]
    (when-not form_name
      (throw (ex-info "Missing required parameter: form_name"
                      {:inputs inputs})))
    (when-not form_type
      (throw (ex-info "Missing required parameter: form_type"
                      {:inputs inputs})))
    (when-not content
      (throw (ex-info "Missing required parameter: content"
                      {:inputs inputs})))
    ;; Return validated inputs
    {:file_path file-path
     :form_name form_name
     :form_type form_type
     :content content}))

(defmethod tool-system/execute-tool :clojure-edit-insert-after-form [config inputs]
  (let [{:keys [file_path form_name form_type content]} inputs
        result (pipeline/edit-form-pipeline file_path form_name form_type content :after config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-insert-after-form [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Implement the required multimethods for docstring edit tool

(defmethod tool-system/tool-name :clojure-edit-replace-docstring [_]
  "clojure_edit_replace_docstring")

(defmethod tool-system/tool-description :clojure-edit-replace-docstring [_]
  "Edits only the docstring of a top-level form in a Clojure file.
   
   This tool updates the docstring of a function, def, or other form without
   modifying the rest of the form. The form is identified by its type and name.
   
   Example: Update the docstring of 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - docstring: \"Takes an integer and doubles it.\"
   
   The tool will find the form, update only its docstring, and format the result.
   It returns the updated file content, the location offsets, and a diff of the changes.")

(defmethod tool-system/tool-schema :clojure-edit-replace-docstring [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file containing the form"}
                :form_name {:type :string
                            :description "Name of the form (e.g., function name)"}
                :form_type {:type :string
                            :description "Type of the form (e.g., \"defn\", \"def\")"}
                :docstring {:type :string
                            :description "New docstring content"}}
   :required [:file_path :form_name :form_type :docstring]})

(defmethod tool-system/validate-inputs :clojure-edit-replace-docstring [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [form_name form_type docstring]} inputs]
    (when-not form_name
      (throw (ex-info "Missing required parameter: form_name"
                      {:inputs inputs})))
    (when-not form_type
      (throw (ex-info "Missing required parameter: form_type"
                      {:inputs inputs})))
    (when-not docstring
      (throw (ex-info "Missing required parameter: docstring"
                      {:inputs inputs})))
    ;; Return validated inputs
    {:file_path file-path
     :form_name form_name
     :form_type form_type
     :docstring docstring}))

(defmethod tool-system/execute-tool :clojure-edit-replace-docstring [config inputs]
  (let [{:keys [file_path form_name form_type docstring]} inputs
        result (pipeline/docstring-edit-pipeline file_path form_name form_type docstring config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-replace-docstring [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Implement the required multimethods for comment block edit tool

(defmethod tool-system/tool-name :clojure-edit-comment-block [_]
  "clojure_edit_replace_comment_block")

(defmethod tool-system/tool-description :clojure-edit-comment-block [_]
  "Replaces an entire comment block in a Clojure file by finding the first block containing the specified substring.

For line comments (;;): A 'block' is a contiguous sequence of comment lines with no blank lines or code between them.

For comment forms: A 'block' is the entire (comment ...) form including all of its contents. When editing a comment form, you must include the (comment ...) wrapper in your new content.

The function searches for the first occurrence of the comment_substring in comment blocks and replaces the entire containing block with the new content. It cannot make partial edits within a block.

For reliable results, use a unique substring that appears in only one comment block.")

(defmethod tool-system/tool-schema :clojure-edit-comment-block [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file containing the comment"}
                :comment_substring {:type :string
                                    :description "Substring to identify the comment block"}
                :new_content {:type :string
                              :description "The replacement comment block"}}
   :required [:file_path :comment_substring :new_content]})

(defmethod tool-system/validate-inputs :clojure-edit-comment-block [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [comment_substring new_content]} inputs]
    (when-not comment_substring
      (throw (ex-info "Missing required parameter: comment_substring"
                      {:inputs inputs})))
    (when-not new_content
      (throw (ex-info "Missing required parameter: new_content"
                      {:inputs inputs})))
    ;; Return validated inputs
    {:file_path file-path
     :comment_substring comment_substring
     :new_content new_content}))

(defmethod tool-system/execute-tool :clojure-edit-comment-block [config inputs]
  (let [{:keys [file_path comment_substring new_content]} inputs
        result (pipeline/comment-block-edit-pipeline file_path comment_substring new_content config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-comment-block [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Implement the required multimethods for file structure tool

(defmethod tool-system/tool-name :clojure-file-structure [_]
  "clojure_file_structure")

(defmethod tool-system/tool-description :clojure-file-structure [_]
  "Generates a collapsed view of a Clojure file showing only function signatures.
   
   This tool creates a simplified view of a Clojure file where function bodies
   are collapsed, showing only the top-level forms' signatures. This is useful for
   understanding the structure of a file without seeing all implementation details.
   
   Example: Generate a file outline:
   - file_path: \"/path/to/file.clj\"
   - expand_symbols: [\"main-function\"] (optional)
   
   The tool will return a simplified view of the file with function bodies replaced by
   '...', except for any symbols listed in expand_symbols (which will be shown in full).")

(defmethod tool-system/tool-schema :clojure-file-structure [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the Clojure file"}
                :expand_symbols {:type :array
                                 :items {:type :string}
                                 :description "Optional list of symbol names to show in expanded form"}}
   :required [:file_path]})

(defmethod tool-system/validate-inputs :clojure-file-structure [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        expand-symbols (get inputs :expand_symbols [])]
    ;; Return validated inputs
    {:file_path file-path
     :expand_symbols expand-symbols}))

(defmethod tool-system/execute-tool :clojure-file-structure [config inputs]
  (let [{:keys [file_path expand_symbols]} inputs
        result (pipeline/file-outline-pipeline file_path expand_symbols config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-file-structure [_ {:keys [error message result]}]
  (if error
    {:result [message]
     :error true}
    {:result result
     :error false}))

;; Backward compatibility functions

(defn top-level-form-edit-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-replace-form-tool nrepl-client-atom)))

(defn top-level-form-insert-before-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-insert-before-form-tool nrepl-client-atom)))

(defn top-level-form-insert-after-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-insert-after-form-tool nrepl-client-atom)))

(defn docstring-edit-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-docstring-tool nrepl-client-atom)))

(defn comment-block-edit-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-comment-block-tool nrepl-client-atom)))

(defn clojure-file-outline-tool [nrepl-client-atom]
  (tool-system/registration-map (create-file-structure-tool nrepl-client-atom)))

(defn create-edit-replace-sexp-tool
  "Creates the tool configuration for replacing s-expressions in a file.
   Automatically inherits emacs notification preferences from the client."
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-edit-replace-sexp
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

(defmethod tool-system/tool-name :clojure-edit-replace-sexp [_]
  "clojure_edit_replace_sexp")

(defmethod tool-system/tool-description :clojure-edit-replace-sexp [_]
  "Edits a file by finding and replacing occurrences of a specific s-expression.

Use this tool for sub-expressions, top level definitions like `defn`, `ns`, `def` are better edited by the `clojure_edit_replace_definition`, `clojure_edit_insert_before_definition`, and `clojure_edit_insert_after_definition` which all save a lot of tokens which makes me happy. 

To delete forms, use an empty string as new_form
   
The tool returns a diff showing the changes made to the file.")

(defmethod tool-system/tool-schema :clojure-edit-replace-sexp [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "Path to the file to edit"}
                :match_form {:type :string
                             :description "The s-expression to find and replace (include # for anonymous functions)"}
                :new_form {:type :string
                           :description "The s-expression to replace with"}
                :replace_all {:type :boolean
                              :description "Whether to replace all occurrences (default: false)"}
                :whitespace_sensitive {:type :boolean
                                       :description "Whether to match forms exactly as written including whitespace (default: false)"}}
   :required [:file_path :match_form :new_form]})

(defmethod tool-system/validate-inputs :clojure-edit-replace-sexp [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [match_form new_form replace_all whitespace_sensitive]} inputs]
    (when-not match_form
      (throw (ex-info "Missing required parameter: match_form"
                      {:inputs inputs})))
    (when-not new_form
      (throw (ex-info "Missing required parameter: new_form"
                      {:inputs inputs})))

    ;; Special handling for empty string
    (when-not (str/blank? match_form)
      ;; Validate that match_form is valid Clojure code and contains only one expression
      (try
        (let [all-forms (p/parse-string-all match_form)
              form-count (count (n/children all-forms))]
          (when (> form-count 1)
            (throw (ex-info (str "match_form must contain only a single s-expression. Found " form-count " forms. Use just one form like '(+ x y)'.")
                            {:inputs inputs}))))
        (catch Exception e
          (throw (ex-info (str "Invalid Clojure code in match_form: " (.getMessage e))
                          {:inputs inputs})))))

    ;; Special handling for empty string
    (when-not (str/blank? new_form)
      ;; Validate that new_form is valid Clojure code
      (try
        (p/parse-string-all new_form)
        (catch Exception e
          (throw (ex-info (str "Invalid Clojure code in new_form: " (.getMessage e))
                          {:inputs inputs})))))

    ;; Return validated inputs
    {:file_path file-path
     :match_form match_form
     :new_form new_form
     :replace_all (boolean (or replace_all false))
     :whitespace_sensitive (boolean (or whitespace_sensitive false))}))

(defmethod tool-system/execute-tool :clojure-edit-replace-sexp [config inputs]
  (let [{:keys [file_path match_form new_form replace_all whitespace_sensitive]} inputs
        result (pipeline/sexp-replace-pipeline
                file_path match_form new_form replace_all whitespace_sensitive config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-edit-replace-sexp [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

;; Function to register the tool
(defn sexp-replace-tool [nrepl-client-atom]
  (tool-system/registration-map (create-edit-replace-sexp-tool nrepl-client-atom)))

(comment
  ;; === Examples of using the form editing tools ===
  (require 'clojure-mcp.nrepl)
  ;; Setup for REPL-based testing
  (def client-atom (atom (assoc
                          (clojure-mcp.nrepl/create {:port 7888})
                          :clojure-mcp.core/nrepl-user-dir (System/getProperty "user.dir"))))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Create tool instances
  (def replace-tool (create-edit-replace-form-tool client-atom))
  (def insert-before-tool (create-edit-insert-before-form-tool client-atom))
  (def insert-after-tool (create-edit-insert-after-form-tool client-atom))
  (def docstring-tool (create-edit-docstring-tool client-atom))
  (def comment-tool (create-edit-comment-block-tool client-atom))
  (def outline-tool (create-file-structure-tool client-atom))
  (def sexp-tool (create-edit-replace-sexp-tool client-atom))

  ;; Test the replace form tool
  (def replace-inputs
    {:file_path "tmp/test.clj"
     :form_name "power"
     :form_type "defn"
     :content "(defn power [x]\n  (* x 55))"})
  (def replace-validated (tool-system/validate-inputs replace-tool replace-inputs))
  (def replace-result (tool-system/execute-tool replace-tool replace-validated))
  (def replace-formatted (tool-system/format-results replace-tool replace-result))

  ;; Test the comment edit tool
  (def comment-inputs
    {:file_path "/tmp/test.clj"
     :comment_substring "TODO"
     :new_content ";; DONE: Implemented feature"})
  (def comment-validated (tool-system/validate-inputs comment-tool comment-inputs))
  (def comment-result (tool-system/execute-tool comment-tool comment-validated))
  (def comment-formatted (tool-system/format-results comment-tool comment-result))

  ;; Test the sexp replace tool
  (def sexp-inputs
    {:file_path "/tmp/test.clj"
     :match_form "(+ 1 2)"
     :new_form "(+ 1 3)"
     :replace_all true})
  (def sexp-validated (tool-system/validate-inputs sexp-tool sexp-inputs))
  (def sexp-result (tool-system/execute-tool sexp-tool sexp-validated))
  (def sexp-formatted (tool-system/format-results sexp-tool sexp-result))

  ;; Test with anonymous functions
  (def anon-fn-inputs
    {:file_path "/tmp/test.clj"
     :match_form "#(+ x 2)"
     :new_form "#(+ x 5)"
     :replace_all true})
  (def anon-fn-validated (tool-system/validate-inputs sexp-tool anon-fn-inputs))
  (def anon-fn-result (tool-system/execute-tool sexp-tool anon-fn-validated))
  (def anon-fn-formatted (tool-system/format-results sexp-tool anon-fn-result))

  ;; Test with empty strings
  (def empty-str-inputs
    {:file_path "/tmp/test.clj"
     :match_form ""
     :new_form "(comment \"This replaces empty nodes\")"
     :replace_all true})
  (def empty-str-validated (tool-system/validate-inputs sexp-tool empty-str-inputs))
  (def empty-str-result (tool-system/execute-tool sexp-tool empty-str-validated))
  (def empty-str-formatted (tool-system/format-results sexp-tool empty-str-result))

  ;; Make a simpler test function
  (defn test-tool [tool inputs]
    (let [prom (promise)
          reg-map (tool-system/registration-map tool)
          tool-fn (:tool-fn reg-map)]
      (tool-fn nil inputs
               (fn [result error]
                 (deliver prom (if error {:error error} {:result result}))))
      @prom))

  ;; Test various tools
  (test-tool replace-tool replace-inputs)
  (test-tool comment-tool comment-inputs)
  (test-tool outline-tool {:file_path "/tmp/test.clj"})
  (test-tool sexp-tool sexp-inputs)
  (test-tool sexp-tool anon-fn-inputs)
  (test-tool sexp-tool empty-str-inputs)

  ;; Clean up
  (clojure-mcp.nrepl/stop-polling @client-atom))
