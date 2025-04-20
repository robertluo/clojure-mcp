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
   [clojure.string :as str]))

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
  "Validates that a file path is provided"
  [inputs]
  (let [{:keys [file_path]} inputs]
    (when-not file_path
      (throw (ex-info "Missing required parameter: file_path"
                      {:inputs inputs})))
    file_path))

;; Implement the required multimethods for the form replacement tool

(defmethod tool-system/tool-name :clojure-edit-replace-form [_]
  "clojure_edit_replace_form")

(defmethod tool-system/tool-description :clojure-edit-replace-form [_]
  "Edits a top-level form in a Clojure file by fully replacing it.
   
   This tool can replace a specific top-level form (like a function, def, or namespace declaration)
   with new content. The form is identified by its type (defn, def, etc.) and name.
   
   Example: Replace the implementation of a function named 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(defn example-fn [x] (* x 2))\"
   
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

(defmethod tool-system/validate-inputs :clojure-edit-replace-form [_ inputs]
  (let [file-path (validate-file-path inputs)
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

(defmethod tool-system/format-results :clojure-edit-replace-form [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

;; Implement the required multimethods for insert before form tool

(defmethod tool-system/tool-name :clojure-edit-insert-before-form [_]
  "clojure_edit_insert_before_form")

(defmethod tool-system/tool-description :clojure-edit-insert-before-form [_]
  "Inserts content before a top-level form in a Clojure file.
   
   This tool adds new content before a specific top-level form (like a function or def)
   without modifying the form itself. The form is identified by its type and name.
   
   Example: Insert a new function before 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(defn helper-fn [x] (* x 2))\"
   
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

(defmethod tool-system/validate-inputs :clojure-edit-insert-before-form [_ inputs]
  (let [file-path (validate-file-path inputs)
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

(defmethod tool-system/format-results :clojure-edit-insert-before-form [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

;; Implement the required multimethods for insert after form tool

(defmethod tool-system/tool-name :clojure-edit-insert-after-form [_]
  "clojure_edit_insert_after_form")

(defmethod tool-system/tool-description :clojure-edit-insert-after-form [_]
  "Inserts content after a top-level form in a Clojure file.
   
   This tool adds new content after a specific top-level form (like a function or def)
   without modifying the form itself. The form is identified by its type and name.
   
   Example: Insert a test function after 'example-fn':
   - file_path: \"/path/to/file.clj\"
   - form_name: \"example-fn\"
   - form_type: \"defn\"
   - content: \"(deftest example-fn-test\n  (is (= 4 (example-fn 2))))\"
   
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

(defmethod tool-system/validate-inputs :clojure-edit-insert-after-form [_ inputs]
  (let [file-path (validate-file-path inputs)
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

(defmethod tool-system/format-results :clojure-edit-insert-after-form [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

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

(defmethod tool-system/validate-inputs :clojure-edit-replace-docstring [_ inputs]
  (let [file-path (validate-file-path inputs)
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

(defmethod tool-system/format-results :clojure-edit-replace-docstring [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

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

(defmethod tool-system/validate-inputs :clojure-edit-comment-block [_ inputs]
  (let [file-path (validate-file-path inputs)
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

(defmethod tool-system/format-results :clojure-edit-comment-block [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

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

(defmethod tool-system/validate-inputs :clojure-file-structure [_ inputs]
  (let [file-path (validate-file-path inputs)
        expand-symbols (get inputs :expand_symbols [])]
    ;; Return validated inputs
    {:file_path file-path
     :expand_symbols expand-symbols}))

(defmethod tool-system/execute-tool :clojure-file-structure [config inputs]
  (let [{:keys [file_path expand_symbols]} inputs
        result (pipeline/file-outline-pipeline file_path expand_symbols config)
        formatted-result (pipeline/format-result result)]
    formatted-result))

(defmethod tool-system/format-results :clojure-file-structure [_ result]
  ;; The pipeline/format-result already formats results correctly
  result)

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

(comment
  ;; === Examples of using the form editing tools ===

  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  
  ;; Create tool instances
  (def replace-tool (create-edit-replace-form-tool client-atom))
  (def insert-before-tool (create-edit-insert-before-form-tool client-atom))
  (def insert-after-tool (create-edit-insert-after-form-tool client-atom))
  (def docstring-tool (create-edit-docstring-tool client-atom))
  (def comment-tool (create-edit-comment-block-tool client-atom))
  (def outline-tool (create-file-structure-tool client-atom))
  
  ;; Test the replace form tool
  (def replace-inputs 
    {:file_path "/tmp/test.clj"
     :form_name "example-fn"
     :form_type "defn"
     :content "(defn example-fn [x]\n  (* x 2))"})
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
  
  ;; Clean up
  (clojure-mcp.nrepl/stop-polling @client-atom)
)
