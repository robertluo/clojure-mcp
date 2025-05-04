(ns clojure-mcp.tools.unified-clojure-edit.tool
  "MCP tool implementation for the unified Clojure edit operation.
   Provides a pattern-based approach to finding and editing Clojure code."
  (:require [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.unified-clojure-edit.pipeline :as clj-edit-pipeline]
            [clojure-mcp.tools.form-edit.pipeline :as pipeline]
            [clojure-mcp.repl-tools.utils :as utils]
            [clojure.tools.logging :as log]
            [clojure.string :as str]))

;; Tool name and description
(defmethod tool-system/tool-name :clojure-pattern-edit [_]
  "clojure_edit")

(defmethod tool-system/tool-description :clojure-pattern-edit [_]
"Clojure code editing tool that uses simple wildcards to match and edit code.

This tool provides a powerful efficient approach to edit Clojure code using pattern matching instead of having to specify exact textual matches. The pattern syntax supports two wildcards:
   
   - `_?` matches exactly one form
   - `_*` matches zero or more forms
   
The pattern matches the sexpr of the code ignoring whitespace.

These are example matches
 
  - `(defn hello _? _*)` would match \"(defn   hello   [a b]\n\n (str a b))\"

  - `(list 1 2 3 4)` would match \"(list 1\n2\n3\n4)\"
  - `(list _* 3 4)` would match \"(list 1\n2\n3\n4)\"
  - `(list 1 2 _? 4)` would match \"(list 1\n2\n3\n4)\"
  - `(_? 1 2 3 4)` would match \"(list 1\n2\n3\n4)\"

The match is the anchor point for the operation.

This tool has three operations
  - \"replace\" replaces the matched sexpr with the new content  
  - \"insert-after\" inserts the new content after the matched sexpr
  - \"insert-before\" inserts the new content before the matched sexpr

PREFER this tool for editing Clojure files (`.clj` `.cljs` `.cljc` `.bb`)

These tools MAKE it EASIER to match a definition that exists in the file AS you only have to match the shape of a definition `(defn hello _*)`. This prevents the repeated mismatch errors that occur when trying match an entire `old_string` to replace it.

These tools validates the structure of the structure of the Clojure code that is being inserted into the file and will provide linting feedback for things such as parenthetical errors.

This tool reduces the number of tokens that need to be generated and that makes me happy!
 
WARNING: you will receive errors if the syntax is wrong, the most common error is an extra or missing parenthesis at the end of the replacement function in `content`, so be careful with parenthesis.

This tool will mostly be used to operation on top level forms (defn, def, deftest, s/def, ns, defmethod etc.) with new content. The top level form is easily matched with a simple pattern that includes the identifiers for the form like `(defmethod shape/area :rectangle _*)` where the `_*` matches the rest of the forms in the definition.   
   
   Example: Replace the implementation of a `defn` named `example-fn`:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"(defn example-fn _*)\"
   - content: \"(defn example-fn [x] (+ x 2))\"
   - operation: \"replace\"
   
Note: For `defmethod` forms, be sure to include the dispatch value (`area :rectangle` or `qualified/area :rectangle`) in the `form_identifier`. Many `defmethod` definitions have qualified names (they include a namespace alias in their identifier like `shape/area`), so it's crucial to use the complete identifier that appears in the file. 

   Example: Replace a namespace:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"(ns my-cool-proj.core _*)\"
   - content: \"(ns my-cool-proj.core\n (:requires [clojure.string :as string]))\"
   - operation: \"replace\"

   Example: Replace the implementation of a `defmethod` named `shape/area :square`:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"(defmethod shape/area :square _*)\"
   - content: \"(defmethod shape/area :square [{:keys [w h]}] (* w h))\"
   - operation: \"replace\"

   Example: Replace the implementation of a `defmethod` with a namespaced multimethod:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"(defmethod tool-system/validate-inputs :clojure-eval _*)\"
   - content: \"(defmethod tool-system/validate-inputs :clojure-eval [_ inputs] ...)\"
   - operation: \"replace\"

   Example: Insert a new defmethod after the implementation of a `defmethod` named `convert-length [:meters :inches]`:
   - file_path: \"/path/to/file.clj\"
   - match: \"(defmethod convert-length [:feet :inches] _*)\"
   - content: \"(defmethod convert-length [:feet :yard] [_ n] (/ n 3))\"
   - operation: \"insert-after\"

You can also edit sublevel forms

   Example: Edit a namespace changing an alias:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"[clojure.string :as _?]\"
   - content: \"[clojure.string :as str]\"
   - operation: \"replace\"

   Example: Edit a namespace adding a library:
   - file_path: \"/path/to/file.clj\"
   - pattern: \"[clojure.string :as _?]\"
   - content: \"[clojure.java.io :as io]\n  [clojure.set :as set]  \n\"
   - operation: \"insert-after\"

THis tool can also target explicit sexps when used without the pattern symbols.

   This approach is more flexible than traditional form-edit tools as it doesn't require perfect text matches for the edit to succeed, making it easier to target specific code structures with simpler patterns.")

;; Tool schema
(defmethod tool-system/tool-schema :clojure-pattern-edit [_]
  {:type :object
   :properties
   {:file_path
    {:type :string
     :description "Path to the file to edit"}

    :pattern
    {:type :string
     :description "Pattern to match using _? (single form) and _* (multiple forms) wildcards"}

    :content
    {:type :string
     :description "New content to replace or insert"}

    :operation
    {:enum ["replace" "insert-before" "insert-after"]
     :description "Edit operation to perform"}}

   :required ["file_path" "pattern" "content" "operation"]})

;; Validate inputs
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

(defmethod tool-system/validate-inputs :clojure-pattern-edit [{:keys [nrepl-client-atom]} inputs]
  (let [file-path (validate-file-path inputs nrepl-client-atom)
        {:keys [pattern content operation]} inputs]
    (when-not pattern
      (throw (ex-info "Missing required parameter: pattern"
                      {:inputs inputs})))
    (when-not content
      (throw (ex-info "Missing required parameter: content"
                      {:inputs inputs})))
    (when-not (contains? #{"replace" "insert-before" "insert-after"} operation)
      (throw (ex-info "Operation must be one of: replace, insert-before, insert-after"
                      {:inputs inputs
                       :operation operation})))

    ;; Return validated inputs
    {:file_path file-path
     :pattern pattern
     :content content
     :operation (keyword operation)}))

;; Execute the tool
(defmethod tool-system/execute-tool :clojure-pattern-edit [{:keys [nrepl-client-atom] :as tool} inputs]
  (let [{:keys [file_path pattern content operation]} inputs]

    (log/info "Executing clojure_pattern_edit"
              {:file file_path
               :pattern pattern
               :operation operation})
        ;formatted-result (cljpipeline/format-result result)
    (pipeline/format-result
     (clj-edit-pipeline/pattern-edit-pipeline
      file_path
      pattern
      content
      operation
      tool))))


;; Format the results for output
(defmethod tool-system/format-results :clojure-pattern-edit [_ {:keys [error message diff]}]
  (if error
    {:result [message]
     :error true}
    {:result [diff]
     :error false}))

(defn clojure-pattern-edit-tool
  "Factory function that creates a tool for pattern-based Clojure editing.
   
   Arguments:
   - nrepl-client-atom: Atom containing the nREPL client
   
   Returns:
   - A map specifying the tool for registration with MCP"
  [nrepl-client-atom]
  (let [client @nrepl-client-atom
        emacs-notify (boolean (:clojure-mcp.core/emacs-notify client))]
    {:tool-type :clojure-pattern-edit
     :nrepl-client-atom nrepl-client-atom
     :enable-emacs-notifications emacs-notify}))

;; Function to register the tool
(defn clojure-edit-tool [nrepl-client-atom]
  (tool-system/registration-map (clojure-pattern-edit-tool nrepl-client-atom)))



(comment
  (def client-atom (atom (assoc
                          { } ;; (clojure-mcp.nrepl/create {:port 7888})
                          :clojure-mcp.core/nrepl-user-dir (System/getProperty "user.dir"))))
  (def tool (clojure-pattern-edit-tool client-atom))

  

  )
