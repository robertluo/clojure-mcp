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
  "clojure_pattern_edit")

(defmethod tool-system/tool-description :clojure-pattern-edit [_]
  "Pattern-based Clojure code editing tool that uses simple wildcards to match and edit code.
   
   This tool provides a unified approach to edit Clojure code using pattern matching
   instead of having to specify exact form types and identifiers. The pattern syntax 
   supports two wildcards:
   
   - ? matches exactly one form
   - * matches zero or more forms
   
   ## Examples
   
   To match a specific function with any arguments:
     `(defn hello [*] ?)`
   
   To match any function with two arguments:
     `(defn ? [? ?] ?)`
   
   To match a specific multimethod:
     `(defmethod handle-request :get [?] ?)`
   
   ## Operations
   
   The tool supports three operations:
   - replace: Replace the matched code with new content
   - insert-before: Insert content before the matched code
   - insert-after: Insert content after the matched code
   
   This approach is more flexible than traditional form-edit tools as it doesn't
   require exact knowledge of form types and names, making it easier to target
   specific code structures with simpler patterns.")

;; Tool schema
(defmethod tool-system/tool-schema :clojure-pattern-edit [_]
  {:type :object
   :properties
   {:file_path
    {:type :string
     :description "Path to the file to edit"}

    :pattern
    {:type :string
     :description "Pattern to match using ? (single form) and * (multiple forms) wildcards"}

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

;; Tool factory function for registration
 ;; Helper function to provide examples of pattern matching
#_(defn pattern-examples
  "Returns examples of pattern usage for the given scenario."
  [scenario]
  (case scenario
    :function
    {:match-any-fn "(defn ? [*] *)"
     :match-specific-fn "(defn my-function [*] *)"
     :match-with-args "(defn ? [arg1 arg2] *)"
     :match-with-docstring "(defn ? ? [*] *)"}

    :defmethod
    {:match-any-method "(defmethod ? ? [*] *)"
     :match-specific-dispatch "(defmethod handle-request :get [*] *)"
     :match-vector-dispatch "(defmethod convert-units [:meters :feet] [*] *)"
     :match-with-ns "(defmethod ns/multimethod ? [*] *)"}

    :threading
    {:match-thread-first "(-> ? ? ?)"
     :match-thread-last "(->> ? ? ?)"
     :match-specific-thread "(-> request auth-middleware ? ?)"}

    ;; Default examples
    {:basic-wildcards "(? ? ?)"
     :single-form "(defn hello [?] ?)"
     :multi-forms "(* hello *)"}))

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
