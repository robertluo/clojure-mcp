(ns clojure-mcp.tools.file-write.tool
  "Implementation of the file-write tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.file-write.core :as core]
   [clojure-mcp.repl-tools.utils :as utils]))

;; Factory function to create the tool configuration
(defn create-file-write-tool
  "Creates the file-write tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :file-write
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the file-write tool
(defmethod tool-system/tool-name :file-write [_]
  "file_write")

(defmethod tool-system/tool-description :file-write [_]
  "Write a file to the local filesystem. Overwrites the existing file if there is one. 
The content will be linted and formatted according to Clojure standards before writing.

Returns information about whether the file was created or updated, along with a diff 
showing the changes made.

Before using this tool:
1. Use the read_file tool to understand the file's contents and context
2. Directory Verification (only applicable when creating new files):
   - Use the list_directory tool to verify the parent directory exists and is the correct location

# Example:
# file_write(
#   file_path: \"/absolute/path/to/file.clj\",
#   content: \"(ns my.namespace)\\n\\n(defn my-function [x]\\n  (* x 2))\"
# )")

(defmethod tool-system/tool-schema :file-write [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "The absolute path to the file to write (must be absolute, not relative)"}
                :content {:type :string
                          :description "The content to write to the file"}}
   :required [:file_path :content]})

(defmethod tool-system/validate-inputs :file-write [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [file_path content]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not file_path
      (throw (ex-info "Missing required parameter: file_path" {:inputs inputs})))

    (when-not content
      (throw (ex-info "Missing required parameter: content" {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (utils/validate-path-with-client file_path nrepl-client)]
      ;; Return validated inputs with normalized path
      {:file-path validated-path
       :content content})))

(defmethod tool-system/execute-tool :file-write [_ inputs]
  (let [{:keys [file-path content]} inputs]
    (core/write-file file-path content)))

(defmethod tool-system/format-results :file-write [_ result]
  (if (:error result)
    ;; If there's an error, return the error message
    {:result [(:message result)]
     :error true}
    ;; Otherwise, format a successful result
    (let [file-type (if (core/is-clojure-file? (:file-path result)) "Clojure" "Text")
          response (str file-type " file " (:type result) "d: " (:file-path result))]
      (if (seq (:diff result))
        ;; If there's a diff, include it in the response
        {:result [(str response "\nChanges:\n" (:diff result))]
         :error false}
        ;; Otherwise, just show the success message
        {:result [response]
         :error false}))))

;; Backward compatibility function that returns the registration map
(defn file-write-tool
  "Returns the registration map for the file-write tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-file-write-tool nrepl-client-atom)))