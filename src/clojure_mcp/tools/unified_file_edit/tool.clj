(ns clojure-mcp.tools.unified-file-edit.tool
  "Implementation of the unified-file-edit tool using the tool-system multimethod approach.
   This tool combines the functionality of file_write and file_edit into a single
   smart tool that automatically selects the appropriate mode based on parameters."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.file-write.core :as file-write-core]
   [clojure-mcp.tools.file-edit.pipeline :as file-edit-pipeline]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Factory function to create the tool configuration
(defn create-unified-file-edit-tool
  "Creates the unified-file-edit tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :unified-file-edit
   :nrepl-client-atom nrepl-client-atom})

;; Helper functions
(defn determine-edit-mode
  "Determines which mode to use based on the provided parameters.
   
   Parameters:
   - old-string: The string to replace (nil or empty means write mode)
   
   Returns:
   - :write or :edit keyword indicating the mode"
  [old-string]
  (if (or (nil? old-string) (empty? old-string))
    :write
    :edit))

;; Implement the required multimethods for the unified file edit tool
(defmethod tool-system/tool-name :unified-file-edit [_]
  "file_edit")

(defmethod tool-system/tool-description :unified-file-edit [_]
  "Edit a file by either replacing specific text or writing complete content.

1. EDIT MODE (when old_string is provided):
   - Replaces a specific text segment while preserving the rest of the file
   - For safety, requires old_string to appear exactly once in the file

2. WRITE MODE (when old_string is missing or blank):
   - Creates a new file or completely overwrites an existing file

Returns a diff showing the changes made to the file.")

(defmethod tool-system/tool-schema :unified-file-edit [_]
  {:type :object
   :properties {:file_path {:type :string
                            :description "The absolute path to the file to edit (must be absolute, not relative)"}
                :old_string {:type :string
                             :description "The text to replace (leave blank to write the entire file)"}
                :new_string {:type :string
                             :description "The new content to write (entire file or replacement segment)"}}
   :required [:file_path :new_string]})

(defmethod tool-system/validate-inputs :unified-file-edit [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [file_path old_string new_string]} inputs
        nrepl-client @nrepl-client-atom]

    ;; Check required parameters
    (when-not file_path
      (throw (ex-info "Missing required parameter: file_path" {:inputs inputs})))

    (when-not (contains? inputs :new_string)
      (throw (ex-info "Missing required parameter: new_string" {:inputs inputs})))

    ;; Determine mode
    (let [mode (determine-edit-mode old_string)]
      ;; If in edit mode with identical strings, reject early
      (when (and (= mode :edit) (= old_string new_string))
        (throw (ex-info "No changes to make: old_string and new_string are exactly the same."
                        {:inputs inputs})))

      ;; Validate path using the utility function
      (let [validated-path (utils/validate-path-with-client file_path nrepl-client)]
        ;; Return validated inputs with normalized path and mode
        {:file-path validated-path
         :old-string old_string
         :new-string new_string
         :mode mode}))))

(defmethod tool-system/execute-tool :unified-file-edit [_ inputs]
  (let [{:keys [file-path old-string new-string mode]} inputs]
    (case mode
      :write
      ;; Use file-write functionality
      (file-write-core/write-file file-path new-string)

      :edit
      ;; Use file-edit functionality
      (let [result (file-edit-pipeline/file-edit-pipeline file-path old-string new-string)]
        (file-edit-pipeline/format-result result)))))

(defmethod tool-system/format-results :unified-file-edit [_ result]
  (if (:error result)
    ;; If there's an error, return the error message
    {:result [(:message result)]
     :error true}
    ;; Otherwise, format a successful result
    (let [file-path (:file-path result)
          file-type (if (and file-path (file-write-core/is-clojure-file? file-path)) "Clojure" "Text")
          response (str file-type " file " (:type result) "d: " file-path)]
      (if (seq (:diff result))
        ;; If there's a diff, include it in the response
        {:result [(:diff result)]
         :error false}
        ;; Otherwise, just show the success message
        {:result [response]
         :error false}))))

;; Function that returns the registration map
(defn unified-file-edit-tool
  "Returns the registration map for the unified-file-edit tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-unified-file-edit-tool nrepl-client-atom)))
