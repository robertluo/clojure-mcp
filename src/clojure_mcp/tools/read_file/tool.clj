(ns clojure-mcp.tools.read-file.tool
  "Implementation of the read-file tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.read-file.core :as core]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.java.io :as io]))

;; Factory function to create the tool configuration
(defn create-read-file-tool 
  "Creates the read-file tool configuration"
  [nrepl-client-atom]
  {:tool-type :read-file
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the read-file tool

(defmethod tool-system/tool-description :read-file [_]
  "Reads the contents of a file. The file_path parameter must be an absolute path, not a relative path.
By default, it reads the entire file. You can optionally specify a line offset and limit for large files.
Returns the file contents or an error message if the file cannot be read.")

(defmethod tool-system/tool-schema :read-file [_]
  {:path {:type "string"
          :description "The path to the file to read."}
   :offset {:type "integer"
            :description "Line number to start reading from (0-indexed)"}
   :limit {:type "integer"
           :description "Maximum number of lines to read"}
   :required ["path"]})

(defmethod tool-system/validate-inputs :read-file [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))
    
    ;; Use the existing validate-path-with-client function
    (let [validated-path (utils/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      (assoc inputs :path validated-path))))

(defmethod tool-system/execute-tool :read-file [_ inputs]
  (let [{:keys [path offset limit]} inputs
        offset (or offset 0)
        limit (or limit 2000)]
    (core/read-file path offset limit)))

(defmethod tool-system/format-results :read-file [_ {:keys [error path content truncated? line-count offset max-lines line-lengths-truncated? truncated-by] :as result}]
  (if error
    ;; If there's an error, return it with error flag true
    {:result [error]
     :error true}
    ;; Otherwise, format the file content with metadata
    (let [file (io/file path)
          size (.length file)
          header (-> (str "<file-content path=\"" path "\" "
                         "size=\"" size "\" "
                         "line-count=\"" line-count "\" "
                         "offset=\"" offset "\" "
                         "limit=\"" max-lines "\" "
                         "truncated=\"" (boolean truncated?) "\" ")
                    (cond-> 
                      truncated-by (str "truncated-by=\"" truncated-by "\" ")
                      line-lengths-truncated? (str "line-lengths-truncated=\"true\" "))
                    (str ">\n"))
          formatted (str header content "\n</file-content>")]
      {:result [formatted]
       :error false})))

;; Backward compatibility function that returns the registration map
(defn read-file-tool [nrepl-client-atom]
  (tool-system/registration-map (create-read-file-tool nrepl-client-atom)))