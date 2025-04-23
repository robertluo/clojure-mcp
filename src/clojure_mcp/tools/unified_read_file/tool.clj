(ns clojure-mcp.tools.unified-read-file.tool
  "Implementation of the unified-read-file tool using the tool-system multimethod approach.
   This tool combines the functionality of fs_read_file and clojure_read_file into a single
   smart tool that automatically selects the appropriate mode based on file type."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.unified-read-file.core :as core]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.java.io :as io]))

;; Factory function to create the tool configuration
(defn create-unified-read-file-tool
  "Creates the unified-read-file tool configuration with optional parameters.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client
   - opts: Optional map with configuration options:
     - :max-lines: Maximum number of lines to read (default: 2000)
     - :max-line-length: Maximum length per line before truncation (default: 1000)"
  ([nrepl-client-atom]
   (create-unified-read-file-tool nrepl-client-atom {}))
  ([nrepl-client-atom {:keys [max-lines max-line-length]
                       :or {max-lines 2000
                            max-line-length 1000}}]
   {:tool-type :unified-read-file
    :nrepl-client-atom nrepl-client-atom
    :max-lines max-lines
    :max-line-length max-line-length}))

;; Implement the required multimethods for the unified read file tool
(defmethod tool-system/tool-name :unified-read-file [_]
  "read_file")

(defmethod tool-system/tool-description :unified-read-file [{:keys [max-lines max-line-length]}]
  (str "Reads file contents with intelligent handling based on file type.
   
For Clojure files (.clj, .cljc, .cljs):
- Shows a collapsed view with function signatures by default
- Can display complete implementations of specified functions
- Includes helpful notice explaining how to expand specific functions or view raw content

For all other file types:
- Shows raw file contents with optional line limits and offsets

Parameters:
- path: Path to the file (required)
- clojure_mode: Control Clojure-specific formatting (auto|on|off, default: auto)
- expand_symbols: List of function names to show in full (Clojure mode only)
- line_offset: Line to start reading from (non-Clojure mode only, default: 0)
- limit: Maximum lines to read (non-Clojure mode only, default: " max-lines ")

By default, reads up to " max-lines " lines, truncating lines longer than " max-line-length " characters.
This unified tool combines the functionality of fs_read_file and clojure_read_file."))

(defmethod tool-system/tool-schema :unified-read-file [_]
  {:type :object
   :properties {:path {:type :string
                       :description "Path to the file to read"}
                :clojure_mode {:type :string
                               :enum ["auto" "on" "off"]
                               :description "Control Clojure-specific formatting: auto(detect), on(force), off(raw)"}
                :expand_symbols {:type :array
                                 :items {:type :string}
                                 :description "Function names to show in expanded form (Clojure mode only)"}
                :line_offset {:type :integer
                              :description "Line to start reading from (non-Clojure mode only)"}
                :limit {:type :integer
                        :description "Maximum lines to read (non-Clojure mode only)"}}
   :required [:path]})

(defmethod tool-system/validate-inputs :unified-read-file [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path clojure_mode expand_symbols line_offset limit]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    ;; Validate mode value
    (when (and clojure_mode (not (contains? #{"auto" "on" "off"} clojure_mode)))
      (throw (ex-info "Invalid clojure_mode value. Must be 'auto', 'on', or 'off'."
                      {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (utils/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      {:path validated-path
       :clojure_mode (or clojure_mode "auto")
       :expand_symbols (or expand_symbols [])
       :line_offset (or line_offset 0)
       :limit limit})))

(defmethod tool-system/execute-tool :unified-read-file [{:keys [max-lines max-line-length]} inputs]
  (let [{:keys [path clojure_mode expand_symbols line_offset limit]} inputs
        limit-val (or limit max-lines)]
    (core/read-unified-file path clojure_mode expand_symbols line_offset limit-val
                            :max-line-length max-line-length)))

(defmethod tool-system/format-results :unified-read-file [{:keys [max-lines]} {:keys [error content path size line-count offset truncated? line-lengths-truncated? result]}]
  (if error
    ;; If there's an error, return it with error flag true
    {:result (if (vector? result) result [(or error "Unknown error")])
     :error true}
    ;; For Clojure view mode
    (if result
      {:result result
       :error false}
      ;; For raw mode with file content
      (let [file (io/file path)
            size (or size (.length file))
            limit (or max-lines 2000)
            header (-> (str "<file-content path=\"" path "\" "
                            "byte-size=\"" size "\" "
                            "line-count=\"" line-count "\" "
                            "line-offset=\"" offset "\" "
                            "line-limit=\"" limit "\" "
                            "truncated=\"" (boolean truncated?) "\" ")
                       (cond->
                        line-lengths-truncated? (str "line-lengths-truncated=\"true\" "))
                       (str ">\n"))
            formatted (str header content "\n</file-content>")]
        {:result [formatted]
         :error false}))))

;; Function to register the tool that returns the registration map
(defn unified-read-file-tool
  "Returns the registration map for the unified-read-file tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client
   - opts: Optional map with configuration options:
     - :max-lines: Maximum number of lines to read (default: 2000)
     - :max-line-length: Maximum length per line before truncation (default: 1000)"
  ([nrepl-client-atom]
   (unified-read-file-tool nrepl-client-atom {}))
  ([nrepl-client-atom opts]
   (tool-system/registration-map (create-unified-read-file-tool nrepl-client-atom opts))))
