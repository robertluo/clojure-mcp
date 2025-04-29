(ns clojure-mcp.tools.unified-read-file.tool
  "Implementation of the unified-read-file tool using the tool-system multimethod approach.
   This tool combines the functionality of fs_read_file and clojure_read_file into a single
   smart tool that automatically selects the appropriate mode based on file type."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.read-file.core :as read-file-core]
   [clojure-mcp.tools.form-edit.core :as form-edit-core]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.java.io :as io]
   [clojure.string :as str]))

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

 ;; Helper functions

(defn clojure-file?
  "Determines if a file is a Clojure source file based on its extension.
   Only .clj, .cljc, and .cljs files are considered Clojure files."
  [file-path]
  (when file-path
    (let [extension (last (str/split file-path #"\."))]
      (contains? #{"clj" "cljc" "cljs"} extension))))

;; Implement the required multimethods for the unified read file tool
(defmethod tool-system/tool-name :unified-read-file [_]
  "read_file")

(defmethod tool-system/tool-description :unified-read-file [{:keys [max-lines max-line-length]}]
  (str "Reads file contents with intelligent handling based on file type.
   
For Clojure files (.clj, .cljc, .cljs):
- Shows a collapsed view with function signatures with `clojure-mode:` `on`
- Can display complete implementations of specified functions with expand-symbols
- Returns content wrapped in <collapsed-clojure-view> XML tags with metadata attributes
- Includes concise advice for viewing specific functions or raw content

For all other file types:
- Shows raw file contents with optional line limits and offsets

Parameters:
- path: Path to the file (required)
- clojure_mode: Control Clojure-specific formatting (auto|on|off, default: off)
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
       :clojure_mode (or clojure_mode "off")
       :expand_symbols (or expand_symbols [])
       :line_offset (or line_offset 0)
       :limit limit})))

(defmethod tool-system/execute-tool :unified-read-file [{:keys [max-lines max-line-length]} inputs]
  (let [{:keys [path clojure_mode expand_symbols line_offset limit]} inputs
        limit-val (or limit max-lines)
        is-clojure-file (clojure-file? path)
        use-clojure-mode (or (= clojure_mode "on")
                             (and (= clojure_mode "auto") is-clojure-file))
        use-raw-mode (or (= clojure_mode "off")
                         (and (= clojure_mode "auto") (not is-clojure-file)))]

    (if use-clojure-mode
      ;; Use Clojure-aware file reading
      (try
        (let [collapsed-view (form-edit-core/generate-collapsed-file-view path expand_symbols)]
          {:mode :clojure
           :content collapsed-view
           :path path
           :clojure-mode clojure_mode
           :expand-symbols expand_symbols
           :error false})
        (catch Exception e
          ;; Don't use :mode :clojure for errors
          {:error true
           :message (.getMessage e)}))

      ;; Use raw file reading
      (let [result (read-file-core/read-file path line_offset limit-val :max-line-length max-line-length)]
        (if (:error result)
          ;; Don't use :mode for errors
          {:error true
           :message (:error result)}
          (assoc result :mode :raw))))))

;; Formatter helper functions for different content types

(defn format-clojure-view
  "Formats a Clojure file view with XML tags and usage advice.
   
   Parameters:
   - content: The collapsed Clojure file content
   - path: The path to the file
   - clojure-mode: The Clojure mode setting ('auto', 'on', or 'off')
   - expand-symbols: List of symbols shown in expanded form
   
   Returns:
   - A formatted string with the collapsed view wrapped in XML tags"
  [content path clojure-mode expand-symbols]
  (let [expand-symbols-str (if (empty? expand-symbols)
                             "[]"
                             (pr-str expand-symbols))
        xml-open-tag (str "<collapsed-clojure-view clojure_mode=\"" clojure-mode
                          "\" file_path=\"" path "\" expand_symbols=" expand-symbols-str ">\n")
        xml-close-tag "\n</collapsed-clojure-view>"
        advice (str "\n<!-- This is a COLLAPSED VIEW"
                    "\nTo see specific functions in full: {\"path\": \""
                    path
                    "\", \"expand_symbols\": [\"function-name\"]}\n"
                    "     For raw text view: {\"path\": \"" path
                    "\", \"clojure_mode\": \"off\"} -->")]
    
    [(str
      ;;xml-open-tag
      content
      ;;xml-close-tag
      )
     #_advice]))

(defn format-raw-file
  "Formats raw file content with XML tags and metadata.
   
   Parameters:
   - result: The raw file reading result map
   - max-lines: Maximum number of lines limit (for header info)
   
   Returns:
   - A formatted string with the file content wrapped in XML tags"
  [result max-lines]
  (let [{:keys [content path size line-count offset truncated? line-lengths-truncated?]} result
        file (io/file path)
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
                   (str ">\n"))]
    ;; TODO clean this up and maybe add line numbers
    [content #_(str header content "\n</file-content>")]))

(defmethod tool-system/format-results :unified-read-file [{:keys [max-lines]} result]
  (if (:error result)
    ;; If there's an error, return it directly without XML wrapping
    {:result [(or (:message result) "Unknown error")]
     :error true}
    ;; Format based on the mode
    (case (:mode result)
      :clojure
      {:result (format-clojure-view (:content result)
                                    (:path result)
                                    (:clojure-mode result)
                                    (:expand-symbols result))
       :error false}

      :raw
      {:result (format-raw-file result max-lines)
       :error false}

      ;; Default case (should not happen)
      {:result ["Unknown result mode"]
       :error true})))

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
