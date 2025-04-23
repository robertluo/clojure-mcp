(ns clojure-mcp.tools.unified-read-file.core
  "Core implementation for the unified-read-file tool.
   This tool combines clojure_read_file and fs_read_file functionality
   into a single smart tool that handles different file types appropriately."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure-mcp.tools.read-file.core :as read-file-core]
   [clojure-mcp.tools.form-edit.core :as form-edit-core]))

(defn clojure-file?
  "Determines if a file is a Clojure source file based on its extension.
   Only .clj, .cljc, and .cljs files are considered Clojure files.
   EDN files are treated as regular files for raw content display."
  [file-path]
  (when file-path
    (let [extension (last (str/split file-path #"\."))]
      (contains? #{"clj" "cljc" "cljs"} extension))))

(defn read-unified-file
  "Reads a file using either Clojure-aware or raw content reading based on parameters.
   
   Parameters:
   - path: The validated and normalized path to the file
   - clojure-mode: Whether to force clojure mode ('auto', 'on', or 'off')
   - expand-symbols: List of symbols to show in expanded form (for Clojure mode)
   - line-offset: Line number to start reading from (for raw mode)
   - limit: Maximum number of lines to read (for raw mode)
   - max-line-length: Maximum length per line before truncation (for raw mode)
   
   Returns a map with either:
   - For Clojure mode: {:result [...] :error false/true}
   - For raw mode: file content details as in read-file-core/read-file"
  [path clojure-mode expand-symbols line-offset limit & {:keys [max-line-length] :or {max-line-length 1000}}]
  (let [is-clojure-file (clojure-file? path)
        use-clojure-mode (or (= clojure-mode "on")
                             (and (= clojure-mode "auto") is-clojure-file))
        use-raw-mode (or (= clojure-mode "off")
                         (and (= clojure-mode "auto") (not is-clojure-file)))]

    (if use-clojure-mode
      ;; Use Clojure-aware file reading
      (try
        (let [collapsed-view (form-edit-core/generate-collapsed-file-view path expand-symbols)
              expand-symbols-str (if (empty? expand-symbols)
                                   "[]"
                                   (pr-str expand-symbols)
                                   #_(str "[\"" (clojure.string/join "\", \"" expand-symbols) "\"]"))
              xml-open-tag (str "<collapsed-clojure-view clojure_mode=\"" clojure-mode
                                "\" file_path=\"" path "\" expand_symbols=" expand-symbols-str ">\n")
              xml-close-tag "\n</collapsed-clojure-view>"
              simple-filename (last (clojure.string/split path #"/"))
              advice (str "\n<!-- This is a COLLAPSED VIEW you can expand this view by providing a set of function names to expand."
                          "\nYou can also read the whole file by turning clojure_mode off"
                          "\nTo see specific functions in full: {\"path\": \""path
                          "\", \"expand_symbols\": [\"function-name\"]}\n"
                          "     For raw text view: {\"path\": \"" path
                          "\", \"clojure_mode\": \"off\"} -->")]
          {:result [(str xml-open-tag collapsed-view advice xml-close-tag)]
           :error false})
        (catch Exception e
          {:result [(str "Error generating Clojure file view: " (.getMessage e))]
           :error true}))

      ;; Use raw file reading
      (let [result (read-file-core/read-file path line-offset limit :max-line-length max-line-length)]
        (if (:error result)
          {:result [(:error result)]
           :error true}
          result)))))
