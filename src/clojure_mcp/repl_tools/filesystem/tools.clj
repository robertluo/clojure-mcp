(ns clojure-mcp.repl-tools.filesystem.tools
  "MCP tools for filesystem operations.
   Provides tools for listing, reading, and searching files and directories."
  (:require [clojure-mcp.repl-tools.filesystem.core :as fs]
            [clojure-mcp.repl-tools.filesystem.grep :as grep]
            [clojure-mcp.repl-tools.filesystem.file-write :as fw]
            [clojure-mcp.repl-tools.utils :as utils]
            [clojure.data.json :as json]
            [clojure-mcp.nrepl :as mcp-nrepl]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn format-directory-listing
  "Format directory listing into a readable string representation.
   
   Arguments:
   - result: Result map from list-directory function
   
   Returns a formatted string with directory content listing"
  [result]
  (if (:error result)
    (:error result)
    (let [{:keys [files directories full-path]} result]
      (with-out-str
        (println "Directory:" full-path)
        (println "===============================")
        (when (seq directories)
          (println "\nDirectories:")
          (doseq [dir (sort directories)]
            (println "[DIR]" dir)))
        (when (seq files)
          (println "\nFiles:")
          (doseq [file (sort files)]
            (println "[FILE]" file)))
        (when (and (empty? directories) (empty? files))
          (println "Directory is empty"))))))

;; Removed format-search-results function (replaced by glob-files)

(defn create-fs-list-directory-tool
  "Creates a tool that lists files and directories at a specified path"
  [nrepl-client-atom]
  {:name "fs_list_directory"
   :description "Lists all files and directories at the specified path. 
Returns a formatted directory listing with files and subdirectories clearly labeled."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string}}
                            :required [:path]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path")
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client path nrepl-client)
                        result (fs/list-directory validated-path)
                        formatted (format-directory-listing result)
                        error? (boolean (:error result))]
                    (clj-result-k [formatted] error?))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e))] true)))))})

(defn create-fs-read-file-tool
  "Creates a tool that reads the contents of a file"
  [nrepl-client-atom {:keys [max-lines max-line-length]
                      :or {max-lines 2000 max-line-length 1000}}]
  {:name "fs_read_file"
   :description (str "Reads a file from the local filesystem. The file_path parameter must be an absolute path, not a relative path. "
                     "By default, it reads up to " max-lines " lines starting from the beginning of the file. "
                     "You can optionally specify a line offset and limit (especially handy for long files), but it's recommended "
                     "to read the whole file by not providing these parameters. Any lines longer than " max-line-length
                     " characters will be truncated.")
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string}
                                         :offset {:type :integer
                                                  :description "Line number to start reading from (0-indexed)"}
                                         :limit {:type :integer
                                                 :description "Maximum number of lines to read"}}
                            :required [:path]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path")
                    offset (get params "offset" 0)
                    limit (get params "limit" max-lines)
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client path nrepl-client)
                        result (fs/read-file-contents validated-path
                                                      :max-lines limit
                                                      :offset offset
                                                      :max-line-length max-line-length)
                        output (if (:error result)
                                 (:error result)
                                 (let [file (io/file validated-path)
                                       size (.length file)
                                       truncated (boolean (:truncated? result))
                                       content (:content result)
                                       line-count (:line-count result)
                                       line-lengths-truncated (:line-lengths-truncated? result)
                                       truncation-reason (:truncated-by result)]
                                   (str "<file-content path=\"" (:path result) "\" "
                                        "size=\"" size "\" "
                                        "line-count=\"" line-count "\" "
                                        "offset=\"" offset "\" "
                                        "limit=\"" limit "\" "
                                        "truncated=\"" truncated "\" "
                                        (when truncation-reason
                                          (str "truncated-by=\"" truncation-reason "\" "))
                                        (when line-lengths-truncated
                                          "line-lengths-truncated=\"true\" ")
                                        ">\n"
                                        content
                                        "\n</file-content>")))
                        error? (boolean (:error result))]
                    (clj-result-k [output] error?))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e))] true)))))})

;; Removed create-fs-file-info-tool function

;; Removed create-fs-search-files-tool function (replaced by create-glob-files-tool)

(defn create-directory-tree-tool
  "Creates a tool that displays a recursive tree view of directory structure"
  [nrepl-client-atom]
  {:name "directory_tree"
   :description "Returns a recursive tree view of files and directories starting from the specified path.
Formats the output as an indented tree structure showing the hierarchy of files and folders."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string}
                                         :max_depth {:type :integer
                                                     :description "Maximum depth to traverse (optional)"}}
                            :required [:path]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path")
                    max-depth (get params "max_depth")
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client path nrepl-client)
                        result (fs/directory-tree validated-path :max-depth max-depth)]
                    (clj-result-k [result] false))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e))] true)))))})

(defn create-glob-files-tool
  "Creates a tool for fast file pattern matching using glob patterns"
  [nrepl-client-atom]
  {:name "glob_files"
   :description "Fast file pattern matching tool that works with any codebase size.
Supports glob patterns like \"**/*.clj\" or \"src/**/*.cljs\".
Returns matching file paths sorted by modification time (most recent first).
Use this tool when you need to find files by name patterns."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string
                                                :description "Root directory to start the search from"}
                                         :pattern {:type :string
                                                   :description "Glob pattern (e.g. \"**/*.clj\", \"src/**/*.tsx\")"}
                                         :max_results {:type :integer
                                                       :description "Maximum number of results to return (default: 1000)"}}
                            :required [:path :pattern]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path")
                    pattern (get params "pattern")
                    max-results (get params "max_results" 1000)
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client path nrepl-client)
                        result (fs/glob-files validated-path pattern :max-results max-results)
                        output (json/write-str
                                (select-keys result [:filenames :numFiles :durationMs :truncated])
                                :escape-slash false)]
                    (clj-result-k [output] false))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e))] true)))))})

;; Removed create-directory-tree-json-tool function

(defn create-grep-tool
  "Creates a tool for searching file contents using regular expressions"
  [nrepl-client-atom]
  {:name "fs_grep"
   :description "Fast content search tool that works with any codebase size.
Searches file contents using regular expressions.
Supports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.).
Filter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\").
Returns matching file paths sorted by modification time.
Use this tool when you need to find files containing specific patterns."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string
                                                :description "The directory to search in"}
                                         :pattern {:type :string
                                                   :description "The regular expression pattern to search for in file contents"}
                                         :include {:type :string
                                                   :description "File pattern to include in the search (e.g. \"*.clj\", \"*.{clj,cljs}\")"}
                                         :max_results {:type :integer
                                                       :description "Maximum number of results to return (default: 1000)"}}
                            :required [:path :pattern]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path" ".")
                    pattern (get params "pattern")
                    include (get params "include")
                    max-results (get params "max_results" 1000)
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client path nrepl-client)
                        ;; No pattern validation - let the underlying tools handle errors

                        result (grep/grep-files validated-path pattern :include include :max-results max-results)

                        ;; Check if there's an error in the result
                        _ (when (:error result)
                            (throw (ex-info (:error result) {:path validated-path :pattern pattern})))

                        ;; Format the output properly, handling the case where no files were found
                        output (json/write-str
                                (if (nil? (:filenames result))
                                  {:filenames [] :numFiles 0 :durationMs (:durationMs result)
                                   :message "No matching files found. Try broadening your search pattern or path."}
                                  {:filenames (:filenames result)
                                   :numFiles (:numFiles result)
                                   :durationMs (:durationMs result)})
                                :escape-slash false)]
                    (clj-result-k [output] false))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e)
                                        (when-let [data (ex-data e)]
                                          (str "\nDetails: " (pr-str data))))] true)))))})

(defn create-file-write-tool
  "Returns a tool map for writing files to the filesystem.
   
   Arguments:
   - nrepl-client-atom: Atom containing the nREPL service connection
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [nrepl-client-atom]
  {:name "file_write"
   :description
   (str "Write a file to the local filesystem. Overwrites the existing file if there is one. "
        "The content will be linted and formatted according to Clojure standards before writing.\n\n"
        "Returns information about whether the file was created or updated, along with a diff "
        "showing the changes made.\n\n"
        "Before using this tool:\n"
        "1. Use the read_file tool to understand the file's contents and context\n"
        "2. Directory Verification (only applicable when creating new files):\n"
        "   - Use the list_directory tool to verify the parent directory exists and is the correct location\n\n"
        "# Example:\n"
        "# file_write(\n"
        "#   file_path: \"/absolute/path/to/file.clj\",\n"
        "#   content: \"(ns my.namespace)\\n\\n(defn my-function [x]\\n  (* x 2))\"\n"
        "# )")
   :schema
   (json/write-str
    {:type :object
     :properties
     {:file_path {:type :string
                  :description "The absolute path to the file to write (must be absolute, not relative)"}
      :content {:type :string
                :description "The content to write to the file"}}
     :required [:file_path :content]})
   :tool-fn (fn [_ arg-map clj-result-k]
              (let [file-path (get arg-map "file_path")
                    content (get arg-map "content")
                    nrepl-client @nrepl-client-atom]
                (try
                  (let [validated-path (utils/validate-path-with-client file-path nrepl-client)
                        result (fw/write-file validated-path content)]
                    (if (:error result)
                      (clj-result-k [(:message result)] true)
                      (let [response (str "File " (:type result) "d: " (:file-path result))]
                        (if (seq (:diff result))
                          (clj-result-k [(str response "\nChanges:\n" (:diff result))] false)
                          (clj-result-k [response] false)))))
                  (catch Exception e
                    (clj-result-k [(str "Error: " (.getMessage e))] true)))))})

(defn get-all-filesystem-tools
  "Returns a collection of all filesystem tools.
   
   Arguments:
   - nrepl-client-atom: Atom containing the nREPL service connection
   
   Returns a vector of tool instances ready for MCP registration
   
   Throws an exception if the required nREPL client settings are missing."
  [nrepl-client-atom]
  (let [nrepl-client @nrepl-client-atom]
    ;; Validate that required settings are present
    (when-not (:clojure-mcp.core/nrepl-user-dir nrepl-client)
      (throw (ex-info "Missing ::nrepl-user-dir in nREPL client"
                      {:client-keys (keys nrepl-client)})))

    (when-not (:clojure-mcp.core/allowed-directories nrepl-client)
      (throw (ex-info "Missing ::allowed-directories in nREPL client"
                      {:client-keys (keys nrepl-client)})))

    [(create-fs-list-directory-tool nrepl-client-atom)
     #_(create-fs-read-file-tool nrepl-client-atom {:max-lines 2000 :max-line-length 1000})
     #_(create-directory-tree-tool nrepl-client-atom) ;; Commented out - replaced by the new tool-system implementation
     #_(create-glob-files-tool nrepl-client-atom) ;; Commented out - replaced by the new tool-system implementation
     #_(create-grep-tool nrepl-client-atom) ;; Commented out - replaced by the new tool-system implementation
     (create-file-write-tool nrepl-client-atom)]))

(comment
  ;; === Examples of using the filesystem tools ===

  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Test helper function
  (defn make-test-tool [{:keys [tool-fn]}]
    (fn [arg-map]
      (let [prom (promise)]
        (tool-fn nil arg-map
                 (fn [res error]
                   (deliver prom {:res res :error error})))
        @prom)))

  ;; Test directory listing
  (def list-dir-tester (make-test-tool (create-fs-list-directory-tool)))
  (list-dir-tester {"path" "src/clojure_mcp"})

  ;; Test file reading
  (def read-file-tester (make-test-tool (create-fs-read-file-tool {:max-lines 2000 :max-line-length 1000})))
  (read-file-tester {"path" "deps.edn"})

  ;; Test directory tree
  (def directory-tree-tester (make-test-tool (create-directory-tree-tool)))
  (directory-tree-tester {"path" "src" "max_depth" 2})

  ;; Test glob pattern matching
  (def glob-files-tester (make-test-tool (create-glob-files-tool)))
  (glob-files-tester {"path" "src" "pattern" "**/*.clj"})

  ;; Test content grep
  (def grep-tester (make-test-tool (create-grep-tool)))
  (grep-tester {"path" "src" "pattern" "defn" "include" "*.clj"})

  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom))
