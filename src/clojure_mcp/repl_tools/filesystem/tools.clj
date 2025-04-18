(ns clojure-mcp.repl-tools.filesystem.tools
  "MCP tools for filesystem operations.
   Provides tools for listing, reading, and searching files and directories."
  (:require [clojure-mcp.repl-tools.filesystem.core :as fs]
            [clojure-mcp.repl-tools.filesystem.grep :as grep]
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
  []
  {:name "fs_list_directory"
   :description "Lists all files and directories at the specified path. 
Returns a formatted directory listing with files and subdirectories clearly labeled."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string}}
                            :required [:path]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path")
                    result (fs/list-directory path)
                    formatted (format-directory-listing result)
                    error? (boolean (:error result))]
                (clj-result-k [formatted] error?)))})

(defn create-fs-read-file-tool
  "Creates a tool that reads the contents of a file"
  [{:keys [max-lines max-line-length]
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
                    result (fs/read-file-contents path
                                                  :max-lines limit
                                                  :offset offset
                                                  :max-line-length max-line-length)
                    output (if (:error result)
                             (:error result)
                             (let [file (io/file path)
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
                (clj-result-k [output] error?)))})

;; Removed create-fs-file-info-tool function

;; Removed create-fs-search-files-tool function (replaced by create-glob-files-tool)

(defn create-directory-tree-tool
  "Creates a tool that displays a recursive tree view of directory structure"
  []
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
                    result (try
                             (fs/directory-tree path :max-depth max-depth)
                             (catch Exception e
                               {:error (.getMessage e)}))
                    error? (map? result)
                    output (if error?
                             (:error result)
                             result)]
                (clj-result-k [output] error?)))})

(defn create-glob-files-tool
  "Creates a tool for fast file pattern matching using glob patterns"
  []
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
                    result (try
                             (fs/glob-files path pattern :max-results max-results)
                             (catch Exception e
                               {:error (.getMessage e)}))
                    error? (boolean (:error result))
                    output (if error?
                             (:error result)
                             (json/write-str
                              (select-keys result [:filenames :numFiles :durationMs :truncated])
                              :escape-slash false))]
                (clj-result-k [output] error?)))})

;; Removed create-directory-tree-json-tool function

(defn create-grep-tool
  "Creates a tool for searching file contents using regular expressions"
  []
  {:name "fs_grep"
   :description "Fast content search tool that works with any codebase size.\nSearches file contents using regular expressions.\nSupports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.).\nFilter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\").\nReturns matching file paths sorted by modification time.\nUse this tool when you need to find files containing specific patterns."
   :schema (json/write-str {:type :object
                            :properties {:path {:type :string
                                                :description "The directory to search in. Defaults to the current working directory."}
                                         :pattern {:type :string
                                                   :description "The regular expression pattern to search for in file contents"}
                                         :include {:type :string
                                                  :description "File pattern to include in the search (e.g. \"*.clj\", \"*.{clj,cljs}\")"}
                                         :max_results {:type :integer
                                                      :description "Maximum number of results to return (default: 1000)"}}
                            :required [:pattern]})
   :tool-fn (fn [_ params clj-result-k]
              (let [path (get params "path" ".")
                    pattern (get params "pattern")
                    include (get params "include")
                    max-results (get params "max_results" 1000)
                    result (try
                             (grep/grep-files path pattern :include include :max-results max-results)
                             (catch Exception e
                               {:error (.getMessage e)}))
                    error? (boolean (:error result))
                    output (if error?
                             (:error result)
                             (json/write-str
                              {:filenames (:filenames result)
                               :numFiles (:numFiles result)
                               :durationMs (:durationMs result)}
                              :escape-slash false))]                
                (clj-result-k [output] error?)))})  

(defn get-all-filesystem-tools
  "Returns a collection of all filesystem tools.
   
   Arguments:
   - nrepl-client-atom: Atom containing the nREPL service connection
   
   Returns a vector of tool instances ready for MCP registration"
  [nrepl-client-atom]
  [(create-fs-list-directory-tool)
   (create-fs-read-file-tool {:max-lines 2000 :max-line-length 1000})
   (create-directory-tree-tool)
   (create-glob-files-tool)
   (create-grep-tool)])

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
