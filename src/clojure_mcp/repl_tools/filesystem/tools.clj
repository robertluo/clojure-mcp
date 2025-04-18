(ns clojure-mcp.repl-tools.filesystem.tools
  "MCP tools for filesystem operations.
   Provides tools for listing, reading, and searching files and directories."
  (:require [clojure-mcp.repl-tools.filesystem.core :as fs]
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

(defn format-search-results
  "Format file search results into a readable string.
   
   Arguments:
   - result: Result map from search-files function
   
   Returns a formatted string with search results"
  [result]
  (if (:error result)
    (:error result)
    (let [{:keys [matches count search-root pattern]} result]
      (with-out-str
        (println "Search Results:")
        (println "===============================")
        (println "Pattern:" pattern)
        (println "Search Root:" search-root)
        (println "Found:" count "matches")
        (if (seq matches)
          (do
            (println "\nMatching Files:")
            (doseq [match matches]
              (println "-" match)))
          (println "\nNo matching files found"))))))

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

(defn create-fs-search-files-tool
  "Creates a tool that searches for files matching a pattern"
  []
  {:name "fs_search_files"
   :description "Searches for files matching a pattern in a directory and its subdirectories.
Results include full paths to matching files, sorted alphabetically."
   :schema (json/write-str {:type :object
                            :properties {:directory {:type :string}
                                         :pattern {:type :string}
                                         :exclude-patterns {:type :array
                                                            :items {:type :string}}}
                            :required [:directory :pattern]})
   :tool-fn (fn [_ params clj-result-k]
              (let [directory (get params "directory")
                    pattern (get params "pattern")
                    exclude-patterns (get params "exclude-patterns" [])
                    result (fs/search-files directory pattern :exclude-patterns exclude-patterns)
                    formatted (format-search-results result)
                    error? (boolean (:error result))]
                (clj-result-k [formatted] error?)))})

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

;; Removed create-directory-tree-json-tool function

(defn get-all-filesystem-tools
  "Returns a collection of all filesystem tools.
   
   Arguments:
   - nrepl-client-atom: Atom containing the nREPL service connection
   
   Returns a vector of tool instances ready for MCP registration"
  [nrepl-client-atom]
  [(create-fs-list-directory-tool)
   (create-fs-read-file-tool {:max-lines 2000 :max-line-length 1000})
   (create-fs-search-files-tool)
   (create-directory-tree-tool)])

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

  ;; Test file search
  (def search-files-tester (make-test-tool (create-fs-search-files-tool)))
  (search-files-tester {"directory" "src" "pattern" "eval"})

  ;; Test directory tree
  (def directory-tree-tester (make-test-tool (create-directory-tree-tool)))
  (directory-tree-tester {"path" "src" "max_depth" 2})

  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom))
