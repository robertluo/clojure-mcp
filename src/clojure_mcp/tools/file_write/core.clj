(ns clojure-mcp.tools.file-write.core
  "Core implementation for the file-write tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]
   [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as tle]
   [clojure-mcp.repl-tools.utils :as utils]
   [rewrite-clj.zip :as z]))

(defn is-clojure-file?
  "Check if a file is a Clojure-related file based on its extension.
   
   Parameters:
   - file-path: Path to the file to check
   
   Returns true if the file has a Clojure-related extension (.clj, .cljs, .cljc, .edn),
   false otherwise."
  [file-path]
  (let [lower-path (clojure.string/lower-case file-path)]
    (some #(clojure.string/ends-with? lower-path %) [".clj" ".cljs" ".cljc" ".edn"])))

(defn write-clojure-file
  "Write content to a Clojure file, with linting, formatting, and diffing.
   
   Parameters:
   - file-path: Validated path to the file to write
   - content: Content to write to the file
   
   Returns:
   - A map with :error, :type, :file-path, and :diff keys"
  [file-path content]
  (let [file (io/file file-path)
        file-exists? (.exists file)
        old-content (if file-exists? (slurp file) "")

        ;; Create a context map for the pipeline
        initial-ctx {::tle/file-path file-path
                     ::tle/source old-content
                     ::tle/new-source-code content
                     ::tle/old-content old-content
                     ::tle/file-exists? file-exists?}

        ;; Use thread-ctx to run the pipeline
        result (tle/thread-ctx
                initial-ctx
                tle/lint-code ;; Lint the content
                (fn [ctx] ;; Prepare for formatting
                  (assoc ctx ::tle/zloc (z/of-string (::tle/new-source-code ctx))))
                tle/format-source ;; Format the content
                tle/generate-diff ;; Generate diff between old and new content
                tle/determine-file-type ;; Determine if creating or updating
                ;; Use the save-file function instead of custom write logic
                (fn [ctx] ;; Modify context for save-file compatibility
                  (let [output-source (::tle/output-source ctx)
                        zloc (z/of-string output-source {:track-position? true})]
                    (assoc ctx ::tle/zloc zloc)))
                tle/save-file)] ;; Save the file and get offsets

    ;; Format the result for tool consumption
    (if (::tle/error result)
      {:error true
       :message (::tle/message result)}
      {:error false
       :type (::tle/type result)
       :file-path (::tle/file-path result)
       :diff (::tle/diff result)})))

(defn write-text-file
  "Write content to a non-Clojure text file, with diffing but no linting or formatting.
   
   Parameters:
   - file-path: Validated path to the file to write
   - content: Content to write to the file
   
   Returns:
   - A map with :error, :type, :file-path, and :diff keys"
  [file-path content]
  (try
    (let [file (io/file file-path)
          file-exists? (.exists file)
          old-content (if file-exists? (slurp file) "")
          ;; Only generate diff if the file already exists
          diff (if file-exists?
                 (if (= old-content content)
                   ""
                   (utils/generate-diff-via-shell old-content content 3))
                 "")]

      ;; Write the content directly
      (spit file content)

      {:error false
       :type (if file-exists? "update" "create")
       :file-path file-path
       :diff diff})
    (catch Exception e
      {:error true
       :message (str "Error writing file: " (.getMessage e))})))

(defn write-file
  "Write content to a file, detecting the file type and using appropriate processing.
   For Clojure files (.clj, .cljs, .cljc, .edn), applies linting and formatting.
   For other file types, writes directly with no processing.
   
   Parameters:
   - file-path: Validated path to the file to write
   - content: Content to write to the file
   
   Returns:
   - A map with :error, :type, :file-path, and :diff keys"
  [file-path content]
  (if (is-clojure-file? file-path)
    (write-clojure-file file-path content)
    (write-text-file file-path content)))