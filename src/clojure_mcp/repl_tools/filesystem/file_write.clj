(ns clojure-mcp.repl-tools.filesystem.file-write
  (:require
   [clojure.java.io :as io]
   [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as tle]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.data.json :as json]
   [rewrite-clj.zip :as z]))

(defn is-clojure-file?
  "Check if a file is a Clojure-related file based on its extension"
  [file-path]
  (let [lower-path (clojure.string/lower-case file-path)]
    (some #(clojure.string/ends-with? lower-path %) [".clj" ".cljs" ".cljc" ".edn"])))

(defn write-clojure-file
  "Write content to a Clojure file, with linting, formatting, and diffing.
   
   Arguments:
   - file-path: Absolute path to the file to write
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
   
   Arguments:
   - file-path: Absolute path to the file to write
   - content: Content to write to the file
   
   Returns:
   - A map with :error, :type, :file-path, and :diff keys"
  [file-path content]
  (try
    (let [file (io/file file-path)
          file-exists? (.exists file)
          old-content (if file-exists? (slurp file) "")
          diff (if (= old-content content)
                 ""
                 (clojure-mcp.repl-tools.utils/generate-diff-via-shell old-content content 3))]

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
   
   Arguments:
   - file-path: Absolute path to the file to write
   - content: Content to write to the file
   
   Returns:
   - A map with :error, :type, :file-path, and :diff keys"
  [file-path content]
  (if (is-clojure-file? file-path)
    (write-clojure-file file-path content)
    (write-text-file file-path content)))

(defn file-write-tool
  "Returns a tool map for writing files to the filesystem.
   
   Arguments:
   - service-atom: Service atom (required for tool registration but not used in this implementation)
   
   Returns a map with :name, :description, :schema and :tool-fn keys"
  [nrepl-client-atom]
  {:name "file_write"
   :description
   (str "Write a file to the local filesystem. Overwrites the existing file if there is one. "
        "For Clojure files (.clj, .cljs, .cljc, .edn), content will be linted and formatted. "
        "Other file types will be written directly without processing.\n\n"
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
              (try
                (let [file-path (get arg-map "file_path")
                      content (get arg-map "content")
                      validated-path (utils/validate-path-with-client file-path @nrepl-client-atom)
                      result (write-file file-path content)]
                  (if (:error result)
                    (clj-result-k [(:message result)] true)
                    (let [file-type (if (is-clojure-file? file-path) "Clojure" "Text")
                          response (str file-type " file " (:type result) "d: " (:file-path result))]
                      (if (seq (:diff result))
                        (clj-result-k [(str response "\nChanges:\n" (:diff result))] false)
                        (clj-result-k [response] false)))))
                (catch Exception e
                  (clj-result-k [(str "Error: " (.getMessage e))] true))))})
