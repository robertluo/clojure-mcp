(ns clojure-mcp.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure-mcp.nrepl :as mcp-nrepl]
            [clojure-mcp.repl-tools.project.inspect :as inspect])
  (:import [io.modelcontextprotocol.spec McpSchema$Resource McpSchema$ReadResourceResult]))

(defn read-file [full-path]
  (let [file (io/file full-path)]
    (if (.exists file)
      (try
        (slurp file)
        (catch Exception e
          (throw (ex-info (str "reading file- " full-path
                               "\nException- " (.getMessage e))
                          {:path full-path}
                          e))))
      (throw (ex-info (str "File not found- " full-path
                           "\nAbsolute path- " (.getAbsolutePath file))
                      {:path full-path})))))

(defn create-file-resource
  "Creates a resource specification for serving a file.
   Takes a full file path resolved with the correct working directory."
  [url name description mime-type full-path]
  {:url url
   :name name
   :description description
   :mime-type mime-type
   :resource-fn
   (fn [_ _ clj-result-k]
     (try
       (let [result (read-file full-path)]
         (clj-result-k [result]))
       (catch Exception e
         (clj-result-k [(str "Error in resource function: "
                             (ex-message e)
                             "\nFor file: " full-path)]))))})

(defn create-string-resource
  "Creates a resource specification for serving a string.
   Accepts nrepl-client-atom for consistency with create-file-resource, but doesn't use it."
  [url name description mime-type contents & [nrepl-client-atom]]
  {:url url
   :name name
   :description description
   :mime-type mime-type
   :resource-fn (fn [_ _ clj-result-k]
                  (clj-result-k contents))})

;; This function is no longer needed as we handle resource creation directly in get-all-resources

(defn get-all-resources
  "Returns a list of all defined resources for registration with the MCP server.
   Now gets the working directory once and uses it for all file resources."
  [nrepl-client-atom]
  (let [nrepl-client @nrepl-client-atom
        working-dir-raw (mcp-nrepl/tool-eval-code nrepl-client "(System/getProperty \"user.dir\")")
        ;; Parse the working directory using edn/read-string to handle quotes and escape sequences
        working-dir (edn/read-string working-dir-raw)]

    ;; List of all resources
    [(create-file-resource
      "custom://project-summary"
      "Project Summary"
      "A Clojure project summary document for the current"
      "text/markdown"
      (str working-dir "/PROJECT_SUMMARY.md"))

     (create-file-resource
      "custom://readme"
      "README"
      "A README document for the current project"
      "text/markdown"
      (str working-dir "/README.md"))

     (create-file-resource
      "custom://claude"
      "Claude Instructions"
      "The Claude instructions document for the current project"
      "text/markdown"
      (str working-dir "/CLAUDE.md"))

     ;; Add dynamic project info resource that uses the inspect-project-code function
     (let [project-code (str (inspect/inspect-project-code))
           project-data (mcp-nrepl/tool-eval-code nrepl-client project-code)
           ;; Also need to parse this data with edn/read-string
           project-markdown (inspect/format-project-info project-data)]
       (create-string-resource
        "custom://project-info"
        "Project Info"
        "Dynamic information about the current Clojure project structure, attached REPL environment and dependencies"
        "text/markdown"
        [project-markdown]))]))
