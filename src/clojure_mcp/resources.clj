(ns clojure-mcp.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure-mcp.nrepl :as mcp-nrepl]
            [clojure-mcp.repl-tools.project.inspect :as inspect])
  (:import [io.modelcontextprotocol.spec McpSchema$Resource McpSchema$ReadResourceResult]))

(defn create-file-resource
  "Creates a resource specification for serving a file.
   Takes a full file path resolved with the correct working directory."
  [url name description mime-type full-path]
  {:url url
   :name name
   :description description
   :mime-type mime-type
   :resource-fn (fn [_ _ clj-result-k]
                  (try
                    (let [file (io/file full-path)]
                      (if (.exists file)
                        (clj-result-k [(slurp file)])
                        (clj-result-k [(str "Error: File not found: " full-path)])))
                    (catch Exception e
                      (clj-result-k [(str "Error: " (.getMessage e))]))))})

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
        working-dir (mcp-nrepl/tool-eval-code nrepl-client "(System/getProperty \"user.dir\")")]

    ;; List of all resources
    [(create-file-resource
      "custom://project-summary"
      "Project Summary"
      "The Clojure MCP project summary document"
      "text/markdown"
      (str working-dir "/PROJECT_SUMMARY.md"))

     (create-file-resource
      "custom://readme"
      "README"
      "The Clojure MCP README file"
      "text/markdown"
      (str working-dir "/README.md"))

     (create-file-resource
      "custom://claude"
      "Claude Instructions"
      "The Clojure MCP Claude instructions document"
      "text/markdown"
      (str working-dir "/CLAUDE.md"))

     ;; Add dynamic project info resource that uses the inspect-project-code function
     (let [project-code (str (inspect/inspect-project-code))
           project-data (mcp-nrepl/tool-eval-code nrepl-client project-code)
           project-markdown (inspect/format-project-info project-data)]
       (create-string-resource
        "custom://project-info"
        "Project and REPL Info"
        "Dynamic information about the current Clojure project structure and dependencies"
        "text/markdown"
        [project-markdown]))]))
