(ns clojure-mcp.resources
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json])
  (:import [io.modelcontextprotocol.spec McpSchema$Resource McpSchema$ReadResourceResult]))

(defn file-as-bytes
  "Convert a file into a byte array"
  [file]
  (with-open [xin (io/input-stream file)
              xout (java.io.ByteArrayOutputStream.)]
    (io/copy xin xout)
    (.toByteArray xout)))

(defn string-as-bytes
  "Convert a string into a byte array using UTF-8 encoding"
  [s]
  (.getBytes s "UTF-8"))

(defn create-file-resource
  "Creates a resource specification for serving a file"
  [url name description mime-type file-path]
  {:url url
   :name name
   :description description
   :mime-type mime-type
   :resource-fn (fn [_ _ clj-result-k]
                  (try
                    (let [file (io/file file-path)]
                      (if (.exists file)
                        (clj-result-k (file-as-bytes file))
                        (clj-result-k (string-as-bytes (str "Error: File not found: " file-path)))))
                    (catch Exception e
                      (clj-result-k (string-as-bytes (str "Error: " (.getMessage e)))))))})

(defn create-string-resource
  "Creates a resource specification for serving a string"
  [url name description mime-type content]
  {:url url
   :name name
   :description description
   :mime-type mime-type
   :resource-fn (fn [_ _ clj-result-k]
                  (clj-result-k (string-as-bytes content)))})

;; Define the PROJECT_SUMMARY resource that serves the content of PROJECT_SUMMARY.md
(def project-summary-resource
  (create-file-resource
   "custom://project-summary"
   "Project Summary"
   "The Clojure MCP project summary document"
   "text/markdown"
   "/Users/bruce/workspace/llempty/clojure-mcp/PROJECT_SUMMARY.md"))

;; Function to get all defined resources for registration
 ;; Define a README resource
(def readme-resource
  (create-file-resource
   "custom://readme"
   "README"
   "The Clojure MCP README file"
   "text/markdown"
   "/Users/bruce/workspace/llempty/clojure-mcp/README.md"))

;; Define a BIG_IDEAS resource
(def big-ideas-resource
  (create-file-resource
   "custom://big-ideas"
   "Big Ideas"
   "The Clojure MCP big ideas document"
   "text/markdown"
   "/Users/bruce/workspace/llempty/clojure-mcp/BIG_IDEAS.md"))

;; Define a dynamic resource for REPL session information
;; Define a dynamic resource for REPL session information

;; Define a dynamic resource for REPL session information
(def repl-info-resource
  {:url "custom://repl-info"
   :name "REPL Info"
   :description "Information about the current REPL session"
   :mime-type "application/json"
   :resource-fn (fn [_ _ clj-result-k]
                  (let [info {:current-ns (str *ns*)
                              :clojure-version (clojure-version)
                              :current-directory (System/getProperty "user.dir")
                              :loaded-libs (into [] (map str (loaded-libs)))
                              :java-version (System/getProperty "java.version")
                              :timestamp (str (java.util.Date.))}
                        json-str (json/write-str info)]
                    (clj-result-k (string-as-bytes json-str))))})

(defn get-all-resources
  "Returns a list of all defined resources for registration with the MCP server"
  []
  [project-summary-resource
   readme-resource
   big-ideas-resource
   repl-info-resource])
