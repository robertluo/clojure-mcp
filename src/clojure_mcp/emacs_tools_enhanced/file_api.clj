(ns clojure-mcp.emacs-tools-enhanced.file-api
  "File-centric API for Emacs interaction.
   
   This namespace provides a comprehensive set of functions for working with files
   directly in Emacs, abstracting away buffer management and focusing on file operations.
   All functions operate directly on files and handle opening, editing, and saving
   automatically."
  (:require [clojure-mcp.emacs-tools-enhanced.file.core :as core]
            [clojure-mcp.emacs-tools-enhanced.file.operations.read :as read-ops]
            [clojure-mcp.emacs-tools-enhanced.file.operations.write :as write-ops]
            [clojure-mcp.emacs-tools-enhanced.file.operations.edit :as edit-ops]
            [clojure-mcp.emacs-tools-enhanced.file.operations.manage :as manage-ops]
            [clojure-mcp.emacs-tools-enhanced.file.operations.visual :as visual-ops]
            [clojure.data.json :as json]))

(defn make-mcp-tool
  "Helper function to create an MCP tool map with common structure.
   
   Arguments:
   - name: Name of the tool (string)
   - description: Tool description (string)
   - schema: JSON schema for the tool parameters (can be string or map)
   - impl-fn: Implementation function that takes arguments map and returns a function
              that accepts a callback and produces the result
   
   Returns a complete tool map ready to be added to an MCP server."
  [name description schema impl-fn]
  {:name name
   :description description
   :schema (if (string? schema) schema (json/write-str schema))
   :tool-fn (fn [_ arg-map clj-result-k]
              (try
                (let [result (impl-fn arg-map)]
                  (if (:success result)
                    (clj-result-k (list* (:content result)) false)
                    (clj-result-k (list* (:message result)) true)
                    ))
                (catch Exception e
                  (clj-result-k [(str "Error: " (.getMessage e))] true))))})

(defn emacs-flash-file-tool []
  (make-mcp-tool
   "emacs_flash_file"
   "Flashes the file in Emacs to draw attention to it.
Given a `path` to a file, this will bring focus to the file and flash its contents."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (visual-ops/flash-file (get arg-map "path")))))

;; -------------------------------------------------------------------------
;; Public API
;; -------------------------------------------------------------------------

#_(defn file-api
    "Returns a map of all file-centric functions for manipulating files in Emacs."
    []
    {:read-file       read-ops/read-file
     :write-file      write-ops/write-file
     :edit-file       edit-ops/edit-file
     :append          write-ops/append-to-file
     :with-file       core/with-file
     :emacs-eval      core/emacs-eval
     :file-exists?    read-ops/file-exists?
     :save-file       write-ops/save-file
     :flash-file      visual-ops/flash-file
     :delete-file     manage-ops/delete-file
     :rename-file     manage-ops/move-file
     :copy-file       manage-ops/copy-file
     :find-files      manage-ops/find-files
     :with-files      manage-ops/with-files})

;; Export the API for easy access
#_(def files (file-api))
