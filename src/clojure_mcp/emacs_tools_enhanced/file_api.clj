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

;; -------------------------------------------------------------------------
;; Visual operations
;; -------------------------------------------------------------------------

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
;; Editing operations
;; -------------------------------------------------------------------------

(defn emacs-edit-file-tool []
  (make-mcp-tool
   "emacs_edit_file"
   "Makes multiple text replacements in a file with optional visual highlighting.
Takes an array of edits, where each edit is a object with `old_text` and `new_text` keys.
Applies edits sequentially in the order provided."
   {:type :object
    :properties {:path {:type :string}
                 :edits {:type :array
                         :items {:type :object
                                 :properties {:old_text {:type :string}
                                              :new_text {:type :string}}
                                 :required [:old_text :new_text]}}
                 :highlight_duration {:type :number}
                 :dry_run {:type :boolean}}
    :required [:path :edits]}
   (fn [arg-map]
     (let [edits (map (fn [edit] {:old-text (get edit "old_text")
                                  :new-text (get edit "new_text")})
                      (get arg-map "edits"))
           highlight-duration (or (get arg-map "highlight_duration") 2.0)
           dry-run (or (get arg-map "dry_run") false)]
       (edit-ops/edit-file (get arg-map "path") 
                           edits 
                           :highlight-duration highlight-duration 
                           :dry-run dry-run)))))

;; -------------------------------------------------------------------------
;; Writing operations
;; -------------------------------------------------------------------------

(defn emacs-write-file-tool []
  (make-mcp-tool
   "emacs_write_file"
   "Writes content to a file, handling both new and existing files.
If the file exists and overwrite is false, returns an error message.
Otherwise, creates or overwrites the file with the given content."
   {:type :object
    :properties {:path {:type :string}
                 :content {:type :string}
                 :overwrite {:type :boolean}
                 :highlight {:type :boolean}
                 :flash {:type :boolean}}
    :required [:path :content]}
   (fn [arg-map]
     (let [overwrite (if (contains? arg-map "overwrite") (get arg-map "overwrite") true)
           highlight (if (contains? arg-map "highlight") (get arg-map "highlight") true)
           flash (if (contains? arg-map "flash") (get arg-map "flash") true)]
       (write-ops/write-file (get arg-map "path") 
                            (get arg-map "content")
                            :overwrite overwrite
                            :highlight highlight
                            :flash flash)))))

(defn emacs-append-to-file-tool []
  (make-mcp-tool
   "emacs_append_to_file"
   "Appends content to the end of a file with optional highlighting.
Adds the content as a new line at the end of the file."
   {:type :object
    :properties {:path {:type :string}
                 :content {:type :string}
                 :highlight_duration {:type :number}}
    :required [:path :content]}
   (fn [arg-map]
     (let [highlight-duration (or (get arg-map "highlight_duration") 2.0)]
       (write-ops/append-to-file (get arg-map "path") 
                                (get arg-map "content")
                                :highlight-duration highlight-duration)))))

;; -------------------------------------------------------------------------
;; Reading operations
;; -------------------------------------------------------------------------

(defn emacs-read-file-tool []
  (make-mcp-tool
   "emacs_read_file"
   "Reads the entire content of a file using Emacs.
Opens the file in Emacs and retrieves its content without any user interaction."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (read-ops/read-file (get arg-map "path")))))

(defn emacs-file-exists-tool []
  (make-mcp-tool
   "emacs_file_exists"
   "Checks if a file or directory exists using Emacs.
Returns a boolean indicating if the file exists."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (read-ops/file-exists? (get arg-map "path")))))

(defn emacs-read-multiple-files-tool []
  (make-mcp-tool
   "emacs_read_multiple_files"
   "Reads the contents of multiple files simultaneously.
More efficient than reading files one by one when you need to analyze
or compare multiple files. Each file's content is returned with metadata."
   {:type :object
    :properties {:paths {:type :array
                        :items {:type :string}}}
    :required [:paths]}
   (fn [arg-map]
     (read-ops/read-multiple-files (get arg-map "paths")))))

;; -------------------------------------------------------------------------
;; File management operations
;; -------------------------------------------------------------------------

(defn emacs-find-files-tool []
  (make-mcp-tool
   "emacs_find_files"
   "Finds files matching a pattern in a directory.
Uses Emacs' directory-files-recursively to find files matching a regex pattern."
   {:type :object
    :properties {:directory {:type :string}
                 :pattern {:type :string}}
    :required [:directory :pattern]}
   (fn [arg-map]
     (manage-ops/find-files (get arg-map "directory") 
                           (get arg-map "pattern")))))

(defn emacs-delete-file-tool []
  (make-mcp-tool
   "emacs_delete_file"
   "Deletes a file with optional confirmation in Emacs."
   {:type :object
    :properties {:path {:type :string}
                 :no_confirm {:type :boolean}}
    :required [:path]}
   (fn [arg-map]
     (let [no-confirm (or (get arg-map "no_confirm") false)]
       (manage-ops/delete-file (get arg-map "path") 
                              :no-confirm no-confirm)))))

(defn emacs-move-file-tool []
  (make-mcp-tool
   "emacs_move_file"
   "Moves or renames files and directories.
Takes source and destination paths. Fails if the destination already exists."
   {:type :object
    :properties {:source {:type :string}
                 :destination {:type :string}}
    :required [:source :destination]}
   (fn [arg-map]
     (manage-ops/move-file (get arg-map "source") 
                          (get arg-map "destination")))))

(defn emacs-copy-file-tool []
  (make-mcp-tool
   "emacs_copy_file"
   "Copies a file from source path to destination path with optional confirmation."
   {:type :object
    :properties {:source {:type :string}
                 :destination {:type :string}
                 :no_confirm {:type :boolean}}
    :required [:source :destination]}
   (fn [arg-map]
     (let [no-confirm (or (get arg-map "no_confirm") false)]
       (manage-ops/copy-file (get arg-map "source") 
                            (get arg-map "destination")
                            :no-confirm no-confirm)))))

(defn emacs-create-directory-tool []
  (make-mcp-tool
   "emacs_create_directory"
   "Creates a new directory or ensures it exists.
Creates parent directories if needed. Succeeds silently if directory already exists."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (manage-ops/create-directory (get arg-map "path")))))

(defn emacs-list-directory-tool []
  (make-mcp-tool
   "emacs_list_directory"
   "Lists directory contents with [FILE] or [DIR] prefixes."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (manage-ops/list-directory (get arg-map "path")))))

(defn emacs-get-file-info-tool []
  (make-mcp-tool
   "emacs_get_file_info"
   "Gets detailed metadata about a file or directory.
Provides information about size, creation time, modification time,
access time, type (file/directory), and permissions (if available)."
   {:type :object
    :properties {:path {:type :string}}
    :required [:path]}
   (fn [arg-map]
     (manage-ops/get-file-info (get arg-map "path")))))

;; -------------------------------------------------------------------------
;; Public API
;; -------------------------------------------------------------------------

(defn file-tools
  "Returns a map of all file-centric MCP tools for Emacs file operations."
  []
  {:emacs-flash-file        (emacs-flash-file-tool)
   :emacs-edit-file         (emacs-edit-file-tool)
   :emacs-write-file        (emacs-write-file-tool)
   :emacs-append-to-file    (emacs-append-to-file-tool)
   :emacs-read-file         (emacs-read-file-tool)
   :emacs-file-exists       (emacs-file-exists-tool)
   :emacs-read-multiple-files (emacs-read-multiple-files-tool)
   :emacs-find-files        (emacs-find-files-tool)
   :emacs-delete-file       (emacs-delete-file-tool)
   :emacs-move-file         (emacs-move-file-tool)
   :emacs-copy-file         (emacs-copy-file-tool)
   :emacs-create-directory  (emacs-create-directory-tool)
   :emacs-list-directory    (emacs-list-directory-tool)
   :emacs-get-file-info     (emacs-get-file-info-tool)})
