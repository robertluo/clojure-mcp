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
            [clojure-mcp.emacs-tools-enhanced.file.operations.visual :as visual-ops]))

;; -------------------------------------------------------------------------
;; Public API
;; -------------------------------------------------------------------------

(defn file-api
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
(def files (file-api))
