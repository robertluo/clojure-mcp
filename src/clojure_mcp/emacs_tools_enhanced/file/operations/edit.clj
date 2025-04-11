(ns clojure-mcp.emacs-tools-enhanced.file.operations.edit
  "File editing operations for Emacs.
   
   This namespace provides functions for editing file content in Emacs."
  (:require [clojure.string :as str]
            [clojure-mcp.emacs-tools-enhanced.file.core :refer [emacs-eval with-file]]))

(defn- dry-run-edits
  "Generates a diff preview of the changes that would be made by the edits.
   
   Parameters:
   - file-path: Path to the file to edit
   - edits: Sequence of maps with :old-text and :new-text keys
   
   Returns a map with:
   - :success - Whether the operation was successful
   - :message - Message about dry run
   - :diff - Git