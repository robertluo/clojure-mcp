(ns clojure-mcp.tools.grep.core
  "Core implementation for the grep tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.repl-tools.filesystem.grep :as fs-grep]
   [clojure.java.io :as io]))

(defn grep-files
  "Searches for patterns in file contents.
   
   Parameters:
   - path: The validated and normalized path to search in
   - pattern: Regular expression pattern to search for
   - include: File pattern to include (e.g. \"*.clj\", \"*.{clj,cljs}\")
   - max-results: Maximum number of results to return (default: 1000)
   
   Returns a map with:
   - :filenames - Vector of files containing the pattern, sorted by modification time
   - :numFiles - Number of matching files found
   - :durationMs - Time taken for the search in milliseconds
   - :error - Error message if an error occurred"
  [path pattern & {:keys [include max-results] :or {include nil max-results 1000}}]
  (fs-grep/grep-files path pattern :include include :max-results max-results))