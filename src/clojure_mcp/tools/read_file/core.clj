(ns clojure-mcp.tools.read-file.core
  "Core implementation for the read-file tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.repl-tools.filesystem.core :as fs-core]))

(defn read-file
  "Reads a file from the filesystem, with optional line offset and limit.
   
   Parameters:
   - path: The validated and normalized path to the file
   - offset: Line number to start reading from (0-indexed, default 0)
   - limit: Maximum number of lines to read (default 2000)
   - max-line-length: Maximum length per line before truncation (default 1000)
   
   Returns the result from read-file-contents directly"
  [path offset limit & {:keys [max-line-length] :or {max-line-length 1000}}]
  (fs-core/read-file-contents path 
                             :offset offset
                             :max-lines limit
                             :max-line-length max-line-length))