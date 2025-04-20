(ns clojure-mcp.tools.list-directory.core
  "Core implementation for the list-directory tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]))

(defn list-directory
  "Lists files and directories at the specified path.
   
   Parameters:
   - path: The validated and normalized path to the directory
   
   Returns:
   - A map with :files and :directories vectors and :full-path string.
   - A map with :error string if the path does not exist or is not a directory."
  [path]
  (let [dir (io/file path)]
    (if (.exists dir)
      (if (.isDirectory dir)
        (let [contents (.listFiles dir)
              files (filter #(.isFile %) contents)
              dirs (filter #(.isDirectory %) contents)]
          {:files (mapv #(.getName %) files)
           :directories (mapv #(.getName %) dirs)
           :full-path (.getAbsolutePath dir)})
        {:error (str path " is not a directory")})
      {:error (str path " does not exist")})))