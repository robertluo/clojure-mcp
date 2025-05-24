(ns clojure-mcp.other-tools.move-file.core
  "Core implementation for the move-file tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]))

(defn move-file
  "Moves or renames a file or directory.
   
   Parameters:
   - source: The validated and normalized source path
   - destination: The validated and normalized destination path
   
   Returns a map with:
   - :success - Boolean indicating if the operation was successful
   - :source - The source path
   - :destination - The destination path
   - :error - Error message if the operation failed
   
   The operation will fail if:
   - The source doesn't exist
   - The destination already exists
   - The move operation fails for any reason"
  [source destination]
  (let [source-file (io/file source)
        dest-file (io/file destination)]

    (cond
      ;; Case 1: Source doesn't exist
      (not (.exists source-file))
      {:success false
       :source source
       :destination destination
       :error (str "Source file or directory does not exist: " source)}

      ;; Case 2: Destination already exists
      (.exists dest-file)
      {:success false
       :source source
       :destination destination
       :error (str "Destination already exists: " destination)}

      ;; Case 3: Try to perform the move
      :else
      (try
        ;; Determine the type before moving the file
        (let [file-type (cond
                          (.isFile source-file) "file"
                          (.isDirectory source-file) "directory"
                          :else "unknown")]
          (if (.renameTo source-file dest-file)
            ;; Success case
            {:success true
             :source source
             :destination destination
             :type file-type}
            ;; Rename failed but didn't throw exception
            {:success false
             :source source
             :destination destination
             :error "Move operation failed. This could be due to permissions or trying to move across different filesystems."}))
        (catch Exception e
          {:success false
           :source source
           :destination destination
           :error (str "Error during move operation: " (.getMessage e))})))))

(comment
  ;; === Examples of using the move-file core functionality directly ===

  ;; Test within temp directory
  (def temp-dir (System/getProperty "java.io.tmpdir"))
  (def test-source (str temp-dir "/test-source.txt"))
  (def test-dest (str temp-dir "/test-dest.txt"))

  ;; Create a test file
  (spit test-source "Test content")

  ;; Test move operation
  (move-file test-source test-dest)

  ;; Verify destination exists and source doesn't
  (.exists (io/file test-dest)) ;; Should be true
  (.exists (io/file test-source)) ;; Should be false

  ;; Clean up
  (io/delete-file test-dest))