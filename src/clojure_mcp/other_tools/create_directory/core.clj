(ns clojure-mcp.other-tools.create-directory.core
  "Core implementation for the create-directory tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]))

(defn create-directory
  "Creates a directory (and parent directories if needed) or succeeds silently if it exists.
   
   Parameters:
   - path: The validated and normalized path to the directory to create
   
   Returns a map with:
   - :success - Boolean indicating if the operation was successful
   - :path - The directory path
   - :exists - Boolean indicating if the directory already existed
   - :created - Boolean indicating if the directory was newly created
   - :error - Error message if the operation failed (only when success is false)"
  [path]
  (let [dir-file (io/file path)]
    (cond
      ;; Case 1: Directory already exists
      (.exists dir-file)
      (if (.isDirectory dir-file)
        {:success true
         :path path
         :exists true
         :created false}
        {:success false
         :path path
         :error (str "Path exists but is a file, not a directory: " path)})

      ;; Case 2: Try to create directory
      :else
      (try
        (if (.mkdirs dir-file)
          {:success true
           :path path
           :exists false
           :created true}
          {:success false
           :path path
           :error (str "Failed to create directory: " path)})
        (catch Exception e
          {:success false
           :path path
           :error (str "Error creating directory: " (.getMessage e))})))))

(comment
  ;; === Examples of using the create-directory core functionality directly ===

  ;; Test within temp directory
  (def temp-dir (System/getProperty "java.io.tmpdir"))
  (def test-path (str temp-dir "/test-dir/nested/path"))

  ;; Create directory
  (create-directory test-path)

  ;; Verify directory exists
  (.exists (io/file test-path)) ;; Should be true
  (.isDirectory (io/file test-path)) ;; Should be true

  ;; Create same directory again (should not error)
  (create-directory test-path)

  ;; Create a file that conflicts with directory path
  (def conflict-path (str temp-dir "/test-file-not-dir"))
  (spit conflict-path "test content")
  (create-directory conflict-path) ;; Should fail

  ;; Clean up
  (io/delete-file conflict-path)
  (.delete (io/file test-path))
  (.delete (io/file (str temp-dir "/test-dir/nested")))
  (.delete (io/file (str temp-dir "/test-dir"))))