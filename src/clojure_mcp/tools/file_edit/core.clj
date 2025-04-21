(ns clojure-mcp.tools.file-edit.core
  "Core implementation for the file-edit tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn load-file-content
  "Load file content from a path.
   
   Parameters:
   - path: Path to the file to load
   
   Returns a map with:
   - :content - The file contents as a string
   - :error - Boolean indicating if an error occurred
   - :message - Error message (only when :error is true)"
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        {:content (slurp file)
         :error false}
        {:error true
         :message (str "File not found: " path)}))
    (catch Exception e
      {:error true
       :message (str "Error reading file: " (.getMessage e))})))

(defn count-occurrences
  "Count occurrences of a substring within a string."
  [string substring]
  (if (empty? substring)
    0
    (count (re-seq (re-pattern (java.util.regex.Pattern/quote substring)) string))))

(defn suggest-similar-file
  "Try to find a similar file with a different extension.
   Return nil if no similar file found."
  [file-path]
  (let [f (io/file file-path)
        dir (.getParent f)
        name-without-ext (second (re-find #"(.+?)(?:\.[^.]*$|$)" (.getName f)))
        dir-file (io/file dir)
        similar-files (when (and dir-file (.exists dir-file) (.isDirectory dir-file))
                        (->> (.listFiles dir-file)
                             (filter #(.isFile %))
                             (filter #(str/starts-with? (.getName %) name-without-ext))
                             (filter #(not= (.getAbsolutePath %) file-path))))]
    (when (seq similar-files)
      (.getAbsolutePath (first similar-files)))))

(defn validate-file-edit
  "Validate a file edit operation.
   
   Parameters:
   - file-path: The path to the file
   - old-string: The string to replace
   - new-string: The replacement string
   - file-content: The current file content (or nil if file doesn't exist)
   
   Returns a map with:
   - :valid - Boolean indicating if the edit is valid
   - :message - Error message (only present when :valid is false)
   - :create-new-file - Boolean indicating if this is a new file creation (only when :valid is true)"
  [file-path old-string new-string file-content]
  (cond
    ;; Case 1: No changes (old-string and new-string are identical)
    (= old-string new-string)
    {:valid false
     :message "No changes to make: old_string and new_string are exactly the same."}
    
    ;; Case 2: Creating new file (empty old-string)
    (empty? old-string)
    (let [file (io/file file-path)]
      (if (.exists file)
        {:valid false
         :message "Cannot create new file - file already exists."}
        {:valid true
         :create-new-file true}))
    
    ;; Case 3: File doesn't exist but trying to edit it
    (nil? file-content)
    (let [similar-file (suggest-similar-file file-path)
          message (str "File does not exist." 
                       (when similar-file (str " Did you mean " similar-file "?")))]
      {:valid false
       :message message})
    
    ;; Case 4: No matches of old-string in file
    (not (str/includes? file-content old-string))
    {:valid false
     :message "String to replace not found in file."}
    
    ;; Case 5: Multiple matches of old-string (not unique)
    (> (count-occurrences file-content old-string) 1)
    (let [matches (count-occurrences file-content old-string)]
      {:valid false
       :message (str "Found " matches " matches of the string to replace. "
                    "For safety, this tool only supports replacing exactly one occurrence at a time. "
                    "Add more lines of context to your edit and try again.")})
    
    ;; Case 6: Valid edit
    :else
    {:valid true
     :create-new-file false}))

(defn perform-file-edit
  "Perform the actual file edit operation.
   
   Parameters:
   - file-path: The path to the file
   - old-string: The string to replace
   - new-string: The replacement string
   - file-content: The current file content (or nil for new files)
   
   Returns the new file content."
  [file-path old-string new-string file-content]
  (if (empty? old-string)
    ;; New file case
    new-string
    ;; Edit existing file case
    (str/replace-first file-content old-string new-string)))

(defn save-file-content 
  "Save content to a file, creating parent directories if needed.
   
   Parameters:
   - file-path: Path to the file to save
   - content: Content to save
   
   Returns a map with:
   - :success - Boolean indicating if the save was successful
   - :message - Error message (only when :success is false)"
  [file-path content]
  (try
    (let [file (io/file file-path)
          parent (.getParentFile file)]
      ;; Create parent directories if they don't exist
      (when (and parent (not (.exists parent)))
        (.mkdirs parent))
      (spit file content)
      {:success true})
    (catch Exception e
      {:success false
       :message (str "Failed to save file: " (.getMessage e))})))

(comment
  ;; === Examples of using the file-edit core functionality directly ===
  
  ;; Test file paths
  (def temp-dir (System/getProperty "java.io.tmpdir"))
  (def test-file (str temp-dir "/file-edit-test.txt"))
  (def test-nested-file (str temp-dir "/nested/test.txt"))
  
  ;; Create a test file
  (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
  
  ;; Test validation
  (validate-file-edit test-file "Line 3" "Line 3 - EDITED" (slurp test-file))
  (validate-file-edit test-file "Line" "EDITED Line" (slurp test-file))
  (validate-file-edit (str temp-dir "/nonexistent.txt") "Line" "EDITED Line" nil)
  (validate-file-edit test-nested-file "" "New content" nil)
  
  ;; Test editing
  (perform-file-edit test-file "Line 3" "Line 3 - EDITED" (slurp test-file))
  (perform-file-edit test-nested-file "" "New content" nil)
  
  ;; Test saving with parent directory creation
  (save-file-content test-nested-file "New content")
  
  ;; Clean up
  (.delete (io/file test-file))
  (.delete (io/file test-nested-file))
  (when (.exists (io/file (str temp-dir "/nested")))
    (.delete (io/file (str temp-dir "/nested"))))
)