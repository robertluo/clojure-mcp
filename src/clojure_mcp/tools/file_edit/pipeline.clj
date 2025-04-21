(ns clojure-mcp.tools.file-edit.pipeline
  "Pipeline architecture for file editing operations.
   Provides a thread-first pattern with error short-circuiting and
   standardized context maps for file editing."
  (:require
   [clojure-mcp.tools.file-edit.core :as core]
   [clojure-mcp.tools.form-edit.pipeline :as form-pipeline]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; We'll reuse many definitions from form-edit pipeline and add our own specific ones

;; Additional context map specs
(s/def ::old-string string?)
(s/def ::new-string string?)
(s/def ::create-new-file boolean?)

;; Pipeline specific steps

(defn validate-edit
  "Validates the file edit operation.
   Requires ::file-path, ::old-string, and ::new-string in the context.
   May add ::create-new-file to the context if this is a new file creation."
  [ctx]
  (let [file-path (::form-pipeline/file-path ctx)
        old-string (::old-string ctx)
        new-string (::new-string ctx)
        source (::form-pipeline/source ctx) ;; This will be nil for non-existent files
        validation-result (core/validate-file-edit file-path old-string new-string source)]
    (if (:valid validation-result)
      (cond-> ctx
        (:create-new-file validation-result) (assoc ::create-new-file true))
      {::form-pipeline/error true
       ::form-pipeline/message (:message validation-result)})))

(defn perform-edit
  "Performs the actual edit on the file content.
   Requires ::file-path, ::old-string, ::new-string, and optionally ::form-pipeline/source in the context.
   Adds ::form-pipeline/output-source to the context."
  [ctx]
  (let [file-path (::form-pipeline/file-path ctx)
        old-string (::old-string ctx)
        new-string (::new-string ctx)
        source (::form-pipeline/source ctx)
        create-new-file (::create-new-file ctx false)
        ;; Get new content
        edited-content (if create-new-file
                         new-string
                         (core/perform-file-edit file-path old-string new-string source))]
    (assoc ctx ::form-pipeline/output-source edited-content)))

(defn save-file-with-dirs
  "Saves the updated source to the file, creating parent directories if needed.
   Requires ::form-pipeline/output-source and ::form-pipeline/file-path in the context."
  [ctx]
  (try
    (let [file-path (::form-pipeline/file-path ctx)
          output-source (::form-pipeline/output-source ctx)
          save-result (core/save-file-content file-path output-source)]
      (if (:success save-result)
        ctx
        {::form-pipeline/error true
         ::form-pipeline/message (:message save-result)}))
    (catch Exception e
      {::form-pipeline/error true
       ::form-pipeline/message (str "Failed to save file: " (.getMessage e))})))

;; Define our file edit pipeline function that composes steps from form-edit pipeline and our own

(defn file-edit-pipeline
  "Pipeline for editing a file by replacing a string or creating a new file.
   
   Arguments:
   - file-path: Path to the file to edit
   - old-string: String to replace (empty string for new file)
   - new-string: New string to insert
   - config: Optional tool configuration map
   
   Returns:
   - A context map with the result of the operation"
  [file-path old-string new-string & [config]]
  (let [initial-ctx {::form-pipeline/file-path file-path
                     ::old-string old-string
                     ::new-string new-string
                     ::form-pipeline/config config}
        ;; For new file creation, we skip loading the source
        create-new-file? (empty? old-string)]
    (if create-new-file?
      ;; Pipeline for new file creation
      (form-pipeline/thread-ctx
       (assoc initial-ctx ::create-new-file true)
       validate-edit
       perform-edit
       form-pipeline/determine-file-type ;; This will mark as "create"
       form-pipeline/generate-diff
       form-pipeline/emacs-set-auto-revert
       save-file-with-dirs)
      
      ;; Pipeline for existing file edit
      (form-pipeline/thread-ctx
       initial-ctx
       form-pipeline/load-source ;; Load existing file
       validate-edit ;; Validate the edit (uniqueness, etc.)
       perform-edit ;; Perform the actual edit
       form-pipeline/determine-file-type ;; This will mark as "update"
       form-pipeline/generate-diff ;; Generate diff between old and new
       form-pipeline/emacs-set-auto-revert
       save-file-with-dirs))))

;; Format result for tool consumption
(defn format-result
  "Format the result of the pipeline for tool consumption.
   
   Arguments:
   - ctx: The final context map from the pipeline
   
   Returns:
   - A map with :error, :message, and :diff keys"
  [ctx]
  (if (::form-pipeline/error ctx)
    {:error true
     :message (::form-pipeline/message ctx)}
    {:error false
     :diff (::form-pipeline/diff ctx)
     :type (::form-pipeline/type ctx)}))

(comment
  ;; === Examples of using the file-edit pipeline directly ===
  
  ;; Test file paths
  (def temp-dir (System/getProperty "java.io.tmpdir"))
  (def test-file (str temp-dir "/file-edit-test.txt"))
  (def test-nested-file (str temp-dir "/nested/test.txt"))
  
  ;; Create a test file
  (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")
  
  ;; Test the pipeline with simple edit
  (def result (file-edit-pipeline test-file "Line 3" "Line 3 - EDITED"))
  (format-result result)
  
  ;; Test the pipeline with new file creation
  (def create-result (file-edit-pipeline test-nested-file "" "New content"))
  (format-result create-result)
  
  ;; Test the pipeline with error (non-unique match)
  (def error-result (file-edit-pipeline test-file "Line" "EDITED Line"))
  (format-result error-result)
  
  ;; Clean up
  (.delete (io/file test-file))
  (.delete (io/file test-nested-file))
  (when (.exists (io/file (str temp-dir "/nested")))
    (.delete (io/file (str temp-dir "/nested"))))
)