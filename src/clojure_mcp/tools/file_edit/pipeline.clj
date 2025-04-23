(ns clojure-mcp.tools.file-edit.pipeline
  "Pipeline architecture for file editing operations.
   Provides a thread-first pattern with error short-circuiting and
   standardized context maps for file editing."
  (:require
   [clojure-mcp.tools.file-edit.core :as core]
   [clojure-mcp.tools.form-edit.pipeline :as form-pipeline]
   [clojure-mcp.tools.file-write.core :as file-write-core]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure-mcp.linting :as linting]
   [clojure.spec.alpha :as s]
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; We'll reuse many definitions from form-edit pipeline and add our own specific ones

;; Additional context map specs
(s/def ::old-string string?)
(s/def ::new-string string?)
;; Pipeline specific steps

(defn validate-edit
  "Validates the file edit operation.
   Requires ::file-path, ::old-string, and ::new-string in the context."
  [ctx]
  (let [file-path (::form-pipeline/file-path ctx)
        old-string (::old-string ctx)
        new-string (::new-string ctx)
        source (::form-pipeline/source ctx) ;; This will be nil for non-existent files
        validation-result (core/validate-file-edit file-path old-string new-string source)]
    (if (:valid validation-result)
      ctx
      {::form-pipeline/error true
       ::form-pipeline/message (:message validation-result)})))

(defn perform-edit
  "Performs the actual edit on the file content.
   Requires ::file-path, ::old-string, ::new-string, and ::form-pipeline/source in the context.
   Adds ::form-pipeline/output-source to the context."
  [ctx]
  (let [file-path (::form-pipeline/file-path ctx)
        old-string (::old-string ctx)
        new-string (::new-string ctx)
        source (::form-pipeline/source ctx)
        ;; Get new content by performing the edit
        edited-content (core/perform-file-edit file-path old-string new-string source)]
    (assoc ctx ::form-pipeline/output-source edited-content)))

(defn lint-clojure-content
  "Lints the output source if it's a Clojure file.
   Catches syntax errors before attempting to save the file.
   Requires ::form-pipeline/file-path and ::form-pipeline/output-source in the context."
  [ctx]
  (let [file-path (::form-pipeline/file-path ctx)
        output-source (::form-pipeline/output-source ctx)]
    ;; Only lint Clojure files
    (if (and (file-write-core/is-clojure-file? file-path) output-source)
      ;; Check for syntax errors
      (let [lint-result (linting/lint output-source)]
        (if (and lint-result (:error? lint-result))
          ;; Report linting errors
          {::form-pipeline/error true
           ::form-pipeline/message (str "Syntax errors detected in Clojure code:\n"
                                        (:report lint-result)
                                        "\nPlease fix the syntax errors before saving.")}
          ;; No linting errors, continue
          ctx))
      ;; Not a Clojure file or no content, skip linting
      ctx)))

;; Define our file edit pipeline function that composes steps from form-edit pipeline and our own

(defn file-edit-pipeline
  "Pipeline for editing a file by replacing a string.
   
   Arguments:
   - file-path: Path to the file to edit
   - old-string: String to replace
   - new-string: New string to insert
   - config: Optional tool configuration map
   
   Returns:
   - A context map with the result of the operation"
  [file-path old-string new-string & [config]]
  (let [initial-ctx {::form-pipeline/file-path file-path
                     ::old-string old-string
                     ::new-string new-string
                     ::form-pipeline/config config}]
    ;; Pipeline for existing file edit
    (form-pipeline/thread-ctx
     initial-ctx
     form-pipeline/load-source ;; Load existing file
     validate-edit ;; Validate the edit (uniqueness, etc.)
     perform-edit ;; Perform the actual edit
     lint-clojure-content ;; Lint Clojure files to catch syntax errors
     form-pipeline/determine-file-type ;; This will mark as "update"
     form-pipeline/generate-diff ;; Generate diff between old and new
     form-pipeline/emacs-set-auto-revert
     form-pipeline/save-file)))

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

  ;; Create a test file
  (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")

  ;; Test the pipeline with simple edit
  (def result (file-edit-pipeline test-file "Line 3" "Line 3 - EDITED"))
  (format-result result)

  ;; Test the pipeline with error (non-unique match)
  (def error-result (file-edit-pipeline test-file "Line" "EDITED Line"))
  (format-result error-result)

  ;; Test the pipeline with error (empty old_string)
  (def empty-string-result (file-edit-pipeline test-file "" "New content"))
  (format-result empty-string-result)

  ;; Clean up
  (.delete (io/file test-file)))
