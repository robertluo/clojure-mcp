(ns clojure-mcp.repl-tools.function-edit
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn replace-function-in-file
  "Replaces the text of a function definition in a file with new text.

  WARNING: This function uses regular expressions to find the function
  definition boundaries. This can be unreliable, especially with complex code,
  macros, comments, or nested definitions. It assumes top-level function
  definitions start with '(defn function-name ...' or '(def function-name ...'
  near the beginning of a line and are followed by another top-level form
  starting similarly or the end of the file. Use with caution. A more robust
  solution would involve parsing the code (e.g., using rewrite-clj).

  Args:
    function-name: The simple name of the function (String).
    filepath: The path to the Clojure source file (String).
    new-function-text: The complete new text for the function definition (String).

  Returns:
    true if replacement was successful, false otherwise (e.g., file not found,
    function not found, write error)."
  [function-name filepath new-function-text]
  (try
    (let [file-content (slurp filepath)
          ;; Regex to find the start of the function definition (defn or def)
          ;; Allows for whitespace before the opening paren and requires whitespace after defn/def and the name
          ;; Matches the function name as a whole word to avoid partial matches.
          start-pattern (re-pattern (str "(?m)^\\s*\\((?:defn|def)\\s+" (java.util.regex.Matcher/quoteReplacement function-name) "\\b"))
          matcher (re-matcher start-pattern file-content)
          found? (.find matcher)]

      (if-not found?
        (do
          (println (str "Error: Function '" function-name "' not found starting with '(defn " function-name " ...' or '(def " function-name " ...' in file: " filepath))
          false) ; Function start not found
        (let [start-index (.start matcher)
              ;; Find the start of the *next* top-level form or EOF
              ;; This is a fragile heuristic! Assumes next form starts similarly at the beginning of a line.
              ;; Looks for (defn ..., (def ..., (comment ..., etc. at the start of a line.
              next-def-pattern (re-pattern "(?m)^\\s*\\(")
              ;; Search *after* the line where the found function started to avoid matching itself immediately
              start-line-end (.indexOf file-content "\n" start-index)
              search-from (if (neg? start-line-end) start-index (inc start-line-end))

              next-matcher (doto (re-matcher next-def-pattern file-content)
                             (.region search-from (count file-content))) ; Search only after the current function's start line

              end-index (if (.find next-matcher)
                          (.start next-matcher) ; Index relative to original string
                          (count file-content))] ; If no next def found, go to end of file

          ;; Construct the new content
          (let [content-before (subs file-content 0 start-index)
                content-after (subs file-content end-index)
                ;; Ensure the new text ends with a newline. Add blank line separator if not at EOF.
                formatted-new-text (cond-> (str/trimr new-function-text)
                                     (not (str/ends-with? new-function-text "\n")) (str "\n")
                                     (not (empty? content-after)) (str "\n")) ; Add blank line separator if not EOF
                new-content (str content-before formatted-new-text content-after)]
            ;; Write the modified content back to the file
            (spit filepath new-content)
            true)))) ; Success
    (catch java.io.FileNotFoundException _
      (println (str "Error: File not found: " filepath))
      false)
    (catch Exception e
      (println (str "Error processing file " filepath ": " (.getMessage e)))
      false)))
