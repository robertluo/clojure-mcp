(ns clojure-mcp.repl-tools.function-edit
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [rewrite-clj.zip :as z]))

(defn- find-toplevel-definition
  "Finds the zipper location of a top-level def or defn form by name."
  [root-zloc function-name-sym]
  (->> (z/children root-zloc) ;; Get all top-level children
       (filter z/sexprable?)   ;; Only consider nodes that represent forms (ignore whitespace/comments)
       (some (fn [loc]         ;; Find the first one that matches
               (let [sexpr (z/sexpr loc)]
                 (when (and (list? sexpr)
                            (#{'def 'defn} (first sexpr))
                            (= function-name-sym (second sexpr)))
                   loc))))))     ;; Return the matching zipper location, or nil if none found

(defn replace-function-in-file
  "Replaces the text of a function definition in a file with new text using rewrite-clj.

  This function parses the Clojure source file, finds the top-level definition
  (def or defn) matching the given function name, and replaces its entire
  form with the parsed new function text. It preserves surrounding code and comments.

  Args:
    function-name: The simple name of the function (String or Symbol).
    filepath: The path to the Clojure source file (String).
    new-function-text: The complete new text for the function definition (String).

  Returns:
    true if replacement was successful, false otherwise (e.g., file not found,
    parse error, function not found, write error)."
  [function-name filepath new-function-text]
  (try
    (let [function-name-sym (symbol function-name) ;; Ensure it's a symbol
          zloc (z/of-file filepath {:track-position? true})] ; Load file into zipper

      ;; Pass the root zloc to the search function
      (if-let [target-loc (find-toplevel-definition zloc function-name-sym)]
        (try
          (let [new-node (z/of-string new-function-text) ;; Use z/of-string to parse the new text
                ;; Replace the found node with the new node
                edited-zloc (z/replace target-loc new-node)
                ;; Get the modified code as a string
                new-content (z/root-string edited-zloc)]
            ;; Write the modified content back to the file
            (spit filepath new-content)
            true) ; Success
          (catch Exception e ; Catch potential parse errors in new-function-text
            (println (str "Error parsing new function text for '" function-name "': " (.getMessage e)))
            false))
        (do
          (println (str "Error: Function definition '" function-name "' not found as a top-level def/defn in file: " filepath))
          false))) ; Function not found

    (catch java.io.FileNotFoundException _
      (println (str "Error: File not found: " filepath))
      false)
    (catch Exception e ; Catch potential file read or initial parse errors
      (println (str "Error processing file " filepath ": " (.getMessage e)))
      false)))
