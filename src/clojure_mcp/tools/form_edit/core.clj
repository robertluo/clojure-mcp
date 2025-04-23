(ns clojure-mcp.tools.form-edit.core
  "Core utility functions for form editing operations.
   This namespace contains the pure functionality for manipulating Clojure forms
   without any MCP-specific code."
  (:require
   [rewrite-clj.zip :as z]
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [cljfmt.core :as fmt]
   [clojure.string :as str]
   [clojure.java.io :as io]))

;; Form identification and location functions

(defn is-top-level-form?
  "Check if a sexp is a top-level definition with a specific tag and name.
   Supports defmethod forms with dispatch values.
   
   For defmethod forms, the name can be either:
   - Just the method name (e.g., 'area') - will match ANY defmethod with that name
   - A compound name with dispatch value (e.g., 'area :rectangle') - will match only that specific implementation
   
   Arguments:
   - zloc: The zipper location to check
   - tag: The definition tag (e.g., 'defn, 'def, 'defmethod)
   - dname: The name of the definition, which can include dispatch value for defmethod
   
   Returns true if the sexp matches the pattern."
  [zloc tag dname]
  (try
    (let [sexp (z/sexpr zloc)]
      (if (and (list? sexp) (= (first sexp) (symbol tag)))
        ;; Special handling for defmethod forms
        (if (= (symbol tag) 'defmethod)
          (let [method-name (second sexp)
                dispatch-value (nth sexp 2 nil)
                ;; Parse the provided name to see if it includes a dispatch value
                [name-part dispatch-part] (if (string? dname)
                                            (let [parts (str/split dname #"\s+")]
                                              [(first parts)
                                               (when (> (count parts) 1)
                                                 (try
                                                   (read-string (second parts))
                                                   (catch Exception _ nil)))])
                                            [dname nil])]
            (and (= method-name (symbol name-part))
                 (or (nil? dispatch-part)
                     (= dispatch-value dispatch-part))))
          ;; Standard handling for other forms
          (= (second sexp) (symbol dname)))
        false))
    (catch Exception _ false)))

(defn find-top-level-form
  "Find a top-level form with a specific tag and name in a zipper.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The tag name as a string (e.g., \"defn\", \"def\", \"ns\")
   - dname: The name of the definition as a string
   
   Returns the zipper location of the matched form, or nil if not found."
  [zloc tag dname]
  (loop [loc zloc]
    (cond
      (nil? loc) nil
      (is-top-level-form? loc tag dname) loc
      :else (recur (z/right loc)))))

;; Form editing operations

(defn edit-top-level-form
  "Edit a top-level form by replacing it or inserting content before or after.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def, 'ns)
   - name: The name of the form
   - content-str: The string to insert or replace with (can contain multiple forms)
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   
   Returns the updated zipper, or nil if the form was not found."
  [zloc tag name content-str edit-type]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (case edit-type
      :replace (z/replace form-zloc (p/parse-string-all content-str))
      ;; it would be nice if this handled comments immediately preceeding the form
      :before (-> form-zloc
                  (z/insert-left (p/parse-string-all "\n\n"))
                  z/left
                  (z/insert-left (p/parse-string-all content-str))
                  z/left)
      :after (-> form-zloc
                 (z/insert-right (p/parse-string-all "\n\n"))
                 z/right
                 (z/insert-right (p/parse-string-all content-str))
                 z/right))))

;; Offset calculation functions for highlighting modified code

(defn row-col->offset
  "Convert row and column coordinates to a character offset in a string.
   
   Arguments:
   - s: The source string
   - target-row: The target row (1-based)
   - target-col: The target column (1-based)
   
   Returns the character offset in the string."
  [s target-row target-col]
  (loop [lines (str/split-lines s)
         current-row 1
         offset 0]
    (if (or (empty? lines) (>= current-row target-row))
      (+ offset target-col) ; Add col for 1-based index
      (recur (next lines)
             (inc current-row)
             (+ offset (count (first lines)) 1)))))

(defn zloc-offsets
  "Calculate character offsets for a zipper location's start and end positions.
   
   Arguments:
   - source-str: The source code string
   - positions: A vector of [row col] pairs
   
   Returns a vector of character offsets."
  [source-str positions]
  (mapv (fn [[row col]] (row-col->offset source-str row col))
        positions))

;; Functions for working with docstrings

(defn find-docstring
  "Finds the docstring node of a top-level form.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def)
   - name: The name of the form
   
   Returns the zipper positioned at the docstring node, or nil if:
   - The form was not found
   - The form doesn't have a docstring"
  [zloc tag name]
  (when-let [form-zloc (find-top-level-form zloc tag name)]
    (let [tag-zloc (z/down form-zloc) ;; Move to the tag (defn, def, etc.)
          name-zloc (z/right tag-zloc) ;; Move to the name
          docstring-candidate (z/right name-zloc)] ;; Move to potential docstring
      (when (and docstring-candidate
                 ;; Check for both :token (single-line) and :multi-line (multi-line) tags
                 (contains? #{:token :multi-line} (z/tag docstring-candidate))
                 (string? (z/sexpr docstring-candidate)))
        docstring-candidate))))

(defn edit-docstring
  "Edit a docstring in a top-level form.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The form type (e.g., 'defn, 'def)
   - name: The name of the form
   - new-docstring: The new docstring content
   
   Returns the updated zipper, or nil if the form or docstring was not found."
  [zloc tag name new-docstring]
  (when-let [docstring-zloc (find-docstring zloc tag name)]
    ;; Replace the docstring with the new one
    (z/replace docstring-zloc (n/string-node new-docstring))))

;; Functions for working with comments

(defn is-comment-form?
  "Check if a zloc is a (comment ...) form.
   
   Arguments:
   - zloc: The zipper location to check
   
   Returns true if the form is a comment form."
  [zloc]
  (try
    (and (z/seq? zloc)
         (let [first-child (z/down zloc)]
           (and first-child
                (= (z/sexpr first-child) 'comment))))
    (catch Exception _ false)))

(defn is-line-comment?
  "Check if a zloc is a line comment.
   
   Arguments:
   - zloc: The zipper location to check
   
   Returns true if the node is a line comment."
  [zloc]
  (try
    (= (-> zloc z/node n/tag) :comment)
    (catch Exception _ false)))

(defn find-comment-block
  "Find a comment block (either a 'comment' form or consecutive comment lines)
   that contains a specific substring.
   
   This version properly handles comment blocks at the end of the file.
   
   Arguments:
   - source: The source string to search in
   - comment-substring: The substring to look for
   
   Returns a map with :type, :start, :end, and :content keys,
   or nil if no matching comment block is found."
  [source comment-substring]
  (let [zloc (z/of-string source {:track-position? true})
        lines (str/split-lines source)]

    ;; First, try to find a comment form
    (loop [loc zloc]
      (cond
        ;; No more forms
        (nil? loc)
        (let [;; Process line by line to find comment blocks
              result (reduce
                      (fn [[blocks current-block] [idx line]]
                        (cond
                         ;; If we're already tracking a block and the line is a comment
                          (and current-block
                               (str/starts-with? (str/trim line) ";;"))
                          [blocks (update current-block :lines conj line)]

                         ;; If we're tracking a block and hit a non-comment line
                          current-block
                          [(conj blocks (assoc current-block :end (dec idx))) nil]

                         ;; If this is a new comment line
                          (str/starts-with? (str/trim line) ";;")
                          [blocks {:start idx
                                   :lines [line]}]

                         ;; Otherwise, continue
                          :else
                          [blocks nil]))
                      [[] nil]
                      (map-indexed vector lines))

              ;; Extract the blocks and potential unfinished block
              [blocks current-block] result

              ;; Finalize blocks, including any unfinished block at EOF
              final-blocks (if current-block
                             (conj blocks (assoc current-block
                                                 :end (+ (:start current-block)
                                                         (dec (count (:lines current-block))))))
                             blocks)

              ;; Find the first consecutive comment block containing the substring
              matching-block (first (filter #(some (fn [line]
                                                     (str/includes? line comment-substring))
                                                   (:lines %))
                                            final-blocks))]

          (when matching-block
            {:type :line-comments
             :start (:start matching-block)
             :end (:end matching-block)
             :content (str/join "\n" (:lines matching-block))}))

        ;; Check if current form is a comment form
        (is-comment-form? loc)
        (let [comment-str (z/string loc)]
          (if (str/includes? comment-str comment-substring)
            (let [pos (z/position-span loc)]
              {:type :comment-form
               :start (first pos) ;; [row col]
               :end (second pos) ;; [row col]
               :content comment-str
               :zloc loc})
            (recur (z/right loc))))

        ;; Move to the next form
        :else (recur (z/right loc))))))

(defn edit-comment-block
  "Edit a comment block in the source code.
   
   Arguments:
   - source: The source string
   - comment-substring: Substring to identify the comment block
   - new-content: New content to replace the comment block with
   
   Returns the updated source code string, or the original if no matching block was found."
  [source comment-substring new-content]
  (let [block (find-comment-block source comment-substring)]
    (if (nil? block)
      source ;; No matching block found
      (let [lines (str/split-lines source)]
        (case (:type block)
          ;; For comment forms, use zloc to replace
          :comment-form
          (-> (:zloc block)
              (z/replace (p/parse-string new-content))
              z/root-string)

          ;; For line comments, replace the relevant lines
          :line-comments
          (let [start (:start block)
                end (:end block)
                new-lines (str/split-lines new-content)
                result (concat
                        (take start lines)
                        new-lines
                        (drop (inc end) lines))]
            (str/join "\n" result)))))))

;; Form summary and visualization functions

;; Forward declaration for extract-form-name to enable better organization
(declare extract-form-name)

(defn get-form-summary
  "Get a summarized representation of a Clojure form showing only up to the argument list.
   
   Arguments:
   - zloc: The zipper location of the form
   
   Returns a string representation of the form summary, or nil if not a valid form."
  [zloc]
  (try
    (let [sexpr (z/sexpr zloc)]
      (when (and (seq? sexpr) (symbol? (first sexpr)))
        (let [form-type (name (first sexpr))
              form-name (extract-form-name sexpr)]

          (case form-type
            "defn" (let [vector-pos (count (take-while #(not (vector? %)) sexpr))
                         args (when (> (count sexpr) vector-pos)
                                (nth sexpr vector-pos))]
                     (str "(defn " form-name " " args " ...)"))

            "defmacro" (let [vector-pos (count (take-while #(not (vector? %)) sexpr))
                             args (when (> (count sexpr) vector-pos)
                                    (nth sexpr vector-pos))]
                         (str "(defmacro " form-name " " args " ...)"))

            "def" (str "(def " form-name " ...)")
            "deftest" (str "(deftest " form-name " ...)")
            "ns" (z/string zloc) ; Always show the full namespace
            (str "(" form-type " " (or form-name "") " ...)")))))
    (catch Exception _
      ;; Provide a fallback in case of errors
      (try
        (let [raw-str (z/string zloc)]
          (if (< (count raw-str) 60)
            raw-str
            (str (subs raw-str 0 57) "...")))
        (catch Exception _
          nil)))))

(defn valid-form-to-include?
  "Check if a form should be included in the collapsed view.
   Excludes forms like comments, unevals, whitespace, etc.
   
   Arguments:
   - zloc: The zipper location to check
   
   Returns:
   - true if the form should be included, false otherwise"
  [zloc]
  (try
    (when zloc
      (let [tag (z/tag zloc)]
        ;; Exclude specific node types we don't want to process
        (not (or
              ;; Skip uneval forms (#_)
              (= tag :uneval)
              ;; Skip whitespace
              (= tag :whitespace)
              ;; Skip newlines
              (= tag :newline)
              ;; Skip comments
              (= tag :comment)))))
    (catch Exception _
      ;; If we can't determine the type, skip it to be safe
      false)))

(defn extract-form-name
  "Extract the name of a form from its sexpr representation.
   For example, from (defn foo [x] ...) it extracts 'foo'.
   
   Arguments:
   - sexpr: The S-expression to extract the name from
   
   Returns:
   - The name as a string, or nil if no name could be extracted"
  [sexpr]
  (try
    (when (and (seq? sexpr)
               (> (count sexpr) 1)
               (symbol? (second sexpr)))
      (name (second sexpr)))
    (catch Exception _
      nil)))

(defn generate-collapsed-file-view
  "Generates a collapsed view of all top-level forms in a Clojure file.
   
   Arguments:
   - file-path: Path to the Clojure file
   - expand-symbols: Optional sequence of symbol names to show in expanded form
   
   Returns:
   - A string containing the collapsed representation of the file"
  [file-path expand-symbols]
  (try
    (let [file-content (slurp file-path)
          zloc (z/of-string file-content)
          expand-set (set (map name expand-symbols))]

      (loop [loc zloc
             forms []]
        (if (nil? loc)
          ;; Return the final string with all forms
          (str/join "\n\n" forms)

          ;; Process current form
          (let [next-loc (try (z/right loc) (catch Exception _ nil))]
            (if (valid-form-to-include? loc)
              ;; Process includable forms
              (let [current-sexpr (try (z/sexpr loc) (catch Exception _ nil))
                    form-name (extract-form-name current-sexpr)
                    should-expand (contains? expand-set form-name)
                    form-str (if should-expand
                               (z/string loc)
                               (get-form-summary loc))]
                (if form-str
                  (recur next-loc (conj forms form-str))
                  (recur next-loc forms)))
              ;; Skip excluded forms
              (recur next-loc forms))))))
    (catch java.io.FileNotFoundException _
      (str "Error: File not found: " file-path))
    (catch Exception e
      (str "Error generating file view: " (.getMessage e)))))

;; Source code formatting

(defn format-source-string
  "Formats a source code string using cljfmt with comprehensive formatting options.
   
   Arguments:
   - source-str: The source code string to format
   
   Returns:
   - The formatted source code string"
  [source-str]
  (let [formatting-options {:indentation? true
                            :remove-surrounding-whitespace? true
                            :remove-trailing-whitespace? true
                            :insert-missing-whitespace? true
                            :remove-consecutive-blank-lines? true
                            :remove-multiple-non-indenting-spaces? true
                            :split-keypairs-over-multiple-lines? false
                            :sort-ns-references? false
                            :function-arguments-indentation :community
                            :indents fmt/default-indents}]
    (fmt/reformat-string source-str formatting-options)))

;; File operations

(defn load-file-content
  "Loads content from a file.
   
   Arguments:
   - file-path: Path to the file
   
   Returns:
   - The file content as a string, or an error map if the file could not be read"
  [file-path]
  (try
    {:content (slurp file-path)
     :error false}
    (catch java.io.FileNotFoundException _
      {:error true
       :message (str "File not found: " file-path)})
    (catch java.io.IOException e
      {:error true
       :message (str "IO error while reading file: " (.getMessage e))})))

(defn save-file-content
  "Saves content to a file.
   
   Arguments:
   - file-path: Path to the file
   - content: The content to save
   
   Returns:
   - A map with :success true if the file was saved, or :success false and :message if an error occurred"
  [file-path content]
  (try
    (spit file-path content)
    {:success true}
    (catch Exception e
      {:success false
       :message (str "Failed to save file: " (.getMessage e))})))

(defn extract-dispatch-from-defmethod
  "Extracts the method name and dispatch value from defmethod source code.
   Returns [method-name dispatch-value-str] or nil if parsing fails.
   
   Arguments:
   - source-code: The defmethod source code as a string
   
   Returns:
   - A vector of [method-name dispatch-value-str] or nil if parsing fails"
  [source-code]
  (try
    (let [zloc (z/of-string source-code)
          sexp (z/sexpr zloc)]
      (when (and (list? sexp)
                 (= (first sexp) 'defmethod)
                 (>= (count sexp) 3))
        (let [method-name (name (second sexp))
              dispatch-value (nth sexp 2)
              dispatch-str (pr-str dispatch-value)]
          [method-name dispatch-str])))
    (catch Exception _ nil)))

(defn find-and-replace-sexp
  [zloc match-form new-form & {:keys [replace-all whitespace-sensitive]
                               :or {replace-all false
                                    whitespace-sensitive false}}]
  (let [is-blank-new? (str/blank? new-form)
        new-node (when-not is-blank-new? (p/parse-string-all new-form))
        match-node (p/parse-string match-form) ;; must not be blank
        match-str   (n/string match-node)
        match-sexpr (when (not whitespace-sensitive)
                      (try (z/sexpr (z/of-node match-node))
                           (catch Exception _ ::invalid)))]
    (loop [loc zloc
           last-replaced nil
           count 0]
      (if (z/end? loc)
        (when last-replaced
          {:zloc last-replaced
           :count count})
        ;; Check the current node
        (let [curr-tag (z/tag loc)
              node-str (try (n/string (z/node loc)) (catch Exception _ ""))

              matched? (if (and
                            (not whitespace-sensitive)
                            (not= match-sexpr ::invalid)
                            (not= curr-tag :fn)) ;; ??? Skip :fn nodes for sexpr comparison
                         (try
                           (= (z/sexpr loc) match-sexpr)
                           (catch Exception _ false))
                         (= node-str match-str))]
          (if matched?
            (let [updated-loc (if is-blank-new?
                                (try
                                  (z/remove loc)
                                  (catch Exception _
                                    (z/replace loc (n/whitespace-node " "))))
                                (z/replace loc new-node))]
              (if replace-all
                ;; If replacing all, continue with the updated loc
                (recur (z/next updated-loc) updated-loc (inc count))
                ;; Otherwise, return immediately after the first replacement
                {:zloc updated-loc
                 :count 1}))
            ;; No match, continue to the next node
            (recur (z/next loc) last-replaced  count)))))))

(comment
  ;; Examples of using the functions
  (def source "(ns example.core)\n\n(defn my-fn [x y]\n  (+ x y))\n\n(def a 1)")
  (def zloc (z/of-string source))

  (def fr-src "(defn test-fn [x] (+ x 1) (+ x 1) (- x 2))")
  
  (let [res (find-and-replace-sexp
             (z/of-string (slurp "test-sexp.clj")
                          {:track-position? true}
                          )
             "(+ x y)"
             "(+ x 55555555555555555)"
             :replace-all false)]
    [(z/root-string (:zloc res))
     (z/position-span (:zloc res))]
   )
  
  ;; Find a function
  (def fn-zloc (find-top-level-form zloc "defn" "my-fn"))

  ;; Get function summary
  (get-form-summary fn-zloc)

  ;; Edit a function
  (def edited-zloc (edit-top-level-form zloc "defn" "my-fn"
                                        "(defn my-fn [x y]\n  (* x y))"
                                        :replace))

  ;; Format source
  (format-source-string (z/root-string edited-zloc))

  ;; Test comment functions
  (def comment-source "(ns example.core)\n\n;; This is a test comment\n;; with multiple lines\n\n(comment\n  (+ 1 2)\n  (* 3 4))")
  (def comment-source2 "(ns example.core)\n\n\n\n(comment\n  (+ 1 2)\n  (* 3 4)) ;; This is a test comment\n;; with multiple lines")
  (find-comment-block comment-source2 "test comment")
  (find-comment-block comment-source2 "(+ 1 2)")
  (edit-comment-block comment-source2 "test comment" ";; Updated comment\n;; with new content"))
