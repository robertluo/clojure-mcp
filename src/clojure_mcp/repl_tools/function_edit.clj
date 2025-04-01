(ns clojure-mcp.repl-tools.function-edit
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]))

(defn is-top-def-name?
  "Check if a sexp is a top-level definition with a specific tag and name.
   
   Arguments:
   - sexp: The S-expression to check
   - tag: The definition tag (e.g., 'defn, 'def)
   - dname: The name of the definition
   
   Returns true if the sexp matches the pattern (tag dname ...)"
  [sexp tag dname]
  (and (list? sexp)
       (= (first sexp) tag)
       (= (second sexp) dname)))

(defn find-toplevel-def
  "Find a top-level definition with a specific tag and name in a zipper.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The definition tag (e.g., 'defn, 'def)
   - dname: The name of the definition
   
   Returns the zipper location of the matched definition, or nil if not found."
  [zloc tag dname]
  (some->> zloc
           (iterate z/right)
           (take-while identity)
           (filter #(try 
                      (is-top-def-name? (z/sexpr %) tag dname)
                      (catch Exception _ false)))
           first))

(defn replace-top-level-def*
  "Replace a top-level definition with a new string.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The definition tag (e.g., 'defn, 'def)
   - name: The name of the definition
   - replacement-str: The string with the new definition
   
   Returns the updated zipper, or nil if the definition was not found."
  [zloc tag name replacement-str]
  (when-let [def-zloc (find-toplevel-def zloc tag name)]
    (z/edit def-zloc (fn [_] 
                      (-> replacement-str 
                          p/parse-string-all)))))

(defn replace-top-level-def
  "Replace a top-level definition with a new string, inferring the tag from the replacement.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - name: The name of the definition
   - replacement-str: The string with the new definition
   
   Returns the updated zipper, or nil if the definition was not found."
  [zloc name replacement-str]
  (let [parsed (try (z/of-string replacement-str) (catch Exception _ nil))
        tag (when parsed (first (z/sexpr parsed)))]
    (when tag
      (replace-top-level-def* zloc tag name replacement-str))))

(defn edit-function-in-file
  "Edit a function in a file, replacing it with a new implementation.
   
   Arguments:
   - file-path: Path to the file containing the function
   - fn-name: Name of the function to replace (symbol)
   - new-fn-str: String with the new function implementation
   
   Returns the updated file content as a string."
  [file-path fn-name new-fn-str]
  (let [file-content (slurp file-path)
        zloc (z/of-string file-content)
        updated-zloc (replace-top-level-def zloc fn-name new-fn-str)]
    (if updated-zloc
      (z/root-string updated-zloc)
      file-content)))

(defn replace-function-in-file
  "Replace a function in a file with a new implementation.
   
   Arguments:
   - fn-name: Name of the function to replace (string or symbol)
   - file-path: Path to the file
   - new-fn-str: String with the new function implementation
   
   Returns true if the function was replaced successfully, false otherwise."
  [fn-name file-path new-fn-str]
  (try
    (let [fn-name (if (string? fn-name) (symbol fn-name) fn-name)
          file-content (slurp file-path)
          zloc (z/of-string file-content)
          updated-zloc (replace-top-level-def zloc fn-name new-fn-str)]
      (if updated-zloc
        (do
          (spit file-path (z/root-string updated-zloc))
          true)
        false))
    #_(catch Exception e
      (println "Error replacing function:" (.getMessage e))
      false)))

(comment
  ;; Example usage
  (def sample "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

(defn bar [] true)")
  
  ;; Test finding a function
  (find-toplevel-def (z/of-string sample) 'defn 'foo)
  
  ;; Test replacing a function
  (z/root-string 
   (replace-top-level-def 
    (z/of-string sample) 
    'bar 
    "(defn bar [] 'updated-value)"))
  
  ;; Example of editing a function in a file
  (def my-code
    ";; A sample file
  (ns my.app
    (:require [clojure.string :as str]))

  (defn helper [z]
    (inc z))

  ;; The function we want to replace
  (defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

  (defn bar []
    :baz)")

  (def replacement-foo
    "(defn foo
    \"This is the new foo.
     It takes one argument.\"
    [a]
    (println \"New foo function!\")
    (* a a))")
  
  ;; Replace foo in my-code
  (println
   (edit-function-in-file 
    "dummy-path.clj"  ;; In real use, this would be an actual file path
    'foo 
    replacement-foo))
)
