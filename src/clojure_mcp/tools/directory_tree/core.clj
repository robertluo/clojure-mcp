(ns clojure-mcp.tools.directory-tree.core
  "Core implementation for the directory-tree tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn directory-tree
  "Creates a recursive tree view of directory structure using simple indentation.
   
   Parameters:
   - path: The validated and normalized path to the directory
   - depth: Current depth level (used for indentation)
   - max-depth: Maximum depth to traverse (nil for unlimited)
   
   Returns a string representation of the directory tree with proper indentation.
   Filters out temporary files like Emacs backup files (ending with ~).
   Adds a \"...\" indicator when directories are truncated due to max-depth."
  [path & {:keys [depth max-depth] :or {depth 0 max-depth nil}}]
  (let [dir (io/file path)]
    (if (and (.exists dir) (.isDirectory dir))
      (let [indent (apply str (repeat depth "  "))
            contents (.listFiles dir)
            ; Filter out temporary files (like Emacs backup files)
            is-temp-file? (fn [file]
                            (let [name (.getName file)]
                              (or (.endsWith name "~") ; Emacs backup
                                  (.startsWith name ".") ; Hidden files
                                  (.startsWith name "#") ; Emacs auto-save
                                  (.contains name ".#")))) ; Emacs lock files
            dirs (filter #(and (.isDirectory %) (not (is-temp-file? %))) contents)
            files (filter #(and (.isFile %) (not (is-temp-file? %))) contents)
            result (StringBuilder.)
            continue? (or (nil? max-depth) (< depth max-depth))]

        ;; Root entry
        (when (zero? depth)
          (.append result (str (.getAbsolutePath dir) "\n")))

        ;; Directory entries
        (doseq [d (sort-by #(.getName %) dirs)]
          (.append result (str indent "- " (.getName d) "/\n"))
          (cond
            ;; If max-depth is set and we're at the limit, add truncation indicator
            (and max-depth (= depth max-depth))
            (.append result (str indent "  - ...\n"))

            ;; Otherwise process subdirectory if we should continue
            continue?
            (let [subtree (directory-tree (.getAbsolutePath d)
                                          :depth (inc depth)
                                          :max-depth max-depth)]
              (when (and subtree (not (map? subtree)) (not (empty? subtree)))
                ;; Indent each line of the subtree with the current indentation
                (let [subtree-lines (str/split-lines subtree)
                      indented-lines (map #(str indent "  " %) subtree-lines)]
                  (.append result (str (str/join "\n" indented-lines) "\n")))))))

        ;; File entries
        (let [sorted-files (sort-by #(.getName %) files)]
          (doseq [f sorted-files]
            (.append result (str indent "- " (.getName f) "\n"))))

        (.toString result))
      {:error (str path " is not a valid directory")})))