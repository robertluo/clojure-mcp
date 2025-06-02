(ns clojure-mcp.tools.scratch-pad.core
  "Core functionality for the scratch pad tool - a persistent storage for structured data."
  (:require
   [clojure.edn :as edn]))

(defn parse-value
  "Parses a value, attempting EDN first, falling back to string.
   
   Returns the parsed value."
  [value]
  (if (string? value)
    (try
      (let [parsed (edn/read-string value)]
        ;; If EDN parsed it as a symbol and the original doesn't start with :
        ;; then it was probably meant to be a string
        (if (and (symbol? parsed)
                 (not (re-matches #"^:.*" value)))
          value
          parsed))
      (catch Exception _
        ;; If EDN parsing fails, treat as plain string
        value))
    value))

(defn parse-path-element
  "Parses a single path element, attempting EDN parsing for atomic values.
   Only accepts numbers, keywords, strings, and symbols as valid keys.
   
   Returns the parsed atomic value or the original string."
  [element]
  (if (string? element)
    (try
      (let [parsed (edn/read-string element)]
        (cond
          ;; Accept numbers, keywords
          (or (number? parsed) (keyword? parsed)) parsed

          ;; For symbols, only accept if it starts with : (was meant to be keyword)
          ;; Otherwise treat as string
          (symbol? parsed) (if (re-matches #"^:.*" element)
                             parsed
                             element)

          ;; Strings need to be explicitly quoted in EDN
          (string? parsed) parsed

          ;; Reject collections and other non-atomic values
          :else element))
      (catch Exception _
        ;; If EDN parsing fails, treat as plain string
        element))
    element))

(defn assoc-in-data
  "Associates a value at the given path in the data map.
   
   Parameters:
   - data: The current data map
   - path: Vector of keys to navigate to the location
   - value: The value to store (will be parsed if string)
   
   Returns the updated data map."
  [data path value]
  (assoc-in data path (parse-value value)))

(defn get-in-data
  "Gets the value at the given path in the data map.
   
   Parameters:
   - data: The current data map
   - path: Vector of keys to navigate to the location
   
   Returns the value at the path or nil if not found."
  [data path]
  (get-in data path))

(defn dissoc-in-data
  "Removes the value at the given path in the data map.
   
   Parameters:
   - data: The current data map
   - path: Vector of keys to navigate to the location
   
   Returns the updated data map."
  [data path]
  (if (empty? path)
    data
    (if (= 1 (count path))
      (dissoc data (first path))
      (update-in data (butlast path) dissoc (last path)))))

(defn tree-view
  "Generates a tree view of the data structure.
   
   Parameters:
   - data: The data map to display
   - max-depth: Maximum depth to display (optional, defaults to 10)
   
   Returns a string representation of the tree."
  ([data] (tree-view data 10))
  ([data max-depth]
   (letfn [(render-tree [node prefix depth]
             (if (or (>= depth max-depth) (not (map? node)))
               (str prefix (pr-str node) "\n")
               (let [entries (seq node)
                     lines (map-indexed
                            (fn [idx [k v]]
                              (let [is-last (= idx (dec (count entries)))
                                    branch (if is-last "└── " "├── ")
                                    continuation (if is-last "    " "│   ")]
                                (str prefix branch k "\n"
                                     (render-tree v (str prefix continuation) (inc depth)))))
                            entries)]
                 (apply str lines))))]
     (if (empty? data)
       "Empty scratch pad"
       (render-tree data "" 0)))))
