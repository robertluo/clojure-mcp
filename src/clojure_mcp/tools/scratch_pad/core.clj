(ns clojure-mcp.tools.scratch-pad.core
  "Core functionality for the scratch pad tool - a persistent storage for structured data."
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]))

(defn parse-value
  "DEPRECATED: This function is kept for backward compatibility only.
   New code should work with JSON values directly.
   
   Previously parsed EDN values, now values come pre-parsed from JSON."
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

(defn assoc-in-data
  "Associates a value at the given path in the data map.
   Values are stored directly as received from JSON (already converted to Clojure collections).
   
   Parameters:
   - data: The current data map
   - path: Vector of keys to navigate to the location
   - value: The value to store (any JSON-compatible value)
   
   Returns the updated data map."
  [data path value]
  (assoc-in data path value))

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
  "Returns a pretty-printed string representation of the data structure.
   
   Parameters:
   - data: The data map to display
   
   Returns a pretty-printed string of the data."
  [data]
  (if (empty? data)
    "Empty scratch pad"
    (with-out-str (pprint/pprint data))))
