(ns clojure-mcp.tools.scratch-pad.truncate
  "Utilities for truncating and pretty-printing deeply nested data structures."
  (:require [clojure.pprint :as pprint]))

(defn- truncate-at-max-depth
  [x ellipsis]
  (cond
    (record? x) ellipsis
    (map? x) (into (empty x) (map (fn [[k _]] [k ellipsis]) x))
    (sequential? x) (into (empty x) (repeat (count x) ellipsis))
    (set? x) #{ellipsis}
    :else x))

(defn- recurse-structure
  [x depth truncate-fn]
  (cond
    (record? x) (into {}
                      (map (fn [[k v]] [k (truncate-fn v (inc depth))]) x))
    (map? x) (into (empty x)
                   (map (fn [[k v]] [k (truncate-fn v (inc depth))]) x))
    (vector? x) (mapv #(truncate-fn % (inc depth)) x)
    (list? x) (apply list (map #(truncate-fn % (inc depth)) x))
    (sequential? x) (into (empty x)
                          (map #(truncate-fn % (inc depth)) x))
    (set? x) (into (empty x)
                   (map #(truncate-fn % (inc depth)) x))
    :else x))

(defn truncate-recursive
  [x depth max-depth ellipsis]
  (cond
    (> depth max-depth) ellipsis
    (= depth max-depth) (truncate-at-max-depth x ellipsis)
    :else (recurse-structure x depth
                             #(truncate-recursive %1 %2 max-depth ellipsis))))

(defn truncate-depth
  "Truncates a data structure at the specified depth, replacing deeper values with ellipsis.
   
   Parameters:
   - data: The data structure to truncate
   - max-depth: Maximum depth to display (0-based)
   - ellipsis: String to show for truncated values (default: '...')
   
   Examples:
   (truncate-depth {:a {:b {:c 1}}} 1)
   ;; => {:a {:b \"...\"}}
   
   (truncate-depth {:a {:b {:c 1}}} 2) 
   ;; => {:a {:b {:c \"...\"}}}
   
   At the max depth level:
   - Maps show all keys with ellipsis values
   - Sequences show same length with ellipsis elements
   - Sets become #{ellipsis}
   - Records become ellipsis
   
   Special handling:
   - Preserves collection types (sorted maps remain sorted)
   - Lists maintain order
   - Records are converted to maps when truncated partially"
  ([data max-depth]
   (truncate-depth data max-depth "..."))
  ([data max-depth ellipsis]
   (truncate-recursive data 0 max-depth ellipsis)))

(defn pprint-truncated
  "Pretty prints data truncated at the specified depth.
   Returns the pretty-printed string."
  [data max-depth]
  (with-out-str
    (pprint/pprint (truncate-depth data max-depth))))
