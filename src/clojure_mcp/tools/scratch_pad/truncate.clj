(ns clojure-mcp.tools.scratch-pad.truncate
  "Utilities for truncating and pretty-printing deeply nested data structures."
  (:require [clojure.pprint :as pprint]))

(defn- make-size-symbol
  "Creates a symbol for size indicators like '...10_elements"
  [size unit]
  (symbol (str "..." size "_" unit)))

(defn truncate-recursive
  [x depth max-depth ellipsis opts]
  (cond
    (> depth max-depth) ellipsis

    (= depth max-depth)
    (cond
      (record? x) ellipsis

      (map? x)
      (if (:show-size opts true)
        (into (empty x) [[ellipsis (make-size-symbol (count x) "entries")]])
        ellipsis)

      (sequential? x)
      (if (and (:show-size opts true) (seq x))
        (into (empty x) [(make-size-symbol (count x) "elements")])
        ellipsis)

      (set? x)
      (if (and (:show-size opts true) (seq x))
        #{(make-size-symbol (count x) "items")}
        #{ellipsis})

      :else ellipsis)

    :else
    (cond
      (or (record? x) (map? x))
      (into (empty x)
            (map (fn [[k v]]
                   [k (if (not (or (map? v) (sequential? v) (set? v) (record? v)))
                        ellipsis
                        (truncate-recursive v (inc depth) max-depth ellipsis opts))])
                 x))

      (sequential? x)
      (into (empty x)
            (map #(if (not (or (map? %) (sequential? %) (set? %) (record? %)))
                    ellipsis
                    (truncate-recursive % (inc depth) max-depth ellipsis opts))
                 x))

      (set? x)
      (into (empty x)
            (map #(if (not (or (map? %) (sequential? %) (set? %) (record? %)))
                    ellipsis
                    (truncate-recursive % (inc depth) max-depth ellipsis opts))
                 x))

      :else ellipsis)))

(defn truncate-depth
  "Truncates a data structure at the specified depth, replacing values with ellipsis.
   Creates a structural overview without showing actual leaf values.
   
   Parameters:
   - data: The data structure to truncate
   - max-depth: Maximum depth to display (0-based)
   - opts: Optional map with:
     :ellipsis - Symbol to show for truncated values (default: '...)
     :show-size - Show collection sizes (default: true)
   
   Examples:
   (truncate-depth {:a {:b {:c 1}}} 1)
   ;; => {:a {:b '...}}
   
   (truncate-depth {:a 1 :b 2} 0)
   ;; => {'... '...2_entries}
   
   (truncate-depth [1 2 3 4 5] 0)
   ;; => ['...5_elements]
   
   At any depth:
   - Leaf values (strings, numbers, keywords, etc.) become ellipsis
   - Maps show keys with ellipsis values
   - Collections show structure with ellipsis for elements
   
   At max depth:
   - Maps show as {'... '...N_entries}
   - Sequences show as ['...N_elements]
   - Sets show as #{'...N_items}
   - Empty collections remain empty"
  ([data max-depth]
   (truncate-depth data max-depth {}))
  ([data max-depth opts]
   (let [ellipsis (get opts :ellipsis '...)]
     (truncate-recursive data 0 max-depth ellipsis opts))))

(defn pprint-truncated
  "Pretty prints data truncated at the specified depth.
   Returns the pretty-printed string.
   
   Parameters:
   - data: The data structure to truncate and print
   - max-depth: Maximum depth to display
   - opts: Same options as truncate-depth"
  ([data max-depth]
   (pprint-truncated data max-depth {}))
  ([data max-depth opts]
   (with-out-str
     (pprint/pprint (truncate-depth data max-depth opts)))))
