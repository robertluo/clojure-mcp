(ns clojure-mcp.tools.scratch-pad.truncate
  "Utilities for truncating and pretty-printing deeply nested data structures."
  (:require [clojure.pprint :as pprint]))

(defn- make-size-symbol
  "Creates a symbol for size indicators like '...10_elements"
  [size unit]
  (symbol (str "..." size "_" unit)))

(defn- truncate-at-max-depth
  [x ellipsis opts]
  (let [{:keys [show-size show-first]
         :or {show-size true show-first 3}} opts]
    (cond
      (record? x) ellipsis

      (map? x)
      (if show-size
        (let [size (count x)]
          (if (<= size show-first)
            (into (empty x) (map (fn [[k _]] [k ellipsis]) x))
            (into (empty x)
                  (concat (take show-first (map (fn [[k _]] [k ellipsis]) x))
                          [[ellipsis (make-size-symbol size "entries")]]))))
        (into (empty x) (map (fn [[k _]] [k ellipsis]) x)))

      (sequential? x)
      (if show-size
        (let [size (count x)]
          (cond
            (empty? x) (empty x)
            (<= size show-first) (into (empty x) (repeat size ellipsis))
            :else (into (empty x)
                        (concat (repeat show-first ellipsis)
                                [(make-size-symbol size "elements")]))))
        (into (empty x) (repeat (count x) ellipsis)))

      (set? x)
      (cond
        (empty? x) #{}
        (and show-size (> (count x) show-first))
        (conj (into #{} (repeat show-first ellipsis))
              (make-size-symbol (count x) "items"))
        :else #{ellipsis})

      :else x)))

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
  [x depth max-depth ellipsis opts]
  (cond
    (> depth max-depth) ellipsis
    (= depth max-depth) (truncate-at-max-depth x ellipsis opts)
    :else (recurse-structure x depth
                             #(truncate-recursive %1 %2 max-depth ellipsis opts))))

(defn truncate-depth
  "Truncates a data structure at the specified depth, replacing deeper values with ellipsis.
   
   Parameters:
   - data: The data structure to truncate
   - max-depth: Maximum depth to display (0-based)
   - opts: Optional map with:
     :ellipsis - Symbol to show for truncated values (default: '...)
     :show-size - Show collection sizes (default: true)
     :show-first - Number of items to show before size indicator (default: 3)
   
   Examples:
   (truncate-depth {:a {:b {:c 1}}} 1)
   ;; => {:a {:b '...}}
   
   (truncate-depth (vec (range 10)) 0)
   ;; => ['... '... '... '...10_elements]
   
   (truncate-depth {:a (vec (range 5))} 1 {:show-first 2})
   ;; => {:a ['... '... '...5_elements]}
   
   At the max depth level:
   - Maps show first N keys then '...N_entries
   - Sequences show first N items then '...N_elements
   - Sets show first N items then '...N_items
   - Records become ellipsis
   
   Special handling:
   - Preserves collection types (sorted maps remain sorted)
   - Lists maintain order
   - Records are converted to maps when truncated partially
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
