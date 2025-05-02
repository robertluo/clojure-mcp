(ns clojure-mcp.sexp.match
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as n]
            [rewrite-clj.parser :as p]))

(defn match-sexpr
  "Return true if `pattern` matches `data`. 
   Wildcards in `pattern`:
     - `?` consumes exactly one form
     - `*` consumes zero or more forms, but if there are more pattern elements
       after it, it will try to align them with the tail of `data`."
  [pattern data]
  (prn :HERE pattern data)
  (cond
    ;; both are sequences ⇒ walk with possible '*' backtracking
    (and (sequential? pattern) (sequential? data))
    (letfn [(match-seq [ps ds]
              (cond
                ;; pattern exhausted ⇒ only match if data also exhausted
                (empty? ps)
                (empty? ds)

                ;; '?' ⇒ must have at least one ds, then consume exactly one
                (= (first ps) '?)
                (and (seq ds)
                     (recur (rest ps) (rest ds)))

                ;; '*' ⇒ two cases:
                ;; 1) no more pattern ⇒ matches anything
                ;; 2) with remaining pattern, try every split point
                (= (first ps) '*)
                (let [ps-rest (rest ps)]
                  (if (empty? ps-rest)
                    ;; try k = 0..(count ds): drop k elements, then match the rest
                    (some true?
                          (for [k (range (inc (count ds)))]
                            (match-seq ps-rest (drop k ds))))))

                ;; nested list/vector ⇒ recurse
                (and (sequential? (first ps))
                     (sequential? (first ds)))
                (and (match-sexpr (first ps) (first ds))
                     (recur (rest ps) (rest ds)))

                ;; literal equality
                :else
                (and (= (first ps) (first ds))
                     (recur (rest ps) (rest ds)))))]
      (match-seq pattern data))
    (= pattern '?) true
    (= pattern '*) true 
    ;; atoms ⇒ direct equality
    :else
    (= pattern data)))

(defn find-match*
  [pattern-sexpr zloc]
  (loop [loc zloc]
    (when-not (z/end? loc)
      (prn :NOT_END )
      (let [form (try (z/sexpr loc)
                      (catch Exception e
                        ::continue))]
        (if (= ::continue form)
          (do (prn :BAD_FORM form)
              (recur (z/next loc)))
          (if (match-sexpr pattern-sexpr form)
            loc
            (recur (z/next loc))))))))

(defn find-match [pattern-str code-str]
  (find-match* (z/sexpr (z/of-string pattern-str))
               (z/of-string code-str)))

#_(z/string (find-match "(z/sexpr ?)" (slurp "src/clojure_mcp/sexp/match.clj")))






