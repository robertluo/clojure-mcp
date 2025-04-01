(ns clojure-mcp.repl-tools.function-edit
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]))

#_(defn has-name? [sexp nm]
    (->> sexp
         rest
         (take-while (complement sequential?))
         (filter #(= nm %))
         first))

(defn is-top-def-name? [sexp tag dname]
  (and (list? sexp)
       (= (first sexp) tag)
       (= (second sexp) dname)))

(defn find-toplevel-def [zloc tag dname]
  (some->> sample
           z/of-string
           (iterate z/right)
           (take-while not-empty)
           (filter #(is-top-def-name? (z/sexpr %) tag dname))
           first))

(defn replace-top-level-def* [zloc tag name replacement-str]
  (when-let [zloc (find-toplevel-def zloc tag name)]
    (z/edit zloc (fn [_] replacement-str))))

(defn replace-top-level-def [zloc name replacment-str]
  (let [tag (first (z/sexpr (z/of-string replacment-str)))]
    (replace-top-level-def* zloc tag name replacment-str)))

(comment
  ;; sexp ignores meta-data so we should be good with simple detection
  (-> (z/of-string "(defn ^Thing nnnnn [] ())")
      (z/sexpr))
  
  (def sample "(defn foo [x y]
    (println \"Original foo was called!\")
    (+ x y (helper 5)))

(defn bar     [] true)")
  
  (z/root-string (replace-top-level-def (z/of-string sample) 'bar "(defn fooby [] 'yep)"))
  
  (find-toplevel-def (z/of-string sample) 'defn 'foo)
  (find-toplevel-defn-zipper (z/of-string my-code) "foo")

  
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

  )


