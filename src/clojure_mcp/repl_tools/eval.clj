(ns clojure-mcp.repl-tools.eval
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.linting :as linting]
   [clojure.data.json :as json]
   [clojure.string :as string]))

;; Eval history helpers
(defn eval-history-push
  "Pushes a form string to the evaluation history.
   Safely handles nil state-atom."
  [state-atom form-str]
  (when state-atom
    (swap! state-atom update ::eval-history conj form-str)))

(defn eval-history-reset 
  "Resets the evaluation history.
   Safely handles nil state-atom."
  [state-atom]
  (when state-atom
    (swap! state-atom assoc ::eval-history nil)))

;; Eval results formatting
;; The goal is to make it clear for the LLM to understand
(defn partition-outputs [outputs]
  (when (not-empty outputs)
    (let [[non-val-parts [val & xs]] (split-with #(not= :value (first %)) outputs)]
      (cons (cond-> (vec non-val-parts)
              val (conj val))
            (partition-outputs xs)))))

(defn format-value [[k v]]
  (string/trim-newline
   (if (= k :value)
     (str "=> " v (if (<= nrepl/truncation-length (count v))
                    " ... RESULT TRUNCATED"
                    ""))
     v)))

(defn format-eval-outputs [outputs]
  (->> outputs
       (map format-value)
       (string/join "\n")))

(defn partition-and-format-outputs [outputs]
  (interpose "*===============================================*"
             (mapv format-eval-outputs (partition-outputs outputs))))

(defn eval-code [service-atom]
  {:name "clojure_eval"
   :description "Takes a Clojure Expression and evaluates it in the current namespace. For example, providing \"(+ 1 2)\" will evaluate to 3. 

This tool is intended to execute Clojure code. This is very helpful for verifying that code is working as expected. It's also helpful for REPL driven development.

If you send multiple expressions they will all be evaluated individually and their output will be clearly partitioned.

If the value that is returned is too long it will be truncated."
   :schema (json/write-str {:type :object
                            :properties {:expression {:type :string}}
                            :required [:expression]})
   :tool-fn (fn tool-fn
              [_ arg-map clj-result-k]
              (let [outputs (atom []) ;; Atom to store prefixed output strings
                    error-occurred (atom false) ;; Atom to track if any error happened
                    form-str (get arg-map "expression")
                    linted (linting/lint form-str)
                    add-output! (fn [prefix value] (swap! outputs conj [prefix value]))
                    finish (fn [_]
                             (clj-result-k
                              (partition-and-format-outputs @outputs)
                              @error-occurred))]

                ;; Add linter output if present
                (when linted
                  (add-output! :lint (:report linted))
                  (when (:error? linted)
                    (reset! error-occurred true)))

                ;; If linter found critical errors or service not available, finish early
                (if @error-occurred
                  (finish nil)
                  (do
                    (eval-history-push (::nrepl/state @service-atom) form-str)
                    (nrepl/eval-code-help @service-atom form-str ;; Dereference the atom
                                          (->> identity
                                               (nrepl/out-err
                                                #(add-output! :out %)
                                                #(add-output! :err %))
                                               ;; TODO we need to limit the size here
                                               (nrepl/value #(add-output! :value %))
                                               (nrepl/done finish)
                                               (nrepl/error (fn [_]
                                                              (reset! error-occurred true)
                                                              ;; Optionally add an error marker to output
                                                              (add-output! "ERROR: " "Evaluation failed")
                                                              (finish _)))))))
                ))})