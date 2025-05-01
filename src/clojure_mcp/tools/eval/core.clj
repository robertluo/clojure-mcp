(ns clojure-mcp.tools.eval.core
  "Core implementation for the eval tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.linting :as linting]
   [clojure-mcp.sexp.paren-utils :as paren-utils]
   [clojure.string :as string]))

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

(defn repair-code
  [code]
  (let [linted (linting/lint code)]
    (if (and linted (:error? linted) (paren-utils/has-delimiter-errors? linted))
      (or (paren-utils/parinfer-repair code) code)
      code)))

(defn evaluate-code
  "Evaluates Clojure code using the nREPL client.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - code: The Clojure code to evaluate as a string
   
   Returns:
   - A map with :outputs (raw outputs), :error (boolean flag)"
  [nrepl-client code]
  (let [outputs (atom []) ;; Atom to store prefixed output strings
        error-occurred (atom false) ;; Atom to track if any error happened
        form-str code
        linted (linting/lint form-str)
        add-output! (fn [prefix value] (swap! outputs conj [prefix value]))
        result-promise (promise)]

    ;; Add linter output if present
    (when linted
      (add-output! :lint (:report linted))
      (when (:error? linted)
        (reset! error-occurred true)))

    ;; If linter found critical errors, return early
    (if @error-occurred
      {:outputs @outputs
       :error true}

      ;; Otherwise, evaluate the code
      (do
        ;; Push to eval history if available
        (when-let [state (::nrepl/state nrepl-client)]
          (swap! state update :clojure-mcp.repl-tools/eval-history conj form-str))

        ;; Evaluate the code
        (nrepl/eval-code-help nrepl-client form-str
                              (->> identity
                                   (nrepl/out-err
                                    #(add-output! :out %)
                                    #(add-output! :err %))
                                   (nrepl/value #(add-output! :value %))
                                   (nrepl/done (fn [_]
                                                 (deliver result-promise
                                                          {:outputs @outputs
                                                           :error @error-occurred})))
                                   (nrepl/error (fn [_]
                                                  (reset! error-occurred true)
                                                  (add-output! :err "Evaluation failed")
                                                  (deliver result-promise
                                                           {:outputs @outputs
                                                            :error true})))))

        ;; Wait for the result and return it
        @result-promise))))

(defn evaluate-with-repair
  "Evaluates Clojure code with automatic repair of delimiter errors.
   First attempts to repair any delimiter errors in the code, 
   then evaluates the repaired code if successful.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - code: The Clojure code to evaluate as a string
   
   Returns:
   - A map with :outputs (raw outputs), :error (boolean flag)"
  [nrepl-client code]
  (evaluate-code nrepl-client (repair-code code)))

(comment
  ;; === Examples of using the eval core functionality directly ===

  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Test simple expression evaluation with original function
  (evaluate-code @client-atom "(+ 1 2)")

  ;; Test multi-expression evaluation with original function
  (evaluate-code @client-atom "(println \"Hello World\")\n(+ 3 4)")

  ;; Test evaluation with error with original function
  (evaluate-code @client-atom "(/ 1 0)")

  ;; Test evaluation with linter warning with original function
  (evaluate-code @client-atom "(let [unused 1] (+ 2 3))")

  ;; Test the repair-code function directly
  (repair-code "(defn hello [name] (println name)") ;; Missing closing paren
  (repair-code "(defn hello [name] (println name)))") ;; Extra closing paren
  (repair-code "(defn hello [123] (println name))") ;; Non-repairable syntax error
  (repair-code "(defn hello [name] (println \"Hello))") ;; Non-repairable string error

  ;; Test the evaluate-with-repair function
  (evaluate-with-repair @client-atom "(+ 1 2)") ;; Well-formed code

  ;; Should auto-repair and evaluate successfully
  (evaluate-with-repair @client-atom "(defn hello [name] (println name)")
  (evaluate-with-repair @client-atom "(defn hello [name] (println name)))")

  ;; Should detect non-repairable errors
  (evaluate-with-repair @client-atom "(defn hello [123] (println name))")
  (evaluate-with-repair @client-atom "(defn hello [name] (println \"Hello))")

  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom))
