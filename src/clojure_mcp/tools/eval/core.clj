(ns clojure-mcp.tools.eval.core
  "Core implementation for the eval tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.linting :as linting]
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

(comment
  ;; === Examples of using the eval core functionality directly ===
  
  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  
  ;; Test simple expression evaluation
  (evaluate-code @client-atom "(+ 1 2)")
  
  ;; Test multi-expression evaluation
  (evaluate-code @client-atom "(println \"Hello World\")\n(+ 3 4)")
  
  ;; Test evaluation with error
  (evaluate-code @client-atom "(/ 1 0)")
  
  ;; Test evaluation with linter warning
  (evaluate-code @client-atom "(let [unused 1] (+ 2 3))")
  
  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom)
)