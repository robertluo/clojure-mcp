(ns clojure-mcp.tools.eval.tool
  "Implementation of the eval tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.eval.core :as core]))

;; Factory function to create the tool configuration
(defn create-eval-tool 
  "Creates the evaluation tool configuration"
  [nrepl-client-atom]
  {:tool-type :clojure-eval
   :nrepl-client-atom nrepl-client-atom
   :timeout 30000})

;; Implement the required multimethods for the eval tool

(defmethod tool-system/tool-description :clojure-eval [_]
  "Evaluates Clojure code in the current namespace and returns the result.")

(defmethod tool-system/tool-schema :clojure-eval [_]
  {:code {:type "string"
          :description "The Clojure code to evaluate."}
   :required ["code"]})

(defmethod tool-system/validate-inputs :clojure-eval [_ inputs]
  (let [{:keys [code]} inputs]
    (when-not code
      (throw (ex-info "Missing required parameter: code" {:inputs inputs})))
    ;; Return validated inputs (could do more validation/coercion here)
    inputs))

(defmethod tool-system/execute-tool :clojure-eval [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [code]} inputs
        ;; Delegate to core implementation
        result (core/evaluate-code @nrepl-client-atom code)]
    result))

(defmethod tool-system/format-results :clojure-eval [_ {:keys [result error] :as eval-result}]
  ;; The core implementation now returns a map with :result and :error
  (if error
    {:error result}
    {:result result}))

;; Backward compatibility function that returns the registration map
(defn eval-code [nrepl-client-atom]
  (tool-system/registration-map (create-eval-tool nrepl-client-atom)))

(comment
  ;; === Examples of using the eval tool ===
  
  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  
  ;; Create a tool instance
  (def eval-tool (create-eval-tool client-atom))
  
  ;; Test the individual multimethod steps
  (def inputs {:code "(+ 1 2)"})
  (def validated (tool-system/validate-inputs eval-tool inputs))
  (def result (tool-system/execute-tool eval-tool validated))
  (def formatted (tool-system/format-results eval-tool result))
  
  ;; Generate the full registration map
  (def reg-map (tool-system/registration-map eval-tool))
  
  ;; Test running the handler function directly
  (def handler-fn (:handler reg-map))
  (handler-fn {"code" "(+ 1 2)"} (fn [result] (println "Result:" result)))
  
  ;; Make a simpler test function that works like tool-fn
  (defn test-tool [code]
    (let [prom (promise)]
      (handler-fn {"code" code}
                  (fn [result] (deliver prom result)))
      @prom))
  
  ;; Test with simple expressions
  (test-tool "(+ 1 2)")
  (test-tool "(println \"Hello\")\n(+ 3 4)")
  (test-tool "(/ 1 0)")
  
  ;; Clean up
  (clojure-mcp.nrepl/stop-polling @client-atom)
)
