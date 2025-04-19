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

(defmethod tool-system/format-results :clojure-eval [_ result]
  ;; Keep the same format as the existing implementation
  (cond
    (:error result) {:error (:error result)}
    :else {:result (or (:value result) "nil")}))

;; Backward compatibility function that returns the registration map
(defn eval-code [nrepl-client-atom]
  (tool-system/registration-map (create-eval-tool nrepl-client-atom)))
