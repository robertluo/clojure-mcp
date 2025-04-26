(ns clojure-mcp.tools.code-critique.tool
  "Implementation of the code critique tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.code-critique.core :as core]
   [clojure-mcp.linting :as linting]))

;; Factory function to create the tool configuration
(defn create-code-critique-tool
  "Creates the code critique tool configuration"
  [nrepl-client-atom]
  {:tool-type :code-critique
   :nrepl-client-atom nrepl-client-atom})

#_(core/critique-code (create-code-critique-tool nil)
                      "(defn a a)")

;; Implement the required multimethods for the code critique tool
(defmethod tool-system/tool-name :code-critique [_]
  "code_critique")

(defmethod tool-system/tool-description :code-critique [_]
  "Starts an interactive code review conversation that provides constructive feedback on your Clojure code.
  
  HOW TO USE THIS TOOL:
  1. Submit your Clojure code for initial critique
  2. Review the suggestions and implement improvements
  3. Test your revised code in the REPL
  4. Submit the updated code for additional feedback
  5. Continue this cycle until the critique is satisfied
  
  This tool initiates a feedback loop where you:
  - Receive detailed analysis of your code
  - Implement suggested improvements
  - Test and verify changes
  - Get follow-up critique on your revisions
  
  The critique examines:
  - Adherence to Clojure style conventions
  - Functional programming best practices
  - Performance optimizations
  - Readability and maintainability improvements
  - Idiomatic Clojure patterns
  
  Example conversation flow:
  - You: Submit initial function implementation
  - Tool: Provides feedback on style and structure
  - You: Revise code based on suggestions and test in REPL
  - Tool: Reviews updates and suggests further refinements
  - Repeat until code quality goals are achieved
  
  Perfect for iterative learning and continuous code improvement.")

;; TODO file-path and optional symbol 
(defmethod tool-system/tool-schema :code-critique [_]
  {:type :object
   :properties {:code {:type :string
                       :description "The Clojure code to analyze and critique"}}
   :required [:code]})

(defmethod tool-system/validate-inputs :code-critique [_ inputs]
  (let [{:keys [code]} inputs]
    (when-not code
      (throw (ex-info "Missing required parameter: code"
                      {:inputs inputs})))

    ;; Perform linting to check if code is valid Clojure
    (let [lint-result (linting/lint code)]
      ;; If there are critical errors in the code, reject it
      (when (and lint-result (:error? lint-result))
        (throw (ex-info (str "Syntax errors detected in Clojure code:\n"
                             (:report lint-result)
                             "\nPlease fix the syntax errors before saving.")
                        {:inputs inputs
                         :error-details (:report lint-result)}))))
    ;; Return validated inputs
    inputs))

(defmethod tool-system/execute-tool :code-critique [tool {:keys [code]}]
  (core/critique-code tool code))

(defmethod tool-system/format-results :code-critique [_ {:keys [critique error]}]
  {:result [critique] :error error})

;; Function that returns the registration map
(defn code-critique-tool [nrepl-client-atom]
  (tool-system/registration-map (create-code-critique-tool nrepl-client-atom)))

(comment
  ;; === Examples of using the code critique tool ===

  ;; Setup for REPL-based testing
  (require '[clojure-mcp.nrepl :as nrepl])
  (def client-atom (atom (nrepl/create {:port 7888})))
  (nrepl/start-polling @client-atom)

  ;; Create a tool instance
  (def critique-tool (create-code-critique-tool client-atom))

  ;; Test the individual multimethod steps
  (def inputs {:code "(defn add [x y] (+ x y))"})
  (def validated (tool-system/validate-inputs critique-tool inputs))
  (def result (tool-system/execute-tool critique-tool validated))
  (def formatted (tool-system/format-results critique-tool result))

  ;; Generate the full registration map
  (def reg-map (tool-system/registration-map critique-tool))

  ;; Test running the tool-fn directly
  (def tool-fn (:tool-fn reg-map))
  (tool-fn nil {"code" "(defn add [x y] (+ x y))"}
           (fn [result error] (println "Result:" result "Error:" error)))
  (tool-fn nil {"code" ""}
           (fn [result error] (println "Result:" result "Error:" error)))

  ;; Make a simpler test function that works like tool-fn
  (defn test-tool [code]
    (let [prom (promise)]
      (tool-fn nil {"code" code}
               (fn [result error]
                 (deliver prom (if error {:error error} {:result result}))))
      @prom))

  ;; Test with various code samples
  (test-tool "(defn add [x y] (+ x y))")
  (test-tool "(let [x 1 y 2] (+ x y))")
  (test-tool "") ;; Should trigger error handling

  ;; Clean up
  (nrepl/stop-polling @client-atom))
