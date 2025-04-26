(ns clojure-mcp.tools.code-critique.core
  "Core implementation for the code critique tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.string :as string]
   [clojure-mcp.agent.langchain :as chain]
   [clojure.tools.logging :as log])
  (:import
   [clojure_mcp.agent.langchain AiService]))

(declare system-message)

(defn create-ai-service
  "Creates an AI service for code critique using Claude model."
  []
  (try
    (let [memory (chain/chat-memory 12)
          model (-> (chain/create-model-claude-3-7)
                    (.thinkingType "enabled")
                    (.thinkingBudgetTokens (int 1024))
                    (.beta "prompt-caching-2024-07-31")
                    (.cacheSystemMessages true)
                    (.maxTokens (int 2048))
                    (.temperature 1.0)
                    (.build))
          ai-service-data {:memory memory
                           :model model
                           :system-message (system-message 2)}
          service (-> (chain/create-service AiService
                                            ai-service-data)
                      (.build))]
      (assoc ai-service-data
             :service service))
    (catch Exception e
      (log/error e "Failed to create AI service for code critique")
      (throw e))))

(defn get-ai-service
  [nrepl-client-atom]
  (or (::ai-service @nrepl-client-atom)
      (let [ai (create-ai-service)]
        (swap! nrepl-client-atom assoc ::ai-service ai)
        ai)))

(defn critique-code
  [{:keys [nrepl-client-atom]} code]
  (if (string/blank? code)
    {:critique "Error: Cannot critique empty code"
     :error true}
    (let [ai-service (get-ai-service nrepl-client-atom)]
      (let [critique (.chat (:service ai-service) code)]
        {:critique critique
         :error false}))))

(comment
  (def ai-service (create-ai-service))

  (critique-code (atom {})
                 "(defn i [x] x)"))

;; beter to read this from an text file in resources
(defn system-message [n]
  (let [nstr (if (= 1 n) "single" n)
        improvement (cond-> "improvement"
                      (> n 1) (str "s"))]
    (format
     "You are a super power Rich Hickey Clojure code advisor. Your task is to analyze code and provide feedback for improvements.

You are going to be presented with code and you job is to find the %1$s most important structural %2$s to the code. And then present a concise description of the %2$s.

When you see a loop reccomend `iterate` or `reduce` if its appropriate.
When you see nesting look for threading `some->` `->>` `->` opporutnities

Do not validate an approach, always offer the next %2$s.

Always, discourage code comments and docstrings as they are unnecessary while iterating on code and they eat up LLM tokens (expensive) and they slow down development.

Always, let based local functions are discouraged for two important reasons:
1. they are harder to test idividualy
2. AI assistants have a much easier time editing smaller functions just like humans
3. It's an opportunity to create a resuable function to share with others. 

Frequently recommend extracting functions.  For longer functions: always extract predicates that are longer than 5 lines. Extracting the step functions (if more than a few lines) for `reduce` and `iterate` is often a gread improvment as you can test them separately.

<example-response>
I think you can use `reduce` instead of a loop
</example-response>

<example-response>
This function is nested deeply you can probably extract the predicate
</example-response>

<example-response>
This function is too long please break it up into several smaller functions, that filter predicate is very long for instance.
</example-response>


<example-response>
This function is using state, probably better to use `iterate` 
</example-response>


<example-response>
* This function is nested deeply you can probably extract the predicate
* I think you can use `reduce` instead of a loop
</example-response>"
     nstr
     improvement)))

(comment
  ;; === Examples of using the code critique functionality directly ===

  ;; Critique a simple function
  (critique-code "(defn add [x y] (+ x y))")

  ;; Critique a more complex function
  (critique-code "
  (defn process-data [data]
    (let [cleaned (remove nil? data)
          processed (map #(* % 2) cleaned)
          result (reduce + processed)]
      result))
  ")

  ;; Test empty input
  (critique-code ""))
