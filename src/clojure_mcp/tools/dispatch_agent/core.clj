(ns clojure-mcp.tools.dispatch-agent.core
  "Core implementation for the dispatch agent tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require [clojure.data.json :as json]
            [clojure.string :as string]))

(defn dispatch-agent
  "Dispatches an agent with the given prompt. The agent will only have access to read tools.
   Returns a string response from the agent."
  [prompt]
  ;; This is a stub that will be implemented later with actual agent functionality
  ;; For now, just return a simulated response that mimics what an agent might return
  {:result (str "Agent executed with prompt: \"" prompt "\"\n\n"
                "Results summary:\n"
                "- Located 3 files matching the search criteria\n"
                "- Found relevant content in 2 files\n"
                "- Key findings: Example finding 1, Example finding 2\n\n"
                "This is a stub implementation that will be replaced with actual agent functionality.")
   :error false})

(defn validate-dispatch-agent-inputs
  "Validates inputs for the dispatch-agent function"
  [inputs]
  (cond
    (nil? inputs)
    (throw (ex-info "Missing inputs" {:error-details ["No input parameters provided"]}))

    (nil? (:prompt inputs))
    (throw (ex-info "Missing required parameter: prompt"
                    {:error-details ["The 'prompt' parameter is required"]}))

    (not (string? (:prompt inputs)))
    (throw (ex-info "Parameter 'prompt' must be a string"
                    {:error-details [(str "Got: " (type (:prompt inputs)))]}))

    :else
    inputs))
