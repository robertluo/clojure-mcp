(ns clojure-mcp.tools.eval.core
  "Core implementation for the eval tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.repl-tools.eval :as existing-eval]))

(defn evaluate-code
  "Evaluates Clojure code using the nREPL client.
   Returns a map with either :value or :error."
  [nrepl-client code]
  ;; Reuse the existing implementation for now
  ;; In a real refactoring, we might move the core functionality here
  (existing-eval/evaluate-code nrepl-client code))