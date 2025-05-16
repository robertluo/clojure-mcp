(ns clojure-mcp.tools.architect.tool
  (:require [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.architect.core :as core]))

(defn create-architect-tool
  "Creates the architect tool configuration"
  [nrepl-client-atom]
  {:tool-type :architect
   :nrepl-client-atom nrepl-client-atom})

(defn architect-tool
  "Returns a tool registration for the architect tool compatible with the MCP system."
  [nrepl-client-atom]
  (tool-system/registration-map (create-architect-tool nrepl-client-atom)))

(defmethod tool-system/tool-name :architect [_]
  "architect")

(defmethod tool-system/tool-description :architect [_]
  "Your go-to tool for any technical or coding task. Analyzes requirements and breaks them down into clear, actionable implementation steps. Use this whenever you need help planning how to implement a feature, solve a technical problem, or structure your code.")

(defmethod tool-system/tool-schema :architect [_]
  {:type :object
   :properties {:prompt {:type :string
                         :description "The technical request or coding task to analyze"}
                :context {:type :string
                          :description "Optional context from previous conversation or system state"}}
   :required [:prompt]})

(defmethod tool-system/validate-inputs :architect [_ inputs]
  (core/validate-architect-inputs inputs))

(defmethod tool-system/execute-tool :architect [{:keys [nrepl-client-atom]} inputs]
  (let [result (core/architect {:nrepl-client-atom nrepl-client-atom} inputs)]
    result))

(defmethod tool-system/format-results :architect [_ {:keys [result error] :as results}]
  {:result [result]
   :error error})