(ns clojure-mcp.tools.dispatch-agent.core
  "Core implementation for the dispatch agent tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.tools.logging :as log]
            [clojure-mcp.agent.langchain :as chain]
            [clojure-mcp.tools.unified-read-file.tool :as read-file-tool]
            [clojure-mcp.tools.directory-tree.tool :as directory-tree-tool]
            [clojure-mcp.tools.grep.tool :as grep-tool]
            [clojure-mcp.tools.glob-files.tool :as glob-files-tool]
            [clojure-mcp.tools.project.tool :as project-tool]
            [clojure-mcp.tools.think.tool :as think-tool]
            ;; maybe unified read-file tool
            )
  (:import
   [clojure_mcp.agent.langchain AiService]))

(declare system-message)

(defn create-ai-service
  "Creates an AI service for doings read only tasks"
  [nrepl-client-atom]
  (try
    (when-let [model (some-> (chain/agent-model)
                             (.build))]
      (let [memory (chain/chat-memory 300)
            ai-service-data {:memory memory
                             :model model
                             :tools
                             (mapv
                              #(% nrepl-client-atom)
                              [read-file-tool/unified-read-file-tool
                               directory-tree-tool/directory-tree-tool
                               grep-tool/grep-tool
                               glob-files-tool/glob-files-tool
                               ;; needs REPL setup
                               project-tool/inspect-project-tool
                               think-tool/think-tool])
                             :system-message system-message}
            service (-> (chain/create-service AiService ai-service-data)
                        (.build))]
        (assoc ai-service-data
               :service service)))
    (catch Exception e
      (log/error e "Failed to create dispatch_agent AI service for code critique")
      (throw e))))

(defn get-ai-service
  [nrepl-client-atom]
  (or (::ai-service @nrepl-client-atom)
      (when-let [ai (create-ai-service nrepl-client-atom)]
        (swap! nrepl-client-atom assoc ::ai-service ai)
        ai)))

(defn dispatch-agent
  "Dispatches an agent with the given prompt. The agent will only have access to read tools.
   Returns a string response from the agent."
  [{:keys [nrepl-client-atom]} prompt]
  (if (string/blank? prompt)
    {:critique "Error: Cannot critique empty code"
     :error true}
    (if-let [ai-service (get-ai-service nrepl-client-atom)]
      ;; TODO we have a stateful memory problem here.
      ;; parallel requests will corrupt the memory?
      ;; should use a Oneshot Memory
      (do
        (.clear (:memory ai-service))
        (let [result (.chat (:service ai-service) prompt)]
          {:result result
           :error false}))
      {:result "ERROR: No model configured for this agent."
       :error true})))

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

(comment
  (let [user-dir (System/getProperty "user.dir")
        client-atom (atom {:clojure-mcp.config/config
                           {:nrepl-user-dir user-dir
                            :allowed-directories [user-dir]
                            :emacs-notify true}})
        ai (create-ai-service client-atom)]
    (.chat (:service ai) "hey I'm looking for the langchain integration code where can I find it")))

(def system-message
  "You are an agent for a Clojure Coding Assistant. Given the user's prompt, you should use the tools available to you to answer the user's question.

Notes:
1. IMPORTANT: You should be concise, direct, and to the point, since your responses will be displayed on a command line interface. Answer the user's question directly, without elaboration, explanation, or details. One word answers are best. Avoid introductions, conclusions, and explanations. You MUST avoid text before/after your response, such as \"The answer is <answer>.\", \"Here is the content of the file...\" or \"Based on the information provided, the answer is...\" or \"Here is what I will do next...\".
2. When relevant, share file names and code snippets relevant to the query
3. Any file paths you return in your final response MUST be absolute. DO NOT use relative paths.")
