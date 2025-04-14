(ns clojure-mcp.core
  (:require [clojure.data.json :as json]
            [clojure-mcp.nrepl :as nrepl]
            [clojure-mcp.repl-tools :as repl-tools]
            [clojure-mcp.prompts :as prompts]
            #_[clojure-mcp.emacs-tools-enhanced.file-api :as file-api])
  (:gen-class)
  (:import [io.modelcontextprotocol.server.transport StdioServerTransportProvider]
           [io.modelcontextprotocol.server McpServer McpServerFeatures
            McpServerFeatures$AsyncToolSpecification]
           [io.modelcontextprotocol.spec
            McpSchema$ServerCapabilities
            McpSchema$Tool
            McpSchema$CallToolResult
            McpSchema$TextContent
            McpSchema$Prompt
            McpSchema$PromptArgument
            McpSchema$GetPromptRequest
            McpSchema$GetPromptResult
            McpSchema$PromptMessage
            McpSchema$Role
            McpSchema$LoggingLevel]
           [io.modelcontextprotocol.server McpServer McpServerFeatures
            McpServerFeatures$AsyncToolSpecification
            McpServerFeatures$AsyncPromptSpecification]
           [reactor.core.publisher Mono]
           [com.fasterxml.jackson.databind ObjectMapper]))

(def nrepl-client-atom (atom nil))

(defn ensure-service-atom
  "Ensures the service-atom is not nil and returns a tuple of [valid? error-message]
   where valid? is true if the atom exists and error-message is nil in that case.
   If the atom is nil, valid? is false and error-message contains an error description."
  [service-atom]
  (if (nil? service-atom)
    [false "REPL service connection is not available"]
    (if (nil? @service-atom)
      [false "REPL client not initialized"]
      [true nil])))

#_(add-tap (fn [x]
             (spit "mcp.log" (prn-str x) :append true)))

(defn create-mono-from-callback
  "Creates a function that takes the exchange and the arguments map and
  returns a Mono promise The callback function should take three
  arguments: 
   - exchange: The MCP exchange object 
   - arguments: The arguments map sent in the request 
   - continuation: A function that will be called with the result and will fullfill the promise"
  [callback-fn]
  (fn [exchange arguments]
    (Mono/create
     (reify java.util.function.Consumer
       (accept [this sink]
         (callback-fn
          exchange
          arguments
          (fn [result]
            (.success sink result))))))))

(defn ^McpSchema$CallToolResult adapt-result [list-str error?]
  (McpSchema$CallToolResult.
   (mapv #(McpSchema$TextContent. %) list-str)
   error?))

(defn create-async-tool
  "Creates an AsyncToolSpecification with the given parameters.
   
   Takes a map with the following keys:
    :name         - The name of the tool
    :description  - A description of what the tool does
    :schema       - JSON schema for the tool's input parameters
    :service-atom - The atom holding the nREPL client connection.
    :tool-fn      - Function that implements the tool's logic.
                    Signature: (fn [exchange args-map nrepl-client clj-result-k] ... )
                      * exchange     - ignored (or used for advanced features)
                      * arg-map      - map with string keys representing the mcp tool call args
                      * nrepl-client - the validated and dereferenced nREPL client
                      * clj-result-k - continuation fn taking vector of strings and boolean error flag."
  [{:keys [name description schema tool-fn]}]
  (let [mono-fn (create-mono-from-callback
                 (fn [exchange arg-map mono-fill-k] ;; The mono wrapper still gets exchange and args
                   (let [[service-valid? service-error] (ensure-service-atom nrepl-client-atom)]
                     (if-not service-valid?
                       ;; If service is invalid, immediately fill the Mono with an error result
                       (mono-fill-k (adapt-result [service-error] true))
                       ;; If service is valid, proceed with the actual tool function
                       (let [nrepl-client @nrepl-client-atom ;; Dereference the validated atom
                             clj-result-k (fn [res-list error?] ;; Define the final continuation
                                            (mono-fill-k (adapt-result res-list error?)))]
                         ;; Call the tool's function with the nrepl-client and the continuation
                         (tool-fn exchange arg-map clj-result-k))))))]
    (McpServerFeatures$AsyncToolSpecification.
     (McpSchema$Tool. name description schema)
     (reify java.util.function.BiFunction
       (apply [this exchange arguments]
         (mono-fn exchange arguments))))))

(defn ^McpSchema$GetPromptResult adapt-prompt-result
  "Adapts a Clojure prompt result map into an McpSchema$GetPromptResult.
   Expects a map like {:description \"...\" :messages [{:role :user :content \"...\"}]}"
  [{:keys [description messages]}]
  (let [mcp-messages (mapv (fn [{:keys [role content]}]
                             (McpSchema$PromptMessage.
                              (case role ;; Convert keyword role to McpSchema$Role enum
                                ;; :system McpSchema$Role/SYSTEM
                                :user McpSchema$Role/USER
                                :assistant McpSchema$Role/ASSISTANT
                                ;; Add other roles if needed
                                )
                              (McpSchema$TextContent. content))) ;; Assuming TextContent for now
                           messages)]
    (McpSchema$GetPromptResult. description mcp-messages)))

(defn create-async-prompt
  "Creates an AsyncPromptSpecification with the given parameters.
   
   Takes a map with the following keys:
    :name        - The name (ID) of the prompt
    :description - A description of the prompt
    :arguments   - A vector of maps, each defining an argument:
                   {:name \"arg-name\" :description \"...\" :required? true/false}
    :prompt-fn   - Function that implements the prompt logic.
                   Signature: (fn [exchange request-args clj-result-k] ... )
                     * exchange - The MCP exchange object
                     * request-args - Map of arguments provided in the client request
                     * clj-result-k - Continuation fn taking one map argument:
                                      {:description \"...\" :messages [{:role :user :content \"...\"}]} "
  [{:keys [name description arguments prompt-fn]}]
  (let [mcp-args (mapv (fn [{:keys [name description required?]}]
                         (McpSchema$PromptArgument. name description required?))
                       arguments)
        mcp-prompt (McpSchema$Prompt. name description mcp-args)
        mono-fn (create-mono-from-callback ;; Reuse the existing helper
                 (fn [_ request mono-fill-k]
                   ;; The request object has an .arguments() method
                   (let [request-args (.arguments ^McpSchema$GetPromptRequest request)] ;; <-- Corrected method call
                     (prompt-fn _ request-args
                                (fn [clj-result-map]
                                  (mono-fill-k (adapt-prompt-result clj-result-map)))))))]
    (McpServerFeatures$AsyncPromptSpecification.
     mcp-prompt
     (reify java.util.function.BiFunction
       (apply [this exchange request]
         (mono-fn exchange request))))))

(defn add-tool
  "Helper function to create an async tool from a map and add it to the server."
  [mcp-server  tool-map]
  (.removeTool mcp-server (:name tool-map))
  ;; Pass the service-atom along when creating the tool
  (-> (.addTool mcp-server (create-async-tool tool-map))
      (.subscribe)))

;; helper tool to demonstrate how all this gets hooked together

(def echo-tool
  {:name "echo"
   :description "Echos back the arguments"
   :schema (json/write-str {:type :object
                            :properties {:firstarg {:type :string}}
                            :required []})
   ;; The tool-fn processes the request and passes the clojure
   ;; result to the clj-result-k continuation
   ;; Arguments 
   ;; * exchange - ignored
   ;; * arg-map - map with string keys representing the mcp tool call args
   ;; * clj-result-k - a function that takes a vector of strings and boolean
   ;;                  that represents wether an error occured during evaluation
   :tool-fn (fn [_ args-map clj-result-k]
              (clj-result-k [(str "ECHO: " (pr-str args-map))] false))})


(defn mcp-server
  "Creates an basic stdio mcp server"
  []
  (let [transport-provider (StdioServerTransportProvider. (ObjectMapper.))
        server (-> (McpServer/async transport-provider)
                   (.serverInfo "clojure-server" "0.1.0")
                   (.capabilities (-> (McpSchema$ServerCapabilities/builder)
                                      (.tools true)
                                      (.prompts true) ;; <-- Ensure prompts are enabled
                                      #_(.logging false)
                                      (.build)))
                   (.build))]

    #_(-> server
        (.level McpSchema$LoggingLevel/INFO)
        (.logger "ClojureMCPlog")
        (.data("Server initialized"))
        (.build))
    ;; for development
    #_(-> (.addTool server (create-async-tool echo-tool))
        (.subscribe))

    ;; Add Prompts
    (-> (.addPrompt server (create-async-prompt prompts/clojure-dev-prompt))
        (.subscribe))
    (-> (.addPrompt server (create-async-prompt prompts/clojure-repl-driven-prompt))
        (.subscribe))
    (-> (.addPrompt server (create-async-prompt prompts/clojure-spec-driven-modifier))
        (.subscribe))
    (-> (.addPrompt server (create-async-prompt prompts/clojure-test-driven-modifier))
        (.subscribe))
    (-> (.addPrompt server (create-async-prompt prompts/clojure-project-context-modifier))
        (.subscribe))
    (-> (.addPrompt server (create-async-prompt prompts/clj-sync-namespace)) ;; <-- Add sync namespace prompt
        (.subscribe))


    server))


(defn create-and-start-nrepl-connection [config]
  (let [nrepl-client (nrepl/create config)]
    (nrepl/start-polling nrepl-client)
    ;; Ensure clojure.repl is loaded for apropos/source-fn used in tools
    (nrepl/eval-code nrepl-client
                     (str
                      "(require 'clojure.repl)"
                      "(require 'nrepl.util.print)")
                     identity)
    nrepl-client))


(defn close-servers [mcp] ;; Remove :nrepl from destructuring
  (when-let [client @nrepl-client-atom] ;; Get client from atom
    (nrepl/stop-polling client))
  (.closeGracefully mcp))

;; the args is a config map that must have :port and may have :host
(defn nrepl-mcp-server [args]
  (let [nrepl-client (create-and-start-nrepl-connection args)
        mcp (mcp-server)] ;; Get only mcp server
    (reset! nrepl-client-atom nrepl-client)
    ;; Use the top-level nrepl-client-atom directly
    (add-tool mcp (repl-tools/eval-code nrepl-client-atom))
    (add-tool mcp (repl-tools/current-namespace nrepl-client-atom))
    (add-tool mcp (repl-tools/symbol-completions nrepl-client-atom))
    (add-tool mcp (repl-tools/symbol-metadata nrepl-client-atom))
    (add-tool mcp (repl-tools/symbol-documentation nrepl-client-atom))
    (add-tool mcp (repl-tools/source-code nrepl-client-atom))
    (add-tool mcp (repl-tools/symbol-search nrepl-client-atom))
    (add-tool mcp (repl-tools/list-namespaces nrepl-client-atom))
    (add-tool mcp (repl-tools/list-vars-in-namespace nrepl-client-atom))
    (add-tool mcp (repl-tools/eval-history nrepl-client-atom)) ;; Add the eval-history tool
    (add-tool mcp (repl-tools/clojure-edit-replace-form nrepl-client-atom)) ;; Add the top-level-form-edit tool
    (add-tool mcp (repl-tools/clojure-edit-insert-before-form nrepl-client-atom)) ;; Add the top-level-form-edit tool
    (add-tool mcp (repl-tools/clojure-edit-insert-after-form nrepl-client-atom)) ;; Add the top-level-form-edit tool

        #_(do
            (add-tool mcp (file-api/emacs-flash-file-tool))
            (add-tool mcp (file-api/emacs-edit-file-tool))
            (add-tool mcp (file-api/emacs-write-file-tool))
            (add-tool mcp (file-api/emacs-append-to-file-tool))
            (add-tool mcp (file-api/emacs-read-file-tool))
            (add-tool mcp (file-api/emacs-file-exists-tool))
            (add-tool mcp (file-api/emacs-read-multiple-files-tool))
            (add-tool mcp (file-api/emacs-find-files-tool))
        
            (add-tool mcp (file-api/emacs-delete-file-tool))
            (add-tool mcp (file-api/emacs-move-file-tool))
            (add-tool mcp (file-api/emacs-copy-file-tool))

            (add-tool mcp (file-api/emacs-create-directory-tool))
            (add-tool mcp (file-api/emacs-list-directory-tool))
            (add-tool mcp (file-api/emacs-get-file-info-tool)))
    
    mcp))

(comment
  (def mcp-serv (nrepl-mcp-server {:port 54171})) ;; Start server, sets atom
  (close-servers mcp-serv) ;; Pass the map containing the mcp server

  )

#_(defn -main [& args]
  (let [server (mcp-server args)]
    (println "MCP Async Server running on STDIO transport.")
    ;; Keep the process alive
    #_(while true
        (Thread/sleep 1000))))

(comment
  ;; For REPL testing:
  (mcp-server)
  )

