(ns clojure-mcp.core
  (:require [clojure.data.json :as json]
            [clojure-mcp.nrepl :as nrepl]
            [clojure-mcp.repl-tools :as repl-tools]
            [clojure-mcp.prompts :as prompts]
            [clojure-mcp.resources :as resources]
            [clojure.edn :as edn])
  (:gen-class)
  (:import [io.modelcontextprotocol.server.transport StdioServerTransportProvider]
           [io.modelcontextprotocol.server McpServer McpServerFeatures
            McpServerFeatures$AsyncToolSpecification
            McpServerFeatures$AsyncResourceSpecification]
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
            McpSchema$LoggingLevel
            McpSchema$Resource
            McpSchema$ReadResourceResult
            McpSchema$TextResourceContents
            McpSchema$ResourceContents]
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
  [mcp-server tool-map]
  (.removeTool mcp-server (:name tool-map))
  ;; Pass the service-atom along when creating the tool
  (-> (.addTool mcp-server (create-async-tool tool-map))
      (.subscribe)))

(defn create-async-resource
  "Creates an AsyncResourceSpecification with the given parameters.
   
   Takes a map with the following keys:
    :url          - The URL of the resource
    :name         - The name of the resource
    :description  - A description of what the resource is
    :mime-type    - The MIME type of the resource
    :resource-fn  - Function that implements the resource retrieval logic.
                    Signature: (fn [exchange request clj-result-k] ... )
                      * exchange     - The MCP exchange object
                      * request      - The request object
                      * clj-result-k - continuation fn taking a vector of strings"
  [{:keys [url name description mime-type resource-fn]}]
  (let [resource (McpSchema$Resource. url name description mime-type nil)
        mono-fn (create-mono-from-callback
                 (fn [exchange request mono-fill-k]
                   (resource-fn
                    exchange
                    request
                    (fn [result-strings]
                      ;; Create TextResourceContents objects with the URL and MIME type
                      (let [resource-contents (mapv #(McpSchema$TextResourceContents. url mime-type %)
                                                    result-strings)]
                        ;; Create ReadResourceResult with the list of TextResourceContents
                        (mono-fill-k (McpSchema$ReadResourceResult. resource-contents)))))))]
    (McpServerFeatures$AsyncResourceSpecification.
     resource
     (reify java.util.function.BiFunction
       (apply [this exchange request]
         (mono-fn exchange request))))))

(defn add-resource
  "Helper function to create an async resource from a map and add it to the server.
   
   Takes an MCP server and a resource map with:
    :url          - The URL of the resource
    :name         - The name of the resource
    :description  - A description of what the resource is
    :mime-type    - The MIME type of the resource
    :resource-fn  - Function that implements the resource retrieval logic."
  [mcp-server resource-map]
  (.removeResource mcp-server (:url resource-map))
  (-> (.addResource mcp-server (create-async-resource resource-map))
      (.subscribe)))

(defn add-prompt
  "Helper function to create an async prompt from a map and add it to the server.
   
   Takes an MCP server and a prompt map with:
    :name        - The name (ID) of the prompt
    :description - A description of the prompt
    :arguments   - A vector of maps, each defining an argument
    :prompt-fn   - Function that implements the prompt logic."
  [mcp-server prompt-map]
  (.removePrompt mcp-server (:name prompt-map))
  (-> (.addPrompt mcp-server (create-async-prompt prompt-map))
      (.subscribe)))

;; helper tool to demonstrate how all this gets hooked together

(defn mcp-server
  "Creates a basic stdio mcp server"
  []
  (let [transport-provider (StdioServerTransportProvider. (ObjectMapper.))
        server (-> (McpServer/async transport-provider)
                   (.serverInfo "clojure-server" "0.1.0")
                   (.capabilities (-> (McpSchema$ServerCapabilities/builder)
                                      (.tools true)
                                      (.prompts true)
                                      (.resources true true) ;; Fixed: resources method takes two boolean parameters
                                      #_(.logging false)
                                      (.build)))
                   (.build))]

    #_(-> server
          (.level McpSchema$LoggingLevel/INFO)
          (.logger "ClojureMCPlog")
          (.data ("Server initialized"))
          (.build))

    ;; Prompts will be registered in nrepl-mcp-server function

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
    (let [user-dir (try
                     (edn/read-string
                      (nrepl/tool-eval-code
                       nrepl-client
                       "(System/getProperty \"user.dir\")"))
                     (catch Exception _
                       nil))]
      (cond-> nrepl-client
        user-dir (assoc ::nrepl-user-dir user-dir
                        ::allowed-directories [user-dir]
                        ::emacs-notify true)))))

(defn close-servers [mcp] ;; Remove :nrepl from destructuring
  (when-let [client @nrepl-client-atom] ;; Get client from atom
    (nrepl/stop-polling client))
  (.closeGracefully mcp))

;; the args is a config map that must have :port and may have :host
(defn nrepl-mcp-server [args]
  (let [nrepl-client (create-and-start-nrepl-connection args)
        mcp (mcp-server)] ;; Get only mcp server
    (reset! nrepl-client-atom nrepl-client)

    ;; Register all defined tools
    (doseq [tool (repl-tools/get-all-tools nrepl-client-atom)]
      (add-tool mcp tool))

    ;; Register all defined prompts
    (doseq [prompt (prompts/get-all-prompts nrepl-client-atom)]
      (add-prompt mcp prompt))

    ;; Register all defined resources
    (doseq [resource (resources/get-all-resources nrepl-client-atom)]
      (add-resource mcp resource))

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

  ;; Example of how to create and add a resource to the MCP server
  (def example-resource
    {:url "custom://example-resource"
     :name "Example Resource"
     :description "An example resource that returns a simple text string"
     :mime-type "text/plain"
     :resource-fn (fn [_ _ clj-result-k]
                    (clj-result-k ["Hello, this is an example resource!"]))})

  ;; Adding the resource to an MCP server
  (def mcp-serv (nrepl-mcp-server {:port 54171}))
  (add-resource mcp-serv example-resource)

  ;; Example of a file resource
  (def file-resource
    {:url "custom://file-resource"
     :name "File Resource"
     :description "A resource that serves the content of a file"
     :mime-type "text/plain"
     :resource-fn (fn [_ _ clj-result-k]
                    (let [file-content (slurp "path/to/file.txt")]
                      (clj-result-k [file-content])))})

  ;; Adding the file resource to an MCP server
  (add-resource mcp-serv file-resource)

  ;; Example of a dynamic resource with request parameters
  (def dynamic-resource
    {:url "custom://dynamic-resource"
     :name "Dynamic Resource"
     :description "A resource that generates content based on request parameters"
     :mime-type "text/plain"
     :resource-fn (fn [_ request clj-result-k]
                    (let [params (.. request parameters) ;; Access request parameters
                          content (str "You requested: " params)]
                      (clj-result-k [content])))})

  ;; Adding the dynamic resource to an MCP server
  (add-resource mcp-serv dynamic-resource)

  ;; Closing the server
  (close-servers mcp-serv))

