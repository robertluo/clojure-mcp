(ns clojure-mcp.core
  (:require [clojure.data.json :as json]
            [clojure-mcp.nrepl :as nrepl]
            [clojure-mcp.repl-tools :as repl-tools])
  (:gen-class)
  (:import [io.modelcontextprotocol.server.transport StdioServerTransportProvider]
           [io.modelcontextprotocol.server McpServer McpServerFeatures
            McpServerFeatures$AsyncToolSpecification]
           [io.modelcontextprotocol.spec
            McpSchema$ServerCapabilities
            McpSchema$Tool
            McpSchema$CallToolResult
            McpSchema$TextContent]
           [reactor.core.publisher Mono]
           [com.fasterxml.jackson.databind ObjectMapper]))

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
    :name  -        The name of the tool
    :description -  A description of what the tool does
    :schema -       JSON schema for the tool's input parameters
    :tool-fn -      Function that implements the tool's logic
                    That has the signature (fn [exchange args-map clj-result-k] ... )
                      * exchange - ignored
                      * arg-map - map with string keys representing the mcp tool call args
                      * clj-result-k - a function that takes a vector of strings and boolean
                                       that represents wether an error occured during evaluation"
  [{:keys [name description schema tool-fn]}]
  (let [mono-fn (create-mono-from-callback
                 (fn [_ arg-map mono-fill-k]
                   (tool-fn _ arg-map
                            (fn [res-list error?]
                              (mono-fill-k (adapt-result res-list error?))))))]
    (McpServerFeatures$AsyncToolSpecification.
     (McpSchema$Tool. name description schema)
     (reify java.util.function.BiFunction
       (apply [this exchange arguments]
         (mono-fn exchange arguments))))))

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
                                      (.build)))
                   (.build))]
    ;; for development
    (-> (.addTool server (create-async-tool echo-tool))
        (.subscribe))
    server))

(defn create-and-start-nrepl-connection [config]
  (let [service (nrepl/create config)]
    (nrepl/start-polling service)
    service))

(defn close-servers [{:keys [nrepl mcp]}]
  (nrepl/stop-polling nrepl)
  (.closeGracefully mcp))

(defn create-nrepl-mcp-server [args]
  {:nrepl (create-and-start-nrepl-connection args)
   :mcp (mcp-server)})

(defn nrepl-mcp-server [args]
  (let [{:keys [mcp nrepl] :as server} (create-nrepl-mcp-server args)]
    (-> (.addTool mcp (create-async-tool (repl-tools/eval-code nrepl)))
        (.subscribe))
    server))



(comment
  (def servs (nrepl-mcp-server {:port 54171}))
  (close-servers servs)

  )



(defn -main [& args]
  (let [server (mcp-server args)]
    (println "MCP Async Server running on STDIO transport.")
    ;; Keep the process alive
    #_(while true
        (Thread/sleep 1000))))

(comment
  ;; For REPL testing:
  (mcp-server)
  )

