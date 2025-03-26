(ns clojure-mcp.core
  (:gen-class)
  (:import [io.modelcontextprotocol.sdk.server.stdio StdioServerTransportProvider]
           [io.modelcontextprotocol.sdk.server McpServer]
           [io.modelcontextprotocol.sdk.server ServerCapabilities]
           [com.fasterxml.jackson.databind ObjectMapper]
           [io.modelcontextprotocol.sdk.server.McpServerFeatures$SyncToolSpecification]
           [io.modelcontextprotocol.sdk.schema Tool]
           [io.modelcontextprotocol.sdk.schema CallToolResult]))

(def hello-schema "{\"type\": \"object\"}")

(defn hello-tool-callback [exchange arguments]
  (CallToolResult. "Hello, world!" false))

(def sync-hello-tool
  (McpServerFeatures$SyncToolSpecification.
   (Tool. "hello" "Prints hello world" hello-schema)
   (reify
     (call [this exchange arguments]
       (hello-tool-callback exchange arguments)))))

(defn -main [& args]
  (let [transport-provider (StdioServerTransportProvider. (ObjectMapper.))
        server (-> (McpServer/sync transport-provider)
                   (.serverInfo "hello-server" "0.1.0")
                   (.capabilities (-> (ServerCapabilities/builder)
                                      (.tools true)
                                      (.build)))
                   (.build))]
    (.addTool server sync-hello-tool)
    (println "MCP Hello World Server running on STDIO transport.")
    (while true
      (Thread/sleep 1000))))

(comment
  ;; For REPL testing:
  (-main))

