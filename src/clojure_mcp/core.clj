(ns clojure-mcp.core
  (:gen-class)
  (:import [io.modelcontextprotocol.server.transport StdioServerTransportProvider]
           [io.modelcontextprotocol.server McpServer McpServerFeatures
            McpServerFeatures$SyncToolSpecification]
           [io.modelcontextprotocol.spec
            McpSchema$ServerCapabilities
            McpSchema$Tool
            McpSchema$CallToolResult]
           [com.fasterxml.jackson.databind ObjectMapper]))

(def hello-schema "{\"type\": \"object\"}")

(defn hello-tool-callback [exchange arguments]
  (McpSchema$CallToolResult. "Hello, world!" false))

(def sync-hello-tool
  (McpServerFeatures$SyncToolSpecification.
   (McpSchema$Tool. "hello" "Prints hello world" hello-schema)
   (reify java.util.function.BiFunction
     (apply [this exchange arguments]
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

