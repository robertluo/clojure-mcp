(ns clojure-mcp.sse-core
  (:require
   [clojure-mcp.main :as main]
   [clojure-mcp.core :as core]
   [clojure-mcp.config :as config]
   [clojure.tools.logging :as log])
  (:import
   [io.modelcontextprotocol.server.transport
    HttpServletSseServerTransportProvider]
   [org.eclipse.jetty.server Server]
   [org.eclipse.jetty.servlet ServletContextHandler ServletHolder]
   [jakarta.servlet.http HttpServlet HttpServletRequest HttpServletResponse]
   [io.modelcontextprotocol.server McpServer McpServerFeatures
    McpServerFeatures$AsyncToolSpecification
    McpServerFeatures$AsyncResourceSpecification]
   [io.modelcontextprotocol.spec
    McpSchema$ServerCapabilities]
   [com.fasterxml.jackson.databind ObjectMapper]))

;; helpers for setting up an sse mcp server

(defn mcp-sse-server []
  (log/info "Starting SSE MCP server")
  (try
    (let [transport-provider (HttpServletSseServerTransportProvider. (ObjectMapper.) "/mcp/message")
          server (-> (McpServer/async transport-provider)
                     (.serverInfo "clojure-server" "0.1.0")
                     (.capabilities (-> (McpSchema$ServerCapabilities/builder)
                                        (.tools true)
                                        (.prompts true)
                                        (.resources true true)
                                        #_(.logging)
                                        (.build)))
                     (.build))]
      (log/info "SSE MCP server initialized successfully")
      {:provider-servlet transport-provider
       :mcp-server server})
    (catch Exception e
      (log/error e "Failed to initialize SSE MCP server")
      (throw e))))

(defn host-mcp-servlet
  "Main function to start the embedded Jetty server."
  [servlet port]
  (let [server (Server. port)
        context (ServletContextHandler. ServletContextHandler/SESSIONS)]
    (.setContextPath context "/")
    (.addServlet context (ServletHolder. servlet) "/")
    (.setHandler server context)
    (.start server)
    (println (str "Clojure tooling SSE MCP server started on port " port "."))
    (.join server)))
