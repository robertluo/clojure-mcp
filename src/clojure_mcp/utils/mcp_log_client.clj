(ns clojure-mcp.utils.mcp-log-client
  "Utilities for sending log messages to MCP clients.
   Provides functions for different log levels and a generic logging function."
  (:require
   [clojure.tools.logging :as log])
  (:import
   [io.modelcontextprotocol.spec
    McpSchema$LoggingLevel
    McpSchema$LoggingMessageNotification]))

(defn send-log-message
  "Sends a log message to MCP clients.
   
   Parameters:
   - mcp-server: The MCP server instance
   - level: Log level, one of :trace, :debug, :info, :warn, :error
   - logger-name: Name of the logger (string)
   - message: The log message (string)
   
   Example:
   ```clojure
   (send-log-message mcp-server :info \"my-logger\" \"Hello from MCP!\")
   ```"
  [mcp-server level logger-name message]
  (try
    (let [log-level (case level
                      :trace McpSchema$LoggingLevel/TRACE
                      :debug McpSchema$LoggingLevel/DEBUG
                      :info McpSchema$LoggingLevel/INFO
                      :warn McpSchema$LoggingLevel/WARN
                      :error McpSchema$LoggingLevel/ERROR
                      McpSchema$LoggingLevel/INFO)]
      (log/debug "Sending MCP client log" {:level level :logger logger-name})
      (.loggingNotification mcp-server
                            (-> (McpSchema$LoggingMessageNotification/builder)
                                (.level log-level)
                                (.logger logger-name)
                                (.data message)
                                (.build))))
    (catch Exception e
      (log/error e "Failed to send log message to MCP client"))))

;; Convenience functions for different log levels

(defn log-trace
  "Sends a TRACE level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :trace logger-name message))

(defn log-debug
  "Sends a DEBUG level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :debug logger-name message))

(defn log-info
  "Sends an INFO level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :info logger-name message))

(defn log-warn
  "Sends a WARN level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :warn logger-name message))

(defn log-error
  "Sends an ERROR level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :error logger-name message))

(defn with-mcp-logging
  "Returns a function that logs to both Clojure's logging system and MCP clients.
   
   Parameters:
   - mcp-server: The MCP server instance
   - logger-name: Name of the logger (string)
   
   Returns a map with log functions:
   {:trace, :debug, :info, :warn, :error}
   
   Example:
   ```clojure
   (let [logger (with-mcp-logging mcp-server \"my-component\")]
     ((:info logger) \"Operation completed successfully\"))
   ```"
  [mcp-server logger-name]
  {:trace (fn [message]
            (log/trace message)
            (log-trace mcp-server logger-name message))
   :debug (fn [message]
            (log/debug message)
            (log-debug mcp-server logger-name message))
   :info (fn [message]
           (log/info message)
           (log-info mcp-server logger-name message))
   :warn (fn [message]
           (log/warn message)
           (log-warn mcp-server logger-name message))
   :error (fn [message]
            (log/error message)
            (log-error mcp-server logger-name message))
   :error-ex (fn [exception message]
               (log/error exception message)
               (log-error mcp-server logger-name (str message ": " (.getMessage exception))))})

(comment
  ;; Example usage
  (def mcp-server (get-in @nrepl-client-atom [::mcp-server]))

  ;; Direct function usage
  (send-log-message mcp-server :info "test-logger" "This is a test message")

  ;; Using convenience functions
  (log-info mcp-server "test-logger" "This is an info message")
  (log-error mcp-server "test-logger" "This is an error message")

  ;; Using the combined logger
  (let [logger (with-mcp-logging mcp-server "my-component")]
    ((:info logger) "Starting operation")
    ((:debug logger) "Processing data...")
    ((:error-ex logger) (Exception. "Something went wrong") "Failed to complete operation")))