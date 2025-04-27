(ns clojure-mcp.utils.mcp-log-client
  "Utilities for sending log messages to MCP clients.
   Provides functions for different log levels and a generic logging function."
  (:require
   [clojure.tools.logging :as log])
  (:import
   [io.modelcontextprotocol.spec
    McpSchema$LoggingLevel
    McpSchema$LoggingMessageNotification]))

;; NOTE: use logger-name "clojure_connect" ?

(defn send-log-message
  "Sends a log message to MCP clients with the specified level."
  [mcp-server level logger-name message]
  (try
    (let [log-level (case level
                      :debug McpSchema$LoggingLevel/DEBUG
                      :info McpSchema$LoggingLevel/INFO
                      :notice McpSchema$LoggingLevel/NOTICE
                      :warning McpSchema$LoggingLevel/WARNING
                      :error McpSchema$LoggingLevel/ERROR
                      :critical McpSchema$LoggingLevel/CRITICAL
                      :alert McpSchema$LoggingLevel/ALERT
                      :emergency McpSchema$LoggingLevel/EMERGENCY
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

(defn debug
  "Sends a DEBUG level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :debug logger-name message))

(defn info
  "Sends an INFO level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :info logger-name message))

(defn warning
  "Sends a WARNING level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :warning logger-name message))

(defn error
  "Sends an ERROR level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :error logger-name message))

(defn notice
  "Sends a NOTICE level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :notice logger-name message))

(defn critical
  "Sends a CRITICAL level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :critical logger-name message))

(defn alert
  "Sends an ALERT level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :alert logger-name message))

(defn emergency
  "Sends an EMERGENCY level log message to MCP clients."
  [mcp-server logger-name message]
  (send-log-message mcp-server :emergency logger-name message))

(defn debug-both
  "Logs a DEBUG message to both Clojure logging and MCP client."
  [mcp-server logger-name message]
  (log/debug message)
  (debug mcp-server logger-name message))

(defn info-both
  "Logs an INFO message to both Clojure logging and MCP client."
  [mcp-server logger-name message]
  (log/info message)
  (info mcp-server logger-name message))

(defn notice-both
  "Logs a NOTICE message to both systems (uses Clojure's INFO level)."
  [mcp-server logger-name message]
  (log/info message) ; No direct notice level in tools.logging
  (notice mcp-server logger-name message))

(defn warning-both
  "Logs a WARNING message to both Clojure logging and MCP client."
  [mcp-server logger-name message]
  (log/warn message)
  (warning mcp-server logger-name message))

(defn error-both
  "Logs an ERROR message to both Clojure logging and MCP client."
  [mcp-server logger-name message]
  (log/error message)
  (error mcp-server logger-name message))

(defn critical-both
  "Logs a CRITICAL message to both systems (uses Clojure's ERROR level)."
  [mcp-server logger-name message]
  (log/error message) ; No direct critical level in tools.logging
  (critical mcp-server logger-name message))

(defn alert-both
  "Logs an ALERT message to both systems (uses Clojure's ERROR level)."
  [mcp-server logger-name message]
  (log/error message) ; No direct alert level in tools.logging
  (alert mcp-server logger-name message))

(defn emergency-both
  "Logs an EMERGENCY message to both systems (uses Clojure's FATAL level)."
  [mcp-server logger-name message]
  (log/fatal message) ; Using fatal for emergency
  (emergency mcp-server logger-name message))

(defn error-ex-both
  "Logs an error with exception to both Clojure logging and MCP client."
  [mcp-server logger-name exception message]
  (log/error exception message)
  (error mcp-server logger-name (str message ": " (.getMessage exception))))

(comment
  ;; Example usage
  (def mcp-server (get-in @nrepl-client-atom [::mcp-server]))

  ;; Direct MCP-only logging functions
  (debug mcp-server "test-logger" "This is a debug message")
  (info mcp-server "test-logger" "This is an info message")
  (notice mcp-server "test-logger" "This is a notice message")
  (warning mcp-server "test-logger" "This is a warning message")
  (error mcp-server "test-logger" "This is an error message")
  (critical mcp-server "test-logger" "This is a critical message")
  (alert mcp-server "test-logger" "This is an alert message")
  (emergency mcp-server "test-logger" "This is an emergency message")

  ;; Using both logging systems (logs to Clojure and MCP)
  (debug-both mcp-server "test-logger" "Debug info")
  (info-both mcp-server "test-logger" "Information message")
  (notice-both mcp-server "test-logger" "Notice message")
  (warning-both mcp-server "test-logger" "Warning condition")
  (error-both mcp-server "test-logger" "Error condition")
  (critical-both mcp-server "test-logger" "Critical error")
  (alert-both mcp-server "test-logger" "Alert condition")
  (emergency-both mcp-server "test-logger" "Emergency situation")
  (error-ex-both mcp-server "test-logger"
                 (Exception. "Something went wrong")
                 "Failed to complete operation")

  ;; Low-level API
  (send-log-message mcp-server :info "test-logger" "Custom message"))
