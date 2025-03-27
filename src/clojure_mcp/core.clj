(ns clojure-mcp.core
  (:require [clojure.data.json :as json])
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

;; {
;;       name: "calculate_sum",
;;       description: "Add two numbers together",
;;       inputSchema: {
;;         type: "object",
;;         properties: {
;;           a: { type: "number" },
;;           b: { type: "number" }
;;         },
;;         required: ["a", "b"]
;;       }
;;     }

(defn capture-output [k]
  (let [out-atom (atom "")
        err-atom (atom "")
        res (atom nil)]
    (binding [*out* (java.io.StringWriter.)
              *err* (java.io.StringWriter.)]
              (reset! res (k))
              (reset! out-atom (str *out*))
              (reset! err-atom (str *err*)))
    {:result @res :out @out-atom :err @err-atom}))

;; just a mock eval tool
(defn eval-tool-helper [form-str]
  (capture-output
   (fn []
     (let [form (read-string (str "(do " form-str ")"))]
       (pr-str
        (eval form))))))

(def eval-schema
  (json/write-str {:type :object
                   :properties {:expression {:type :string}}
                   :required [:expression]}))

;; result json constructor
(defn text-content [^String s]
  (McpSchema$TextContent. s))

#_(text-result "")

(defn text-result [^String s]
  (McpSchema$CallToolResult. [(McpSchema$TextContent. s)] false))

(defn create-mono-from-callback
  "Creates a Mono from an async callback function.
   The callback function should take three arguments:
   - exchange: The MCP exchange object
   - arguments: The tool arguments
   - continuation: A function that will be called with the result"
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

(defn create-async-tool
  "Creates an AsyncToolSpecification with the given parameters.
   
   Parameters:
   - name: The name of the tool
   - description: A description of what the tool does
   - schema: JSON schema for the tool's input parameters
   - callback-fn: Function that implements the tool's logic
     (takes exchange, arguments, and continuation function)"
  [name description schema callback-fn]
  (let [mono-fn (create-mono-from-callback callback-fn)]
    (McpServerFeatures$AsyncToolSpecification.
     (McpSchema$Tool. name description schema)
     (reify java.util.function.BiFunction
       (apply [this exchange arguments]
         (mono-fn exchange arguments))))))

(defn eval-tool-callback 
  "Asynchronous eval tool callback that takes a continuation function.
   The continuation will be called with the result when ready."
  [exchange arguments continuation]
  ;; Run evaluation in a separate thread to not block
  (future
    (let [{:keys [out err result]} (eval-tool-helper (get arguments "expression"))]
      (continuation (text-result result)))))

(def eval-tool
  (create-async-tool
   "clojure_eval"
   "Takes a Clojure Expression and evaluates it in the 'user namespace. For example: provide \"(+ 1 2)\" and this will evaluate that and return 3"
   eval-schema
   eval-tool-callback))

#_(eval-tool-callback nil "hello")

(def hello-schema (json/write-str {:type :object}))

(defn hello-tool-callback 
  "Asynchronous hello tool callback that takes a continuation function.
   The continuation will be called with the result when ready."
  [exchange arguments continuation]
  ;; Simulate async work with a future
  (future
    (Thread/sleep 1000) ;; Simulate 1 second of work
    (continuation (McpSchema$CallToolResult. 
                   [(text-content "Hello world!") 
                    (text-content "Hello doors!")] 
                   false))))

(def hello-tool
  (create-async-tool
   "hello"
   "Returns hello"
   hello-schema
   hello-tool-callback))

(defn mcp-server [& args]
  (let [transport-provider (StdioServerTransportProvider. (ObjectMapper.))
        server (-> (McpServer/async transport-provider)
                   (.serverInfo "clojure-server" "0.1.0")
                   (.capabilities (-> (McpSchema$ServerCapabilities/builder)
                                      (.tools true)
                                      (.build)))
                   (.build))]
    (-> (.addTool server eval-tool)
        (.subscribe))
    (-> (.addTool server hello-tool)
        (.subscribe))
    #_(println "MCP Async Server running on STDIO transport.")
    server))

(defn -main [& args]
  (let [server (mcp-server args)]
    (println "MCP Async Server running on STDIO transport.")
    ;; Keep the process alive
    (while true
      (Thread/sleep 1000))))

(comment
  ;; For REPL testing:
  (mcp-server)
  )

