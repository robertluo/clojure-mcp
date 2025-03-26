(ns clojure-mcp.core
  (:require [clojure.data.json :as json])
  #_(:gen-class)
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

(defn eval-tool-callback [exchange arguments]
  (let [{:keys [out err result]} (eval-tool-helper (get arguments "expression"))]
    (text-result result)))

(def eval-tool
  (McpServerFeatures$AsyncToolSpecification.
   (McpSchema$Tool. "clojure_eval" "Takes a Clojure Expression and evaluates it in the 'user namespace. For example: provide \"(+ 1 2)\" and this will evaluate that and return 3" eval-schema)
   (reify java.util.function.BiFunction
     (apply [this exchange arguments]
       (Mono/just (eval-tool-callback exchange arguments))))))

#_(eval-tool-callback nil "hello")

(def hello-schema (json/write-str {:type :object}))

(defn hello-tool-callback [exchange arguments]
  (McpSchema$CallToolResult. [(text-content "Hello world!") (text-content "Hello doors!") ] false))

(def hello-tool
  (McpServerFeatures$AsyncToolSpecification.
   (McpSchema$Tool. "hello" "Returns hello" hello-schema)
   (reify java.util.function.BiFunction
     (apply [this exchange arguments]
       (Mono/just (hello-tool-callback exchange arguments))))))

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
    server))

(comment
  ;; For REPL testing:
  (-main)

  )

