(ns clojure-mcp.agent.langchain
  (:require
   [clojure.data.json :as json]
   [clojure-mcp.agent.langchain.schema :as schema])
  (:import
   ;; LangChain4j Core and Service classes
   [dev.langchain4j.service AiServices MemoryId]
   [dev.langchain4j.agent.tool ToolSpecification #_ToolParameter]
   [dev.langchain4j.service.tool ToolExecutor ToolExecution]
   [dev.langchain4j.service TokenStream]
   [dev.langchain4j.data.message SystemMessage UserMessage TextContent TextFileContent]
   [dev.langchain4j.agent.tool ToolExecutionRequest]
   [dev.langchain4j.model.chat.request.json JsonObjectSchema]
   [dev.langchain4j.memory ChatMemory]
   [dev.langchain4j.memory.chat MessageWindowChatMemory]

   ;; LangChain4j Model classes (using Anthropic as an example)
   [dev.langchain4j.model.chat ChatLanguageModel]
   [dev.langchain4j.model.anthropic
    AnthropicChatModel
    AnthropicStreamingChatModel
    AnthropicChatModelName]
   [java.util.function Consumer Function]

   ;; Java Time API
   [java.time LocalTime LocalDate ZoneId]))

(def default-max-memory 100)

(defn create-model-claude-3-7 []
  (-> (AnthropicChatModel/builder)
      (.apiKey (System/getenv "ANTHROPIC_API_KEY"))
      (.modelName AnthropicChatModelName/CLAUDE_3_7_SONNET_20250219)
      #_(.modelName AnthropicChatModelName/CLAUDE_3_5_SONNET_20241022)
      (.logRequests true)
      (.logResponses true)))

(defn chat-memory
  ([]
   (chat-memory default-max-memory))
  ([size]
   (MessageWindowChatMemory/withMaxMessages size)))

;; ------------------------------------------------------------
;; converting tools
;; ------------------------------------------------------------

(defn is-well-formed-json? [json-string]
  (try
    {:result (json/read-str json-string :key-fn keyword)}
    (catch Exception _ false)))

(defn registration-map->tool-executor [{:keys [tool-fn]}]
  (reify ToolExecutor
    (execute [_this request memory-id]
      (let [tool-name (.name request)
            arg-str (.arguments request)]
        (if-let [arg-result (is-well-formed-json? arg-str)]
          (try
            (let [callback-result (promise)]
              (tool-fn nil
                       (:result arg-result)
                       (fn [result error]
                         (deliver callback-result
                                  (if error
                                    (throw (ex-info (str "Tool error: " (first result))
                                                    {:tool-error true, :result result}))
                                    (if (sequential? result)
                                      (clojure.string/join "\n\n" result)
                                      (str result))))))
              @callback-result)
            (catch Exception e
              (throw (ex-info
                      (str "Error executing tool '" tool-name "': " (.getMessage e)
                           (with-out-str (clojure.pprint/pprint (Throwable->map e))))
                      {} e))))
          (throw (ex-info
                  (str "ERROR: Arguments provided to the tool call were malformed.\n=====\n" arg-str "\n=====\n")
                  {})))))))

(defn registration-map->tool-specification [{:keys [name description schema]}]
  {:pre [(string? schema) (string? name) (string? description)]}
  (-> (ToolSpecification/builder)
      (.name name)
      (.description description)
      (.parameters (schema/edn->sch
                    (if (string? schema)
                      (json/read-str schema :key-fn keyword)
                      schema)))
      (.build)))

(defn convert-tools [registration-maps]
  (into {}
        (map (juxt registration-map->tool-specification
                   registration-map->tool-executor)
             registration-maps)))

(definterface AiService
  (^String chat [^String userMessage])
  (^String chat [^dev.langchain4j.data.message.UserMessage userMessage]))

(defn create-service [klass {:keys [model memory tools system-message]}]
  (-> (AiServices/builder klass) ; Use the interface defined above
      (.chatLanguageModel model)
      (.systemMessageProvider
       (reify Function
         (apply [this mem-id]
           system-message)))
      (cond->
       tools (.tools (convert-tools tools)))
      (.chatMemory memory)))

(comment
  (def test-tool
    {:name "hello"
     :description "says hello"
     :schema
     (json/json-str {:type :object
                     :properties
                     {:nm {:type :string
                           :description "Name to say hi to"}}
                     :required [:nm]})
     :tool-fn (fn [_ {:keys [nm]} callback]
                (callback [(str "Hello " nm "!")] false))})

  (let [exec (registration-map->tool-executor test-tool)]
    (.execute exec (-> (ToolExecutionRequest/builder)
                       (.name "hello")
                       (.arguments (json/json-str {:nm "Joey"}))
                       (.build))
              "asdf"))
  
  )

;; keep this example
#_(definterface StreamingAiService
    (^dev.langchain4j.service.TokenStream chat [^String userMessage])
    (^dev.langchain4j.service.TokenStream chat [^dev.langchain4j.data.message.UserMessage userMessage]))

