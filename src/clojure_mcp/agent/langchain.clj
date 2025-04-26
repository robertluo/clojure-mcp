(ns clojure-mcp.agent.langchain
  (:require
   [clojure.data.json :as json])
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

#_(definterface StreamingAiService
  (^dev.langchain4j.service.TokenStream chat [^String userMessage])
  (^dev.langchain4j.service.TokenStream chat [^dev.langchain4j.data.message.UserMessage userMessage]))

(definterface AiService
  (^String chat [^String userMessage])
  (^String chat [^dev.langchain4j.data.message.UserMessage userMessage]))

(defn create-service [klass {:keys [model memory tools system-message]}]
  (-> (AiServices/builder klass)     ; Use the interface defined above
      (.chatLanguageModel model)
      (.systemMessageProvider
       (reify Function
         (apply [this mem-id]
           system-message)))
      (cond-> 
          tools (.tools tools))
      (.chatMemory memory)))
