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
              ;; Tool-fn expects a callback function to be called with result and error flag
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

(comment
  (let [exec (registration-map->tool-executor test-tool)]
    (.execute exec (-> (ToolExecutionRequest/builder)
                       (.name "hello")
                       (.arguments (json/json-str {:nm "Joey"}))
                       (.build))
              "asdf")))

#_(registration-map->tool-executor test-tool)

(defn json->JsonObjectSchema [json-str]
  (let [schema (json/read-str json-str :key-fn keyword)
        builder (JsonObjectSchema/builder)]
    ;; Add properties from the schema
    (doseq [[prop-name prop-def] (get schema :properties)]
      (let [prop-type (get prop-def :type)
            description (get prop-def :description "")]
        (case prop-type
          :string (.addStringProperty builder (name prop-name) description)
          :integer (.addIntegerProperty builder (name prop-name) description)
          :number (.addNumberProperty builder (name prop-name) description)
          :boolean (.addBooleanProperty builder (name prop-name) description)
          ;; TODO this probably needs to be recursively handled
          ;; :object (.addObjectProperty builder (name prop-name) description)
          :array (let [items (get prop-def :items {})
                       item-type (get items :type)]
                   (case item-type
                     ;; TODO I don't think these methods exist
                     :string (.addStringArrayProperty builder (name prop-name) description)
                     :integer (.addIntegerArrayProperty builder (name prop-name) description)
                     :number (.addNumberArrayProperty builder (name prop-name) description)
                     :boolean (.addBooleanArrayProperty builder (name prop-name) description)
                     ;; Default to string array if item type not specified or recognized
                     (.addStringArrayProperty builder (name prop-name) description)))
          ;; Default to string for any unrecognized types
          (.addStringProperty builder (name prop-name) description))))

    ;; Add required properties
    (when-let [required (get schema :required)]
      (.required builder (map name required)))

    ;; Build and return the schema
    (.build builder)))

(defn registration-map->tool-specification [{:keys [name description schema]}]
  (-> (ToolSpecification/builder)
      (.name name)
      (.description description)
      (.parameters (json->JsonObjectSchema schema))
      (.build)))

#_(registration-map->tool-specification test-tool)

;; This converts from a list of registration maps
;; to a map of ToolSpecifications to ToolExecutor s
(defn convert-tools [registration-maps]
  (into {}
        (map (juxt registration-map->tool-specification
                   registration-map->tool-executor)
             registration-maps)))

#_(definterface StreamingAiService
    (^dev.langchain4j.service.TokenStream chat [^String userMessage])
    (^dev.langchain4j.service.TokenStream chat [^dev.langchain4j.data.message.UserMessage userMessage]))

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
       tools (.tools tools))
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
                (callback [(str "Hello " nm "!")] false))}))
