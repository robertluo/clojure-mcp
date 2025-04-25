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

(defn create-model []
  (-> (AnthropicChatModel/builder)
      (.apiKey (System/getenv "ANTHROPIC_API_KEY"))
      (.modelName AnthropicChatModelName/CLAUDE_3_7_SONNET_20250219)
      #_(.modelName AnthropicChatModelName/CLAUDE_3_5_SONNET_20241022)
      (.logRequests true)
      (.logResponses true)
      (.thinkingType "enabled")
      (.thinkingBudgetTokens (int 1024))
      (.maxTokens (int 2048))
      (.temperature 1.0)
      (.build)))

(comment
  (def model (create-model))
  
  (def sys
    "You are a super power Rich Hickey Clojure code advisor. Your task is to analyze code and provide feedback for improvements"
    )
  
  
  (def res (.chat model [(SystemMessage. system)
                         (UserMessage. test-code)]))
  (println (.text (.aiMessage res)))


  (def system
    "You are a super power Rich Hickey Clojure code advisor. Your task is to analyze code and provide feedback for improvements.

You are going to be presented with code and you job is to find the single most important structural improvement to the code. And then present a concise description of the improvement.

When you see a loop reccomend `iterate` or `reduce` if its appropriate.
When you see nesting look for threading `some->` `->>` `->` opporutnities

Do not validate, an approach, always offer the next improvement.

Always, let based local functions are discouraged for two important reasons:
1. they are harder to test idividualy
2. AI assistants have a much easier time editing smaller functions just like humans
3. It's an opportunity to create a resuable function to share with others. 

Frequently recommend extracting functions.  For longer functions: always extract predicates that are longer than 5 lines. Extracting the step functions (if more than a few lines) for `reduce` and `iterate` is often a gread improvment as you can test them separately.

<example-response>
I think you can use `reduce` instead of a loop
</example-response>

<example-response>
This function is nested deeply you can probably extract the predicate
</example-response>

<example-response>
This function is too long please break it up into several smaller functions, that filter predicate is very long for instance.
</example-response>


<example-response>
This function is using state, probably better to use `iterate` 
</example-response>


<example-response>
* This function is nested deeply you can probably extract the predicate
* I think you can use `reduce` instead of a loop
</example-response>")

  )

#_(definterface StreamingAiService
  (^dev.langchain4j.service.TokenStream chat [^String userMessage])
  (^dev.langchain4j.service.TokenStream chat [^dev.langchain4j.data.message.UserMessage userMessage]))

(definterface AiService
  (^String chat [^String userMessage])
  (^String chat [^dev.langchain4j.data.message.UserMessage userMessage]))

(defn create-service [{:keys [klass model memory tools system-message]}]
  (-> (AiServices/builder klass)     ; Use the interface defined above
      (.chatLanguageModel model)
      (.systemMessageProvider
       (reify Function
         (apply [this mem-id]
           system-message)))
      (cond-> 
          tools (.tools tools))
      (.chatMemory memory)
      (.build)))




(comment
  (def aiserv (create-service {:klass AiService
                               :model model
                               :memory (MessageWindowChatMemory/withMaxMessages 20)
                               :system-message system}))
  (.chat aiserv test-code)
  (def test-code
  (str
   "(ns clojure-mcp.tools.form-edit.core
  \"Core utility functions for form editing operations.
   This namespace contains the pure functionality for manipulating Clojure forms
   without any MCP-specific code.\"
  (:require
   [rewrite-clj.zip :as z]
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [cljfmt.core :as fmt]
   [clojure.string :as str]
   [clojure.java.io :as io]))"
   "\n\n"
   "(defn find-top-level-form
  \"Find a top-level form with a specific tag and name in a zipper.
   
   Arguments:
   - zloc: The zipper location to start searching from
   - tag: The tag name as a string (e.g., \\\"defn\\\", \\\"def\\\", \\\"ns\\\")
   - dname: The name of the definition as a string
   
   Returns a map with:
   - :zloc - the zipper location of the matched form, or nil if not found
   - :similar-matches - a vector of maps with {:form-name, :qualified-name, :tag} for potential namespace-qualified matches\"
  [zloc tag dname]
  (let [similar-matches (atom [])]
    (loop [loc zloc]
      (cond
        (nil? loc) {:zloc nil :similar-matches @similar-matches}

        (is-top-level-form? loc tag dname)
        {:zloc loc :similar-matches @similar-matches}

        :else
        (do
          ;; Check for namespace-qualified form with matching unqualified name
          (try
            (let [sexpr (z/sexpr loc)]
              (when (and (list? sexpr) (> (count sexpr) 1))
                (let [form-tag (first sexpr)
                      form-name (second sexpr)]
                  ;; Check for forms where the tag's unqualified name matches our tag
                  (when (and (symbol? form-tag)
                             (symbol? form-name)
                             (= (name form-tag) tag) ;; Tag's name part matches our tag
                             (= (name form-name) dname)) ;; Form name's name part matches our name
                    (swap! similar-matches conj
                           {:form-name dname
                            :qualified-name form-name
                            :tag form-tag})))))
            (catch Exception _ nil))

          (recur (z/right loc)))))))")
  )
  )




#_(defn send-message! [service message]
    {:pre [(string? message)]}
    (.chat service (UserMessage. message)))

#_(defn build-chat-service
  "Build a chat service with the given configuration map
  Expected keys in the config map:
  - :tools - Tool specifications to use
  - :repl-context - The REPL context to use
  - :initial-content - Initial content for the chat session
  
  Optional keys:
  - :max-memory - Maximum number of messages to keep in memory (default: max-memory)
  - :model - Custom model to use (default: create-model)
  - Any other keys will be included in the returned service map"
  [{:keys [tools repl-context initial-content max-memory model] :as config}]
  (let [model (or model (create-model))
        model-mem-tools {:model model
                         :memory (MessageWindowChatMemory/withMaxMessages
                                  (or max-memory default-max-memory))
                         :tools tools}]
    (merge
     (dissoc config :initial-content)
     model-mem-tools
     {:content-atom (atom initial-content)
      :ai-service (create-service model-mem-tools)})))

