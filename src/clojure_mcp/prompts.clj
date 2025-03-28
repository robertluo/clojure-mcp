(ns clojure-mcp.prompts
  "Prompt definitions for the MCP server"
  (:require [clojure.string :as str]))

(def greeting-prompt
  {:name "simple-greeting"
   :description "Generates a simple greeting message."
   :arguments [{:name "personName" :description "The name of the person to greet." :required? true}
               {:name "mood" :description "The desired mood of the greeting (e.g., 'happy', 'formal')." :required? false}]
   :prompt-fn (fn [_ request-args clj-result-k]
                (let [person-name (get request-args "personName")
                      mood (get request-args "mood" "neutral") ; Default mood
                      greeting (case mood
                                 "happy" (str "Hey " person-name "! Hope you're having a great day!")
                                 "formal" (str "Good day, " person-name ".")
                                 (str "Hello, " person-name "."))]
                  ;; Call the continuation with the result map
                  (clj-result-k
                   {:description (str "A " mood " greeting for " person-name ".")
                    :messages [{:role :user :content (str "Generate a " mood " greeting for " person-name)} ;; Example user message
                               {:role :assistant :content greeting}]})))})
