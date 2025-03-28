(ns clojure-mcp.prompts
  "Prompt definitions for the MCP server"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn simple-content-prompt-fn
  "Returns a prompt-fn that ignores request arguments and returns
   a fixed description and a single assistant message with the given content."
  [description content]
  (fn [_ _ clj-result-k]
    (clj-result-k
     {:description description
      :messages [{:role :assistant :content content}]})))

(defn- load-prompt-from-resource
  "Loads prompt content from a classpath resource file."
  [filename]
  (if-let [resource (io/resource filename)]
    (slurp resource)
    (str "Error: Prompt file not found on classpath: " filename)))

;; --- Prompt Definitions ---

;; This is an example prompt to help document how to create a prompt with arguments

(def greeting-prompt
  {:name "simple-greeting"
   :description "Generates a simple greeting message."
   :arguments [{:name "personName"
                :description "The name of the person to greet."
                :required? true}
               {:name "mood"
                :description "The desired mood of the greeting (e.g., 'happy', 'formal')."
                :required? false}]
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

#_(io/resource "prompts/clojure_dev.txt")

(def clojure-dev-prompt
  {:name "clojure_dev"
   :description "Provides instructions and guidelines for Clojure development, including style and best practices."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "Clojure Development Guidelines"
               (str
                (load-prompt-from-resource "prompts/CLOJURE.md")
                (load-prompt-from-resource "prompts/clojure_dev.txt")))})

(def clojure-repl-driven-prompt
  {:name "clojure-repl-driven"
   :description "Provides comprehensive instructions for REPL-driven development in Clojure, including style, best practices, and REPL usage guidelines."
   :arguments [] ;; No arguments needed
   :prompt-fn (simple-content-prompt-fn
               "REPL-Driven Development Guide for Clojure"
               (str
                (load-prompt-from-resource "prompts/repl_driven.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/clojure-repl-guide.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/CLOJURE.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/clojure_dev.txt")))})



