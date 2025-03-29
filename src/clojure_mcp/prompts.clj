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

(def clojure-dev-prompt
  {:name "clojure_dev"
   :description "Provides instructions and guidelines for Clojure development, including style and best practices."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "Clojure Development Guidelines"
               (str
                (load-prompt-from-resource "prompts/CLOJURE.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/clojure_dev.txt")))})

(def clojure-repl-driven-prompt
  {:name "clojure-repl-driven"
   :description "Provides comprehensive instructions for REPL-driven development in Clojure, including style, best practices, and REPL usage guidelines."
   :arguments [] ;; No arguments needed
   :prompt-fn (simple-content-prompt-fn
               "REPL-Driven Development Guide for Clojure"
               (str
                (load-prompt-from-resource "prompts/CLOJURE.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/clojure_dev.txt")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/clojure-repl-guide.md")
                "\n\n---\n\n" ;; Separator
                (load-prompt-from-resource "prompts/repl_driven.md")))})

(def clojure-spec-driven-modifier
  {:name "clj-spec-driven-modifier"
   :description "Spec first modifer for REPL-driven development"
   :arguments [] ;; No arguments needed
   :prompt-fn (simple-content-prompt-fn
               "Spec-Driven-Development Modifier for Clojure"
               (load-prompt-from-resource "prompts/spec_modifier.md"))})

(def clojure-test-driven-modifier
  {:name "clj-test-driven-modifier"
   :description "Test driven modifer for REPL-driven development"
   :arguments [] ;; No arguments needed
   :prompt-fn (simple-content-prompt-fn
               "Test-Driven-Development Modifier for Clojure"
               (load-prompt-from-resource "prompts/test_modifier.md"))})

(defn working-dir-prompt [dir]
  (str
   "The Clojure project assocated with the code we are developing is located at `"
   dir
   "`   
When interacting with filesystem for this code it will be in this project.

Filesystem writes, saves, edits and other interactions should use the `filesystem` tool rather than the Clojure REPL."
   ;; If the filesystem tool can't acces this directory then fallback to the REPL for filesystem access
   ))

(def clojure-project-context-modifier
  {:name "clj-set-project-dir"
   :description "Set the project context of the code we are working on"
   :arguments [{:name "project-working-directory"
                :description "The root directory of your Clojure project"
                :required? true}] ;; No arguments needed
   :prompt-fn (fn [_ request-args clj-result-k]
                (let [working-directory (get request-args "project-working-directory")]
                  ;; Call the continuation with the result map
                  (clj-result-k
                   {:description "Set the working directory of a project"
                    :messages [{:role :user
                                :content (working-dir-prompt working-directory)}
                               #_{:role :assistant
                                  :content (str "Working directory set to " working-directory "\n all filesytem interactions will be focused here.")}]})))})


