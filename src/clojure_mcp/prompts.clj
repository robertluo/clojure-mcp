(ns clojure-mcp.prompts
  "Prompt definitions for the MCP server"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [pogonos.core :as pg]))

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

(def clojure-system-repl-flex
  {:name "clojure_repl_flex_system_prompt"
   :description "Provides instructions and guidelines for Clojure development, including style and best practices."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "System Prompt: Clojure REPL Flex"
               (load-prompt-from-resource "prompts/system/clojure_flex.md"))})

(def clojure-system-repl
  {:name "clojure_repl_system_prompt"
   :description "Provides instructions and guidelines for Clojure development, including style and best practices."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "System Prompt: Clojure REPL"
               (load-prompt-from-resource "prompts/system/clojure_repl.md"))})

#_(def clojure-dev-prompt
    {:name "clojure_dev"
     :description "Provides instructions and guidelines for Clojure development, including style and best practices."
     :arguments [] ;; No arguments needed for this prompt
     :prompt-fn (simple-content-prompt-fn
                 "Clojure Development Guidelines"
                 (str
                  (load-prompt-from-resource "prompts/CLOJURE.md")
                  "\n\n---\n\n" ;; Separator
                  (load-prompt-from-resource "prompts/clojure_dev.txt")))})

#_(def clojure-repl-driven-prompt
    {:name "clojure-repl-driven"
     :description "Provides comprehensive instructions for REPL-driven development in Clojure, including style, best practices, and REPL usage guidelines."
     :arguments [] ;; No arguments needed
     :prompt-fn (simple-content-prompt-fn
                 "REPL-Driven Development Guide for Clojure"
                 (str
                ;(load-prompt-from-resource "prompts/CLOJURE.md")
                ; "\n\n---\n\n" ;; Separator
                ;(load-prompt-from-resource "prompts/clojure_dev.txt")
                ;"\n\n---\n\n" ;; Separator
                ;(load-prompt-from-resource "prompts/clojure-repl-guide.md")
                ;"\n\n---\n\n" ;; Separator
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

(defn sync-namespace-workflow-prompt [namesp]
  (format "I'm currently working on a Clojure namespace `%s`  

Can you:

1. find it 
2. `require` `:reload` it into the REPL environment 
3. change into the `%s` with `in-ns` 
4. and make an artifact for it

If the file get's *edited* outside and must be read to see the changes, you should `require` :reload the file into the REPL enviromnent."
          namesp
          namesp))

(def clj-sync-namespace
  {:name "clj-sync-namespace"
   :description "Generates a prompt instructing the assistant to synchronize the REPL with a specific namespace (require :reload, in-ns)."
   :arguments [{:name "namespace"
                :description "The fully qualified name of the Clojure namespace to sync."
                :required? true}]
   :prompt-fn (fn [_ request-args clj-result-k]
                (let [namespace-arg (get request-args "namespace")]
                  (clj-result-k
                   {:description (str "Sync REPL with namespace: " namespace-arg)
                    :messages [{:role :user
                                :content (sync-namespace-workflow-prompt namespace-arg)}]})))})

(defn create-project-summary [working-dir]
  {:name "create-project-summary"
   :description "Generates a prompt instructing the LLM to create a summary of a project."
   :arguments []
   :prompt-fn (fn [_ _ clj-result-k]
                (if (and working-dir
                         (let [f (io/file working-dir)]
                           (and (.exists f)
                                (.isDirectory f))))
                  (clj-result-k
                   {:description (str "Create project summary for: " working-dir)
                    :messages [{:role :user
                                :content
                                (pg/render-resource "prompts/create_project_summary.md"
                                                    {:root-directory
                                                     working-dir})}]})
                  (clj-result-k
                   {:description (str "Root directory not found.")
                    :messages [{:role :user
                                :content
                                (str "Root directory not provided So this will not be a prompt." "::" working-dir "::")}]})))})

;; Function to get all prompts for registration with the MCP server
(def clojure-edit-guide
  {:name "clojure_edit_guide"
   :description "Provides specialized guidance for using Clojure structure-aware editing tools instead of text editing."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "Clojure Structure-Aware Editing Guide"
               (load-prompt-from-resource "prompts/system/clojure_edit.md"))})

(def incremental-file-creation
  {:name "incremental_file_creation"
   :description "Guide for creating Clojure files incrementally to maximize success."
   :arguments [] ;; No arguments needed for this prompt
   :prompt-fn (simple-content-prompt-fn
               "Incremental File Creation for Clojure"
               (load-prompt-from-resource "prompts/system/incremental_file_creation.md"))})

(defn get-all-prompts
  "Returns a list of all defined prompts for registration with the MCP server.
   Takes an nrepl-client-atom for consistency with other similar functions,
   though current prompts don't use it."
  [nrepl-client-atom]
  [clojure-system-repl
   clojure-system-repl-flex
   clojure-edit-guide
   incremental-file-creation
   clj-sync-namespace
   (create-project-summary (:clojure-mcp.core/nrepl-user-dir @nrepl-client-atom))
   ;; Commented out prompts can be uncommented if needed
   #_clojure-dev-prompt
   #_clojure-repl-driven-prompt
   #_clojure-spec-driven-modifier
   #_clojure-test-driven-modifier
   #_clojure-project-context-modifier])


