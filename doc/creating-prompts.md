# Creating Prompts

This guide explains how to create prompts for ClojureMCP using simple Clojure maps. This is the standard way to create prompts, whether you're using them within ClojureMCP or as standalone components.

## What Are Prompts?

Prompts in MCP generate conversation contexts that help AI assistants understand specific tasks or workflows. They can provide instructions, set up scenarios, or guide the assistant through complex operations.

## Prompt Registration Map Schema

A prompt is defined as a Clojure map with the following structure:

```clojure
{:name        "prompt-name"         ; String: unique identifier for the prompt
 :description "Prompt description"  ; String: human-readable description
 :arguments   [...]                 ; Vector: argument definitions (can be empty)
 :prompt-fn   (fn [exchange request-args callback] ...)} ; Function: prompt implementation
```

### Arguments Field

The `:arguments` field is a vector of maps, where each map defines an argument:

```clojure
{:name        "arg-name"           ; String: argument identifier
 :description "Argument description" ; String: human-readable description
 :required?   true}                 ; Boolean: whether the argument is required
```

### Prompt Function Signature

The `:prompt-fn` has the following signature:

```clojure
(fn [exchange request-args callback]
  ;; Prompt implementation
  )
```

Where:
- `exchange` - Typically ignored for simple prompts (MCP exchange object)
- `request-args` - A Clojure map with string keys containing the prompt's arguments
- `callback` - A function to call with the result

### Callback Function

The callback function expects one argument - a map with the following structure:

```clojure
(callback {:description "Description of the result"
           :messages    [{:role :user :content "Message content"}
                         {:role :assistant :content "Response content"}]})
```

Where:
- `:description` - A string describing what the prompt produces
- `:messages` - A vector of message maps, each with:
  - `:role` - Either `:user`, `:assistant`, or `:system`
  - `:content` - The text content of the message

## Simple Example: Static Prompt

Here's a simple example of a prompt without arguments:

```clojure
(ns coding-prompts.core)

(def code-review-prompt
  {:name "code_review_guide"
   
   :description "Provides guidelines for conducting thorough code reviews"
   
   :arguments [] ; No arguments needed
   
   :prompt-fn (fn [_exchange _request-args callback]
                (callback
                 {:description "Code Review Guidelines"
                  :messages [{:role :assistant
                              :content "When reviewing code, please focus on:

1. **Correctness**: Does the code do what it's supposed to do?
2. **Clarity**: Is the code easy to understand and maintain?
3. **Performance**: Are there any obvious performance issues?
4. **Security**: Are there any security vulnerabilities?
5. **Style**: Does the code follow project conventions?

For each issue found:
- Explain why it's a problem
- Suggest a specific improvement
- Provide a code example when helpful"}]}))})
```

## Example with Arguments: Namespace Workflow

Here's an example that uses arguments to customize the prompt:

```clojure
(ns workflow-prompts.core
  (:require [clojure.string :as str]))

(defn create-namespace-sync-content [namespace-name]
  (format "Please synchronize the REPL with namespace: %s

Steps to follow:
1. Locate the namespace file using glob or grep
2. Load it with: (require '[%s] :reload)
3. Switch to it with: (in-ns '%s)
4. Verify the namespace is loaded correctly
5. List available functions with (dir %s)

This ensures the REPL has the latest version of the namespace."
          namespace-name namespace-name namespace-name namespace-name))

(def namespace-sync-prompt
  {:name "sync_namespace"
   
   :description "Generates instructions for synchronizing REPL with a specific namespace"
   
   :arguments [{:name "namespace"
                :description "The fully qualified Clojure namespace (e.g., 'myapp.core')"
                :required? true}]
   
   :prompt-fn (fn [_exchange request-args callback]
                (let [namespace-name (get request-args "namespace")]
                  (if namespace-name
                    (callback
                     {:description (str "Sync REPL with namespace: " namespace-name)
                      :messages [{:role :user
                                  :content (create-namespace-sync-content namespace-name)}]})
                    ;; Handle missing argument
                    (callback
                     {:description "Error: Missing namespace argument"
                      :messages [{:role :assistant
                                  :content "Error: The 'namespace' argument is required but was not provided."}]}))))})
```

## Advanced Example: Multi-Message Conversation

Prompts can return multiple messages to set up a conversation context:

```clojure
(ns teaching-prompts.core)

(def refactoring-lesson-prompt
  {:name "teach_refactoring"
   
   :description "Interactive lesson on refactoring Clojure code"
   
   :arguments [{:name "code_snippet"
                :description "The code to refactor"
                :required? true}
               {:name "focus_area"
                :description "Specific aspect to focus on (e.g., 'performance', 'readability')"
                :required? false}]
   
   :prompt-fn (fn [_exchange request-args callback]
                (let [code (get request-args "code_snippet")
                      focus (get request-args "focus_area" "general improvement")]
                  (if code
                    (callback
                     {:description "Interactive refactoring lesson"
                      :messages [{:role :user
                                  :content (str "I have this code that needs refactoring:\n\n```clojure\n" 
                                               code "\n```\n\n"
                                               "Focus area: " focus)}
                                 {:role :assistant
                                  :content "I'll help you refactor this code. Let me analyze it first..."}
                                 {:role :user
                                  :content "Please explain your analysis and suggest improvements step by step."}]})
                    (callback
                     {:description "Error: Missing code"
                      :messages [{:role :assistant
                                  :content "Please provide a code snippet to refactor."}]}))))})
```

## Utility Functions

You can create helper functions to simplify prompt creation:

```clojure
(defn simple-prompt
  "Creates a simple prompt with no arguments and a single message."
  [name description content]
  {:name name
   :description description
   :arguments []
   :prompt-fn (fn [_ _ callback]
                (callback
                 {:description description
                  :messages [{:role :assistant :content content}]}))})

;; Usage example
(def style-guide 
  (simple-prompt "clojure_style"
                 "Clojure style guidelines"
                 "Follow these Clojure style guidelines:
                  - Use kebab-case for function names
                  - Prefer threading macros for readability
                  - Keep functions small and focused"))
```

## Best Practices

1. **Clear Descriptions**: Provide clear descriptions for both the prompt and its arguments.

2. **Argument Validation**: Always validate required arguments before using them.

3. **Meaningful Messages**: Structure messages to provide clear context for the AI assistant.

4. **Error Handling**: Handle missing or invalid arguments gracefully.

5. **Message Roles**: Use appropriate roles (`:user`, `:assistant`, `:system`) to structure conversations.

6. **String Keys**: Remember that `request-args` uses string keys, not keywords.

## Testing Your Prompt

You can test your prompt function independently:

```clojure
(defn test-namespace-prompt []
  ;; Create a test callback that prints results
  (let [test-callback (fn [result]
                        (println "Description:" (:description result))
                        (println "Messages:")
                        (doseq [msg (:messages result)]
                          (println (str "  " (:role msg) ": " (:content msg)))))]
    ;; Test with valid input
    ((:prompt-fn namespace-sync-prompt) nil {"namespace" "myapp.core"} test-callback)
    
    ;; Test with missing input
    ((:prompt-fn namespace-sync-prompt) nil {} test-callback)))
```

## How Parameters Are Passed

When a client (like Claude Desktop) invokes a prompt, it passes parameters as a map. Here's how the flow works:

### Client Side (MCP Client)
The client sends a request with parameters like this:
```json
{
  "method": "prompts/get",
  "params": {
    "name": "sync_namespace",
    "arguments": {
      "namespace": "myapp.core"
    }
  }
}
```

### Server Side (Your Prompt)
The MCP server converts this to a Clojure map and passes it to your prompt function:
```clojure
;; Your prompt-fn receives:
;; request-args = {"namespace" "myapp.core"}
```

### Example Usage Flow

1. **Client lists available prompts** - discovers "sync_namespace" with its arguments
2. **Client invokes the prompt** with specific argument values:
   ```
   Prompt: sync_namespace
   Arguments: namespace="myapp.utils"
   ```
3. **Server calls your prompt-fn**:
   ```clojure
   (prompt-fn exchange {"namespace" "myapp.utils"} callback)
   ```
4. **Your prompt generates messages** based on the arguments
5. **Client receives the generated messages** and uses them in the conversation

### Testing Parameter Passing

Here's a more complete test example showing different parameter scenarios:

```clojure
(defn test-prompt-parameters []
  (let [test-callback (fn [result]
                        (println "\nResult:")
                        (println result))]
    
    ;; Test 1: All required parameters provided
    (println "Test 1: Valid parameters")
    ((:prompt-fn refactoring-lesson-prompt) 
     nil 
     {"code_snippet" "(defn add [a b] (+ a b))"
      "focus_area" "performance"} 
     test-callback)
    
    ;; Test 2: Optional parameter omitted (uses default)
    (println "\nTest 2: Optional parameter omitted")
    ((:prompt-fn refactoring-lesson-prompt) 
     nil 
     {"code_snippet" "(defn add [a b] (+ a b))"} 
     test-callback)
    
    ;; Test 3: Required parameter missing (error case)
    (println "\nTest 3: Missing required parameter")
    ((:prompt-fn refactoring-lesson-prompt) 
     nil 
     {"focus_area" "readability"} 
     test-callback)))
```

## Integration with ClojureMCP

To use your prompt with a ClojureMCP server:

```clojure
;; In the server's code
(require '[clojure-mcp.core :as mcp])
(require '[workflow-prompts.core :as workflow])

(defn setup-server [mcp-server]
  ;; Register your prompt
  (mcp/add-prompt mcp-server workflow/namespace-sync-prompt))
```

## Common Prompt Patterns

### 1. System Instructions
Prompts that provide consistent instructions or context:

```clojure
(def coding-standards
  {:name "coding_standards"
   :description "Project-specific coding standards"
   :arguments []
   :prompt-fn (fn [_ _ callback]
                (callback
                 {:description "Coding Standards"
                  :messages [{:role :system
                              :content "Apply these coding standards to all code..."}]}))})
```

### 2. Template-Based Prompts
Prompts that fill in templates with arguments:

```clojure
(defn template-prompt [template-string]
  (fn [_ request-args callback]
    (let [filled (reduce (fn [s [k v]]
                          (str/replace s (str "{{" k "}}") (str v)))
                        template-string
                        request-args)]
      (callback
       {:description "Filled template"
        :messages [{:role :user :content filled}]}))))

(def bug-report-prompt
  {:name "bug_report"
   :description "Generate a bug report"
   :arguments [{:name "component" :description "Affected component" :required? true}
               {:name "error" :description "Error message" :required? true}]
   :prompt-fn (template-prompt "Bug in {{component}}: {{error}}\nPlease investigate.")})
```

### 3. Conditional Prompts
Prompts that change based on arguments:

```clojure
(def adaptive-helper
  {:name "adaptive_helper"
   :description "Provides help based on user expertise level"
   :arguments [{:name "topic" :description "Topic to explain" :required? true}
               {:name "level" :description "Expertise level: beginner, intermediate, expert" :required? false}]
   :prompt-fn (fn [_ request-args callback]
                (let [topic (get request-args "topic")
                      level (get request-args "level" "intermediate")
                      intro (case level
                              "beginner" "Let me explain this in simple terms:"
                              "expert" "Here's a detailed technical explanation:"
                              "Let me provide a comprehensive overview:")]
                  (callback
                   {:description (str "Help with " topic)
                    :messages [{:role :user
                                :content (str intro "\nExplain " topic)}]})))})
```

Whether you're using prompts within ClojureMCP or as standalone components, the structure and implementation remain the same, making them highly portable and reusable.
