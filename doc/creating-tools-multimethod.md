# Creating Tools with ClojureMCP's Multimethod System

This guide demonstrates how to create custom MCP tools using ClojureMCP's multimethod-based tool system. This approach provides structure, validation, and integration benefits when building tools within the ClojureMCP ecosystem.

> **📝 Note**: This guide covers the multimethod approach for creating tools. For alternative approaches, see:
> - [Creating Tools Without ClojureMCP](creating-tools-without-clojuremcp.md) - For simple map-based tools
> - [Creating Your Own Custom MCP Server](custom-mcp-server.md) - For integrating tools into your server

## Table of Contents

- [When to Use This Approach](#when-to-use-this-approach)
- [Architecture Overview](#architecture-overview)
- [Tool System Multimethods](#tool-system-multimethods)
- [Simple Tool Example](#simple-tool-example)
- [Complex Tool Example](#complex-tool-example)
- [Core vs Tool Separation](#core-vs-tool-separation)
- [Testing Your Tools](#testing-your-tools)
- [Integration with Your Custom Server](#integration-with-your-custom-server)
- [Best Practices](#best-practices)

## When to Use This Approach

Use the multimethod system when:
- You're building tools specifically for ClojureMCP
- You want structured validation and error handling
- You need to leverage ClojureMCP's infrastructure (nREPL client, path validation, etc.)
- You're contributing tools back to the ClojureMCP project
- You prefer the structure and guidance of the multimethod pattern

Use simple maps (see [Creating Tools Without ClojureMCP](creating-tools-without-clojuremcp.md)) when:
- You want maximum portability
- You're creating standalone tool libraries
- You need to share tools with non-ClojureMCP servers
- You prefer minimal dependencies

## Architecture Overview

The ClojureMCP tool system uses Clojure's multimethod dispatch to create a flexible, extensible architecture for tools. Each tool is implemented through a set of multimethods that define its behavior:

```
Tool Definition
├── Factory Function (creates tool config)
├── Multimethod Implementations
│   ├── tool-name
│   ├── tool-description  
│   ├── tool-schema
│   ├── validate-inputs
│   ├── execute-tool
│   └── format-results
└── Registration Function (returns MCP registration map)
```

## Tool System Multimethods

The tool system defines six core multimethods that every tool must implement:

### Required Multimethods

1. **`tool-name`** - Returns the tool name as a string
2. **`tool-description`** - Returns a description for the AI assistant
3. **`tool-schema`** - Defines the input parameter schema
4. **`validate-inputs`** - Validates and normalizes input parameters
5. **`execute-tool`** - Performs the actual tool operation
6. **`format-results`** - Formats results for the MCP protocol

### Multimethod Dispatch

All multimethods dispatch on the `:tool-type` key in the tool configuration:

```clojure
(defmethod tool-system/tool-name :my-custom-tool [_]
  "my_custom_tool")
```

## Simple Tool Example

Let's create a simple "echo" tool that demonstrates the basic pattern:

### Step 1: Create the Tool Namespace

```clojure
(ns my-project.tools.echo.tool
  "Simple echo tool that returns the input message."
  (:require
   [clojure-mcp.tool-system :as tool-system]))

;; Factory function to create the tool configuration
(defn create-echo-tool
  "Creates the echo tool configuration."
  []
  {:tool-type :echo})
```

### Step 2: Implement the Multimethods

```clojure
;; Tool name (as it appears to the AI)
(defmethod tool-system/tool-name :echo [_]
  "echo")

;; Description for the AI assistant
(defmethod tool-system/tool-description :echo [_]
  "Echo tool that returns the input message with optional prefix.
   
   Parameters:
   - message: The message to echo (required)
   - prefix: Optional prefix to add to the message")

;; Input schema validation
(defmethod tool-system/tool-schema :echo [_]
  {:type :object
   :properties {:message {:type :string
                          :description "The message to echo"}
                :prefix {:type :string
                         :description "Optional prefix for the message"}}
   :required [:message]})

;; Input validation and normalization
(defmethod tool-system/validate-inputs :echo [_ inputs]
  (let [{:keys [message prefix]} inputs]
    (when-not message
      (throw (ex-info "Missing required parameter: message" {:inputs inputs})))
    
    (when (not (string? message))
      (throw (ex-info "Message must be a string" {:inputs inputs})))
    
    {:message message
     :prefix (or prefix "")}))

;; Execute the tool operation
(defmethod tool-system/execute-tool :echo [_ inputs]
  (let [{:keys [message prefix]} inputs]
    {:echo-result (str prefix message)
     :original-message message
     :had-prefix (not (empty? prefix))}))

;; Format results for MCP protocol
(defmethod tool-system/format-results :echo [_ result]
  {:result [(str "Echo: " (:echo-result result))]
   :error false})
```

### Step 3: Create Registration Function

```clojure
;; Registration function
(defn echo-tool
  "Returns the registration map for the echo tool."
  []
  (tool-system/registration-map (create-echo-tool)))
```

## Complex Tool Example

Let's examine a more complex tool that interacts with the nREPL client:

### File Counter Tool

This tool counts lines in files with filtering capabilities:

```clojure
(ns my-project.tools.file-counter.tool
  "Tool for counting lines in files with filtering."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Factory function with dependencies
(defn create-file-counter-tool
  "Creates file counter tool with nREPL client dependency."
  [nrepl-client-atom]
  {:tool-type :file-counter
   :nrepl-client-atom nrepl-client-atom})

(defmethod tool-system/tool-name :file-counter [_]
  "file_counter")

(defmethod tool-system/tool-description :file-counter [_]
  "Count lines in files with optional filtering.
   
   Parameters:
   - path: Path to file or directory
   - pattern: Optional regex pattern to match lines
   - include_empty: Whether to include empty lines (default: true)")

(defmethod tool-system/tool-schema :file-counter [_]
  {:type :object
   :properties {:path {:type :string :description "Path to file or directory"}
                :pattern {:type :string :description "Regex pattern to match lines"}
                :include_empty {:type :boolean :description "Include empty lines"}}
   :required [:path]})

(defmethod tool-system/validate-inputs :file-counter [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path pattern include_empty]} inputs
        nrepl-client @nrepl-client-atom]
    
    ;; Validate path exists and is accessible
    (let [validated-path (valid-paths/validate-path-with-client path nrepl-client)
          file (io/file validated-path)]
      
      (when-not (.exists file)
        (throw (ex-info "File does not exist" {:path validated-path})))
      
      ;; Validate regex pattern if provided
      (when pattern
        (try (re-pattern pattern)
             (catch Exception e
               (throw (ex-info "Invalid regex pattern" 
                             {:pattern pattern :error (.getMessage e)})))))
      
      {:path validated-path
       :pattern pattern
       :include-empty (if (nil? include_empty) true include_empty)})))

(defmethod tool-system/execute-tool :file-counter [_ inputs]
  (let [{:keys [path pattern include-empty]} inputs
        file (io/file path)
        regex (when pattern (re-pattern pattern))]
    
    (if (.isDirectory file)
      ;; Handle directory
      (let [files (->> (file-seq file)
                       (filter #(.isFile %))
                       (filter #(str/ends-with? (.getName %) ".clj")))
            results (for [f files]
                      (let [lines (line-seq (io/reader f))
                            filtered-lines (cond->> lines
                                             (not include-empty) (remove str/blank?)
                                             regex (filter #(re-find regex %)))]
                        {:file (.getPath f)
                         :line-count (count filtered-lines)}))]
        {:type :directory
         :path path
         :total-files (count results)
         :total-lines (reduce + (map :line-count results))
         :file-results results})
      
      ;; Handle single file
      (let [lines (line-seq (io/reader file))
            filtered-lines (cond->> lines
                             (not include-empty) (remove str/blank?)
                             regex (filter #(re-find regex %)))]
        {:type :file
         :path path
         :line-count (count filtered-lines)
         :total-lines (count (line-seq (io/reader file)))}))))

(defmethod tool-system/format-results :file-counter [_ result]
  (case (:type result)
    :file
    {:result [(str "File: " (:path result))
              (str "Matching lines: " (:line-count result))
              (str "Total lines: " (:total-lines result))]
     :error false}
    
    :directory
    {:result (concat [(str "Directory: " (:path result))
                      (str "Files processed: " (:total-files result))
                      (str "Total matching lines: " (:total-lines result))
                      ""]
                     (map #(str "  " (:file %) ": " (:line-count %) " lines")
                          (:file-results result)))
     :error false}))

(defn file-counter-tool
  "Returns the registration map for the file counter tool."
  [nrepl-client-atom]
  (tool-system/registration-map (create-file-counter-tool nrepl-client-atom)))
```

## Core vs Tool Separation

For complex tools, separate the core functionality from MCP integration:

### Core Functionality (`core.clj`)

```clojure
(ns my-project.tools.file-counter.core
  "Core file counting functionality."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn count-lines-in-file
  "Count lines in a single file with optional filtering."
  [file-path & {:keys [pattern include-empty]
                :or {include-empty true}}]
  (let [file (io/file file-path)
        lines (line-seq (io/reader file))
        regex (when pattern (re-pattern pattern))]
    
    (when-not (.exists file)
      (throw (ex-info "File not found" {:path file-path})))
    
    (let [filtered-lines (cond->> lines
                           (not include-empty) (remove str/blank?)
                           regex (filter #(re-find regex %)))]
      {:path file-path
       :line-count (count filtered-lines)
       :total-lines (count lines)})))

(defn count-lines-in-directory
  "Count lines in all files in a directory."
  [dir-path & opts]
  (let [dir (io/file dir-path)
        files (->> (file-seq dir)
                   (filter #(.isFile %)))]
    
    (when-not (.exists dir)
      (throw (ex-info "Directory not found" {:path dir-path})))
    
    {:directory dir-path
     :files (map #(apply count-lines-in-file (.getPath %) opts) files)}))
```

### Tool Integration (`tool.clj`)

```clojure
(ns my-project.tools.file-counter.tool
  "MCP tool integration for file counter."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]
   [my-project.tools.file-counter.core :as core]
   [clojure.java.io :as io]))

;; Implementation uses core functions in execute-tool
(defmethod tool-system/execute-tool :file-counter [_ inputs]
  (let [{:keys [path pattern include-empty]} inputs
        file (io/file path)]
    (if (.isDirectory file)
      (core/count-lines-in-directory path 
                                     :pattern pattern 
                                     :include-empty include-empty)
      (core/count-lines-in-file path 
                                :pattern pattern 
                                :include-empty include-empty))))
```

## Testing Your Tools

### Unit Testing Core Functionality

```clojure
(ns my-project.tools.file-counter.core-test
  (:require
   [clojure.test :refer :all]
   [my-project.tools.file-counter.core :as core]
   [clojure.java.io :as io]))

(deftest test-count-lines
  (let [temp-file (io/file "test-file.txt")]
    (try
      (spit temp-file "line 1\nline 2\n\nline 4")
      (let [result (core/count-lines-in-file (.getPath temp-file))]
        (is (= 4 (:total-lines result)))
        (is (= 4 (:line-count result))))
      
      (let [result (core/count-lines-in-file (.getPath temp-file) 
                                             :include-empty false)]
        (is (= 3 (:line-count result))))
      
      (finally
        (.delete temp-file)))))
```

### Integration Testing with Tool System

```clojure
(ns my-project.tools.file-counter.tool-test
  (:require
   [clojure.test :refer :all]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.config :as config]
   [my-project.tools.file-counter.tool :as tool]
   [clojure.java.io :as io]))

(deftest test-tool-integration
  (let [test-dir (System/getProperty "user.dir")
        mock-client (atom {})
        _ (config/set-config! mock-client :nrepl-user-dir test-dir)
        _ (config/set-config! mock-client :allowed-directories [test-dir])
        tool-config (tool/create-file-counter-tool mock-client)
        
        ;; Test validation
        inputs {:path "README.md"}
        validated (tool-system/validate-inputs tool-config inputs)
        
        ;; Test execution
        result (tool-system/execute-tool tool-config validated)
        
        ;; Test formatting
        formatted (tool-system/format-results tool-config result)]
    
    (is (string? (:path validated)))
    (is (map? result))
    (is (vector? (:result formatted)))
    (is (false? (:error formatted)))))
```

### Manual REPL Testing

```clojure
(comment
  ;; Test in REPL
  (require '[my-project.tools.file-counter.tool :as tool])
  (require '[clojure-mcp.tool-system :as tool-system])
  (require '[clojure-mcp.config :as config])
  
  ;; Create tool with proper config setup
  (def test-dir (System/getProperty "user.dir"))
  (def mock-client (atom {}))
  (config/set-config! mock-client :nrepl-user-dir test-dir)
  (config/set-config! mock-client :allowed-directories [test-dir])
  (def tool-instance (tool/file-counter-tool mock-client))
  
  ;; Test the tool function directly
  (def tool-fn (:tool-fn tool-instance))
  (tool-fn nil {"path" "README.md"} 
           (fn [result error] 
             (println "Result:" result) 
             (println "Error:" error))))
```

## Integration with Your Custom Server

To add your multimethod-based tools to your custom MCP server, see [Creating Your Own Custom MCP Server](custom-mcp-server.md).

Here's a quick example:

```clojure
(ns my-company.custom-mcp-server
  (:require
   [clojure-mcp.core :as core]
   [clojure-mcp.main :as main]
   ;; Your custom tools
   [my-project.tools.file-counter.tool :as file-counter-tool]
   [my-project.tools.echo.tool :as echo-tool]))

(defn my-tools [nrepl-client-atom]
  ;; Combine main tools with your custom ones
  (concat
   (main/my-tools nrepl-client-atom)
   [(echo-tool/echo-tool)
    (file-counter-tool/file-counter-tool nrepl-client-atom)]))
```

> **💡 Tip**: Always check `src/clojure_mcp/main.clj` to see how the standard tools are integrated and organized.

## Best Practices

### 1. Error Handling

Always provide detailed error messages with context:

```clojure
(defmethod tool-system/validate-inputs :my-tool [_ inputs]
  (when-not (:required-param inputs)
    (throw (ex-info "Missing required parameter: required-param"
                    {:inputs inputs
                     :error-details "This parameter is needed for processing"}))))
```

### 2. Input Validation

Validate all inputs thoroughly:

```clojure
(defmethod tool-system/validate-inputs :my-tool [_ inputs]
  (let [{:keys [file-path timeout]} inputs]
    ;; Check required parameters
    (when-not file-path
      (throw (ex-info "Missing file path" {:inputs inputs})))
    
    ;; Validate types
    (when (and timeout (not (pos-int? timeout)))
      (throw (ex-info "Timeout must be a positive integer" {:inputs inputs})))
    
    ;; Normalize and return
    {:file-path (str file-path)
     :timeout (or timeout 5000)}))
```

### 3. Result Formatting

Always return results as a vector of strings:

```clojure
(defmethod tool-system/format-results :my-tool [_ result]
  {:result [(str "Processed: " (:file result))
            (str "Lines: " (:line-count result))]
   :error false})
```

### 4. Configuration and Dependencies

Use the factory function pattern for dependency injection:

```clojure
(defn create-my-tool
  "Creates tool with configurable options."
  [nrepl-client-atom & {:keys [timeout max-files]
                        :or {timeout 30000 max-files 100}}]
  {:tool-type :my-tool
   :nrepl-client-atom nrepl-client-atom
   :timeout timeout
   :max-files max-files})
```

### 5. Documentation

Provide comprehensive descriptions for AI assistants:

```clojure
(defmethod tool-system/tool-description :my-tool [_]
  "Detailed description of what the tool does.
   
   Parameters:
   - param1: Description of first parameter
   - param2: Description of second parameter
   
   Examples:
   - Basic usage: tool({param1: 'value'})
   - Advanced usage: tool({param1: 'value', param2: 'option'})
   
   Note: Any important usage notes or limitations.")
```

### 6. Separation of Concerns

- Keep core logic in `core.clj` files
- Use `tool.clj` only for MCP integration
- Write unit tests for core functionality
- Write integration tests for tool behavior

This architecture makes your tools:
- Testable in isolation
- Reusable outside of MCP context
- Easier to maintain and debug
- More modular and composable

## Choosing Your Approach

The multimethod system is powerful but not always necessary. Consider:

- **Use multimethods** when you need the structure and validation they provide
- **Use simple maps** when you need portability or simplicity
- **Mix approaches** - you can use both in the same server!

Whatever approach you choose, the key is creating tools that enhance your development workflow. The multimethod system provides a robust foundation for complex tools while maintaining consistency across the ClojureMCP ecosystem.
