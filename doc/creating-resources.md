# Creating Resources

This guide explains how to create resources for ClojureMCP using simple Clojure maps. This is the standard way to create resources, whether you're using them within ClojureMCP or as standalone components.

## What Are Resources?

Resources in MCP are read-only content that clients can access. Unlike tools (which perform actions) and prompts (which generate conversation), resources provide static or dynamic content like documentation, configuration files, or project information.

## Resource Registration Map Schema

A resource is defined as a Clojure map with the following structure:

```clojure
{:url         "custom://resource-id"    ; String: unique URI for the resource
 :name        "Resource Name"           ; String: human-readable name
 :description "Resource description"    ; String: what this resource provides
 :mime-type   "text/plain"             ; String: MIME type of the content
 :resource-fn (fn [exchange request callback] ...)} ; Function: resource implementation
```

### URL Field

The `:url` field should be a unique URI that identifies your resource. Common patterns:
- `custom://my-resource` - Custom scheme for your resources
- `file://path/to/file` - File-based resources
- `project://component` - Project-specific resources

### MIME Type Field

Common MIME types for resources:
- `"text/plain"` - Plain text files
- `"text/markdown"` - Markdown documents
- `"application/json"` - JSON data
- `"text/html"` - HTML content
- `"text/csv"` - CSV data

### Resource Function Signature

The `:resource-fn` has the following signature:

```clojure
(fn [exchange request callback]
  ;; Resource implementation
  )
```

Where:
- `exchange` - Typically ignored for simple resources (MCP exchange object)
- `request` - The request object (usually ignored for simple resources)
- `callback` - A function to call with the content

### Callback Function

The callback function expects one argument - a vector of strings:

```clojure
(callback ["content line 1" "content line 2" ...])
```

For single-string content, wrap it in a vector:

```clojure
(callback ["This is my resource content"])
```

## Simple Example: Static Text Resource

Here's a simple example of a static resource:

```clojure
(ns documentation-resources.core)

(def coding-guidelines-resource
  {:url "custom://coding-guidelines"
   
   :name "Coding Guidelines"
   
   :description "Project coding standards and best practices"
   
   :mime-type "text/markdown"
   
   :resource-fn (fn [_exchange _request callback]
                  (callback
                   ["# Coding Guidelines

## Naming Conventions
- Use kebab-case for function names
- Use PascalCase for protocols and records
- Prefix private functions with `-`

## Code Structure
- Keep functions under 20 lines
- One concept per function
- Clear, descriptive names

## Testing
- Write tests for all public functions
- Use generative testing where appropriate
- Aim for 80% code coverage"]))})
```

## Example: File-Based Resource

Here's an example that reads content from a file:

```clojure
(ns file-resources.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file-safely
  "Safely read a file and return its contents as a vector of strings."
  [file-path]
  (try
    (with-open [reader (io/reader file-path)]
      [(slurp reader)])
    (catch Exception e
      [(str "Error reading file: " (.getMessage e))])))

(def readme-resource
  {:url "custom://project-readme"
   
   :name "README.md"
   
   :description "Project README file with setup instructions"
   
   :mime-type "text/markdown"
   
   :resource-fn (fn [_exchange _request callback]
                  (let [content (read-file-safely "README.md")]
                    (callback content)))})
```

## Example: Dynamic Resource with Current Data

Resources can provide dynamic content that changes over time:

```clojure
(ns dynamic-resources.core
  (:require [clojure.data.json :as json])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn get-system-info []
  {:timestamp (.format (LocalDateTime/now) 
                       (DateTimeFormatter/ISO_LOCAL_DATE_TIME))
   :java-version (System/getProperty "java.version")
   :os-name (System/getProperty "os.name")
   :user-dir (System/getProperty "user.dir")
   :available-processors (.availableProcessors (Runtime/getRuntime))
   :free-memory (.freeMemory (Runtime/getRuntime))
   :total-memory (.totalMemory (Runtime/getRuntime))})

(def system-info-resource
  {:url "custom://system-info"
   
   :name "System Information"
   
   :description "Current system and JVM information"
   
   :mime-type "application/json"
   
   :resource-fn (fn [_exchange _request callback]
                  (try
                    (let [info (get-system-info)
                          json-str (json/write-str info {:indent true})]
                      (callback [json-str]))
                    (catch Exception e
                      (callback [(str "Error generating system info: " 
                                     (.getMessage e))]))))})
```

## Advanced Example: Multi-Part Resource

Resources can return multiple strings in the vector for multi-part content:

```clojure
(ns report-resources.core
  (:require [clojure.string :as str]))

(defn generate-project-report [project-data]
  ;; Returns a vector of report sections
  [(str "# Project Report\n\nGenerated: " (java.util.Date.))
   "\n## Overview\n"
   (str "Project: " (:name project-data))
   (str "Version: " (:version project-data))
   "\n## Dependencies\n"
   (str/join "\n" (map #(str "- " %) (:dependencies project-data)))
   "\n## Statistics\n"
   (str "Files: " (:file-count project-data))
   (str "Lines of Code: " (:loc project-data))])

(def project-report-resource
  {:url "custom://project-report"
   
   :name "Project Report"
   
   :description "Comprehensive project analysis report"
   
   :mime-type "text/markdown"
   
   :resource-fn (fn [_exchange _request callback]
                  (let [project-data {:name "my-project"
                                     :version "1.0.0"
                                     :dependencies ["org.clojure/clojure"
                                                   "ring/ring-core"
                                                   "compojure"]
                                     :file-count 42
                                     :loc 3500}
                        report-sections (generate-project-report project-data)]
                    (callback report-sections)))})
```

## Best Practices

1. **Unique URLs**: Ensure each resource has a unique URL to avoid conflicts.

2. **Error Handling**: Always handle errors gracefully and return error messages in the callback.

3. **Content Format**: Return content as a vector of strings, even for single-string content.

4. **MIME Types**: Use appropriate MIME types to help clients handle the content correctly.

5. **Performance**: Cache expensive computations if the resource is accessed frequently.

6. **Documentation**: Provide clear descriptions for what each resource contains.

## How Resources Are Accessed

Unlike tools and prompts, resources are typically accessed without parameters. The client simply requests the resource by its URL.

### Client Side (MCP Client)
The client sends a request like:
```json
{
  "method": "resources/read",
  "params": {
    "uri": "custom://coding-guidelines"
  }
}
```

### Server Side (Your Resource)
The MCP server calls your resource function:
```clojure
(resource-fn exchange request callback)
```

For simple resources, the `exchange` and `request` parameters are usually ignored since resources are identified by their URL and don't typically need additional parameters.

### Advanced: Using the Request Object

While most resources ignore the request parameter, advanced use cases might examine it:

```clojure
(def advanced-resource
  {:url "custom://versioned-doc"
   :name "Versioned Documentation"
   :description "Documentation that can show different versions"
   :mime-type "text/markdown"
   :resource-fn (fn [exchange request callback]
                  ;; In practice, the request object structure depends on the MCP 
                  ;; implementation, but you might extract version info if supported
                  (let [version "latest" ; Default version
                        content (case version
                                  "v1" "# Version 1 Documentation..."
                                  "v2" "# Version 2 Documentation..."
                                  "latest" "# Latest Documentation...")]
                    (callback [content])))})
```

## Testing Your Resource

You can test your resource function independently:

```clojure
(defn test-resource []
  ;; Create a test callback that prints results
  (let [test-callback (fn [result]
                        (println "Resource content:")
                        (doseq [line result]
                          (println line)))]
    ;; Test the resource
    ((:resource-fn coding-guidelines-resource) nil nil test-callback)))
```

## Integration with ClojureMCP

To use your resource with a ClojureMCP server:

```clojure
;; In the server's code
(require '[clojure-mcp.core :as mcp])
(require '[documentation-resources.core :as docs])

(defn setup-server [mcp-server]
  ;; Register your resource
  (mcp/add-resource mcp-server docs/coding-guidelines-resource))
```

## Common Resource Patterns

### 1. Configuration Resources
Provide configuration or settings information:

```clojure
(def config-resource
  {:url "custom://app-config"
   :name "Application Configuration"
   :description "Current application settings"
   :mime-type "application/json"
   :resource-fn (fn [_ _ callback]
                  (callback [(slurp "config.json")]))})
```

### 2. Generated Documentation
Create documentation on-the-fly:

```clojure
(defn generate-api-docs [api-endpoints]
  (str "# API Documentation\n\n"
       (str/join "\n\n" 
                 (map (fn [{:keys [method path description]}]
                        (format "## %s %s\n%s" method path description))
                      api-endpoints))))

(def api-docs-resource
  {:url "custom://api-docs"
   :name "API Documentation"
   :description "Auto-generated API documentation"
   :mime-type "text/markdown"
   :resource-fn (fn [_ _ callback]
                  (let [endpoints [{:method "GET" :path "/users" 
                                   :description "List all users"}
                                  {:method "POST" :path "/users" 
                                   :description "Create a new user"}]
                        docs (generate-api-docs endpoints)]
                    (callback [docs])))})
```

### 3. Aggregated Resources
Combine multiple sources into one resource:

```clojure
(defn read-files [file-paths]
  (map #(try (slurp %) 
             (catch Exception e (str "Error reading " % ": " (.getMessage e))))
       file-paths))

(def combined-docs-resource
  {:url "custom://all-docs"
   :name "All Documentation"
   :description "Combined documentation from multiple files"
   :mime-type "text/plain"
   :resource-fn (fn [_ _ callback]
                  (let [files ["README.md" "CONTRIBUTING.md" "LICENSE"]
                        contents (read-files files)
                        combined (str/join "\n\n---\n\n" contents)]
                    (callback [combined])))})
```

## Resource Discovery

When a client connects to your MCP server, it can discover available resources. Your resources will appear in the resource list with their URL, name, and description, making it easy for users to find and access the content they need.

Whether you're using resources within ClojureMCP or as standalone components, the structure and implementation remain the same, making them highly portable and reusable.
