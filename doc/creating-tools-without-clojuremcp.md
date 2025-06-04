# Creating Tools Without ClojureMCP Dependency

This guide explains how to create tools for ClojureMCP using simple Clojure maps, without requiring ClojureMCP as a library dependency. This approach allows you to create standalone tools that can be registered with any ClojureMCP server.

## Tool Registration Map Schema

A tool is defined as a Clojure map with the following structure:

```clojure
{:name        "tool-name"           ; String: unique identifier for the tool
 :description "Tool description"    ; String: human-readable description
 :schema      {...}                 ; Map: JSON Schema defining input parameters
 :tool-fn     (fn [exchange params callback] ...)} ; Function: tool implementation
```

### Schema Field

The `:schema` field should be a Clojure map that represents a JSON Schema. This defines the structure and validation rules for the tool's input parameters:

```clojure
{:type "object"
 :properties {"param1" {:type "string"
                        :description "Description of param1"}
              "param2" {:type "integer"
                        :description "Description of param2"}}
 :required ["param1"]}  ; List of required parameter names
```

### Tool Function Signature

The `:tool-fn` has the following signature:

```clojure
(fn [exchange params callback]
  ;; Tool implementation
  )
```

Where:
- `exchange` - Typically ignored for simple tools (can be used for advanced features)
- `params` - A Clojure map with string keys containing the tool's input parameters
- `callback` - A function to call with the results

### Callback Function

The callback function expects two arguments:

```clojure
(callback result-vector error-boolean)
```

Where:
- `result-vector` - A vector of strings containing the tool's output
- `error-boolean` - `true` if an error occurred, `false` otherwise

## Complete Example: Weather Tool

Here's a complete example of a weather tool that doesn't depend on ClojureMCP:

```clojure
(ns weather-tool.core
  (:require [clojure.string :as str]))

(defn fetch-weather
  "Mock function to fetch weather data. In a real implementation,
   this would call a weather API."
  [location]
  ;; Simulate weather data
  (case (str/lower-case location)
    "london"     {:temp 15 :condition "Partly cloudy" :humidity 65}
    "new york"   {:temp 22 :condition "Sunny" :humidity 45}
    "tokyo"      {:temp 18 :condition "Rainy" :humidity 80}
    "sydney"     {:temp 25 :condition "Clear" :humidity 55}
    ;; Default for unknown locations
    {:temp 20 :condition "Unknown" :humidity 50}))

(defn format-weather-report
  "Format weather data into a readable string."
  [{:keys [temp condition humidity]} location]
  (format "Weather in %s: %d°C, %s, Humidity: %d%%"
          location temp condition humidity))

(def weather-tool
  {:name "get_weather"
   
   :description "Get the current weather for a specified location"
   
   :schema {:type "object"
            :properties {"location" {:type "string"
                                    :description "The city or location to get weather for"}}
            :required ["location"]}
   
   :tool-fn (fn [_exchange params callback]
              (try
                ;; Extract location from params (string keys)
                (let [location (get params "location")]
                  (if location
                    ;; Fetch and format weather data
                    (let [weather-data (fetch-weather location)
                          report (format-weather-report weather-data location)]
                      ;; Success: return result vector with false error flag
                      (callback [report] false))
                    ;; Error: missing required parameter
                    (callback ["Error: Location parameter is required"] true)))
                ;; Handle any exceptions
                (catch Exception e
                  (callback [(str "Error fetching weather: " (.getMessage e))] true))))})

;; Example of registering the tool with a ClojureMCP server
;; (This would be done by the server, not the tool library)
#_
(defn register-weather-tool [mcp-server]
  (clojure-mcp.core/add-tool mcp-server weather-tool))
```

## Advanced Example: Calculator Tool

Here's a more complex example with multiple parameters and validation:

```clojure
(ns calculator-tool.core)

(defn safe-divide [a b]
  (if (zero? b)
    (throw (ex-info "Division by zero" {:a a :b b}))
    (/ a b)))

(def calculator-tool
  {:name "calculator"
   
   :description "Perform basic arithmetic operations"
   
   :schema {:type "object"
            :properties {"operation" {:type "string"
                                     :enum ["add" "subtract" "multiply" "divide"]
                                     :description "The arithmetic operation to perform"}
                         "a" {:type "number"
                              :description "First operand"}
                         "b" {:type "number"
                              :description "Second operand"}}
            :required ["operation" "a" "b"]}
   
   :tool-fn (fn [_exchange params callback]
              (try
                ;; Extract parameters
                (let [operation (get params "operation")
                      a (get params "a")
                      b (get params "b")]
                  ;; Validate all parameters are present
                  (cond
                    (nil? operation)
                    (callback ["Error: operation parameter is required"] true)
                    
                    (nil? a)
                    (callback ["Error: parameter 'a' is required"] true)
                    
                    (nil? b)
                    (callback ["Error: parameter 'b' is required"] true)
                    
                    :else
                    ;; Perform the calculation
                    (let [result (case operation
                                   "add"      (+ a b)
                                   "subtract" (- a b)
                                   "multiply" (* a b)
                                   "divide"   (safe-divide a b)
                                   (throw (ex-info "Invalid operation" 
                                                   {:operation operation})))]
                      (callback [(str "Result: " result)] false))))
                
                ;; Handle exceptions
                (catch Exception e
                  (callback [(str "Calculation error: " (.getMessage e))] true))))})
```

## Best Practices

1. **Parameter Validation**: Always validate that required parameters are present before processing.

2. **Error Handling**: Wrap your tool logic in try-catch blocks and use the error flag appropriately.

3. **Result Format**: Always return results as a vector of strings, even for single-line outputs.

4. **String Keys**: Remember that `params` uses string keys, not keywords.

5. **Pure Functions**: Keep your tool logic in pure functions that can be tested independently of the MCP framework.

6. **Documentation**: Provide clear descriptions for both the tool and its parameters.

## Testing Your Tool

You can test your tool function independently:

```clojure
(defn test-weather-tool []
  ;; Create a test callback that prints results
  (let [test-callback (fn [result error?]
                        (println "Result:" result)
                        (println "Error?" error?))]
    ;; Test with valid input
    ((:tool-fn weather-tool) nil {"location" "London"} test-callback)
    
    ;; Test with missing input
    ((:tool-fn weather-tool) nil {} test-callback)))
```

## Integration with ClojureMCP

To use your tool with a ClojureMCP server, the server administrator would simply add your tool map to their server configuration:

```clojure
;; In the server's code (not your tool library)
(require '[clojure-mcp.core :as mcp])
(require '[weather-tool.core :as weather])

(defn setup-server [mcp-server]
  ;; Register your tool
  (mcp/add-tool mcp-server weather/weather-tool))
```

This separation allows tool authors to create and distribute tools without any dependency on ClojureMCP, while server operators can easily integrate any compatible tools.
