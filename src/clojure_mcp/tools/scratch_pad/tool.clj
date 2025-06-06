(ns clojure-mcp.tools.scratch-pad.tool
  "Implementation of the scratch-pad tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.scratch-pad.core :as core]
   [clojure.tools.logging :as log]
   [clojure.walk]))

(defn get-scratch-pad
  "Gets the current scratch pad data from the nrepl-client.
   Returns an empty map if no data exists yet."
  [nrepl-client-atom]
  (get @nrepl-client-atom ::scratch-pad {}))

(defn update-scratch-pad!
  "Updates the scratch pad data in the nrepl-client-atom."
  [nrepl-client-atom f & args]
  (apply swap! nrepl-client-atom update ::scratch-pad f args))

(defmethod tool-system/tool-name :scratch-pad [_]
  "scratch_pad")

(defmethod tool-system/tool-description :scratch-pad [_]
  "A persistent scratch pad for storing structured data between tool calls. Accepts any JSON value (objects, arrays, strings, numbers, booleans, null) and stores them at nested paths using set_path, get_path, delete_path operations.

Your persistent workspace for planning, organizing thoughts, and maintaining state across tool invocations.

This tool can be used to:
 * develop and track multi-step plans
 * maintain todo lists and task tracking
 * store intermediate results between operations
 * keep notes about your current approach or strategy
 * maintain a list of files or resources to process
 * build up complex data structures incrementally
 * share context between different tool calls
 * any other persistent storage needs during your work

The user will not see the scratch pad contents unless you explicitly retrieve and display them. Use the chat to communicate with users; use the scratch pad to organize your own work.

CORE OPERATIONS:
- set_path: Store a value at a path, returns the parent container
- get_path: Retrieve a value from a path, returns the value or nil
- delete_path: Remove a value at a path, returns a confirmation message
- inspect: Display the entire structure (or a specific path) with truncation at specified depth

TRACKING PLANS WITH TODO LISTS:

Recommended todo item structure:
{
  task: \"Description of the task\",
  done: false,
  priority: \"high\", // optional: \"high\", \"medium\", \"low\"
  context: \"Additional details\" // optional
}

Adding todo items:
- First item:
  op: set_path
  path: [\"todos\" 0]
  value: {task: \"Write tests\", done: false}
  todo: \"todos\"
  explanation: Adding first task

- Next item:
  op: set_path
  path: [\"todos\" 1]
  value: {task: \"Review PR\", done: false, priority: \"high\"}
  todo: \"todos\"
  explanation: Adding high priority task

Adding multiple todo items at once:
- Entire map:
  op: set_path
  path: [\"todos\"]
  value: {
    0: {task: \"Write tests\", done: false, priority: \"high\"},
    1: {task: \"Review PR\", done: false, priority: \"high\"},
    2: {task: \"Update docs\", done: false, priority: \"medium\"}
  }
  todo: \"todos\"
  explanation: Adding multiple todos at once

Checking off completed tasks:
- Mark as done:
  op: set_path
  path: [\"todos\" 0 \"done\"]
  value: true
  todo: \"todos\"
  explanation: Completed writing tests

Deleting tasks:
- Remove entire task:
  op: delete_path
  path: [\"todos\" 0]
  explanation: Removing completed task

- Remove specific field:
  op: delete_path
  path: [\"todos\" 1 \"priority\"]
  explanation: Removing priority field

Viewing todos:
- All data:
  op: inspect
  explanation: Checking todo list

- Specific task:
  op: get_path
  path: [\"todos\" 0]
  explanation: Checking first task details

The \"todo\" parameter helps UI tools track and display task progress when working with todo lists.")

(defmethod tool-system/tool-schema :scratch-pad [_]
  {:type "object"
   :properties {"op" {:type "string"
                      :enum ["set_path" "get_path" "delete_path" "inspect"]
                      :description "The operation to perform either\n * set_path: set a value at a path\n * get_path: retrieve a value at a path\n * delete_path: remove the value AND the leaf key from the data structure\n * inspect: view the datastructure (or a specific path within it) up to a certain depth"}
                "path" {:type "array"
                        :items {:type ["string" "number"]}
                        :description "Path to the data location (array of string or number keys) this works for all operations including inspect"}
                "value" {:description "Value to store (for set_path). Can be any JSON value: object, array, string, number, boolean, or null."
                         :type ["object" "array" "string" "number" "boolean" "null"]}
                "explanation" {:type "string"
                               :description "Explanation of why this operation is being performed"}
                "todo" {:type "string"
                        :description "If you are adding marking deleting a todos or otherwise changing a todo this should be set to key of the todo list being operated on (ie.  \"rename_function_todos\") OR \"NOT_TODO\" if this isn't a todo operation"}
                "depth" {:type "number"
                         :description "(Optional) For inspect operation: Maximum depth to display (default: 5). Must be a positive integer."}}
   :required ["op" "explanation" "todo"]})

(defmethod tool-system/validate-inputs :scratch-pad [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [op path value explanation depth]} inputs]

    ;; Check required parameters
    (when-not op
      (throw (ex-info "Missing required parameter: op" {:inputs inputs})))

    (when-not explanation
      (throw (ex-info "Missing required parameter: explanation" {:inputs inputs})))

    ;; Validate operation
    (when-not (#{"set_path" "get_path" "delete_path" "inspect"} op)
      (throw (ex-info "Invalid operation. Must be one of: set_path, get_path, delete_path, inspect"
                      {:op op :inputs inputs})))

    ;; Operation-specific validation
    (case op
      "set_path" (do
                   (when-not path
                     (throw (ex-info "Missing required parameter for set_path: path" {:inputs inputs})))
                   (when-not (contains? inputs :value)
                     (throw (ex-info "Missing required parameter for set_path: value" {:inputs inputs}))))
      ("get_path" "delete_path") (when-not path
                                   (throw (ex-info (str "Missing required parameter for " op ": path")
                                                   {:inputs inputs})))
      "inspect" (when depth
                  (when-not (and (number? depth) (integer? depth) (pos? depth))
                    (throw (ex-info "Depth must be a positive integer greater than 0"
                                    {:depth depth :inputs inputs})))))

    ;; Validate path has at least one element when provided
    (when (and path (empty? path) (not= op "inspect"))
      (throw (ex-info "Path must have at least one element" {:path path :inputs inputs})))

    ;; Validate path elements are strings or numbers
    (when path
      (doseq [element path]
        (when-not (or (string? element) (number? element))
          (throw (ex-info "Path elements must be strings or numbers"
                          {:element element :type (type element) :path path})))))

    ;; Convert path to vector if needed (MCP provides as array)
    ;; And ensure depth is provided with default value for inspect
    (cond-> inputs
      path (assoc :path (vec path))
      (and (= op "inspect") (nil? depth)) (assoc :depth 5))))

(defmethod tool-system/execute-tool :scratch-pad [{:keys [nrepl-client-atom]} {:keys [op path value explanation todo depth]}]
  (try
    (let [current-data (get-scratch-pad nrepl-client-atom)
          exec-result (case op
                        "set_path" (let [{:keys [data result]} (core/execute-set-path current-data path value todo)]
                                     (update-scratch-pad! nrepl-client-atom (constantly data))
                                     ;; Add parent value to result
                                     (let [parent-path (butlast path)
                                           parent-value (if (empty? parent-path)
                                                          data
                                                          (get-in data parent-path))]
                                       (assoc result :parent-value parent-value)))

                        "get_path" (:result (core/execute-get-path current-data path))

                        "delete_path" (let [{:keys [data result]} (core/execute-delete-path current-data path)]
                                        (update-scratch-pad! nrepl-client-atom (constantly data))
                                        result)

                        "inspect" (:result (core/execute-inspect current-data depth path)))]
      {:result exec-result
       :explanation explanation
       :error false})
    (catch Exception e
      {:error true
       :message (str "Error executing scratch pad operation: " (.getMessage e))})))

;; this is convoluted this can be stream lined as most of this is imply echoing back what was sent
(defmethod tool-system/format-results :scratch-pad [_ {:keys [error message result explanation]}]
  (if error
    {:result [message]
     :error true}
    (cond
      ;; set_path - return pprinted parent value
      (:stored-at result)
      {:result [(with-out-str (clojure.pprint/pprint (:parent-value result)))]
       :error false}

      ;; get_path - return pprinted value
      (contains? result :value)
      {:result [(or (:pretty-value result) "nil")]
       :error false}

      ;; delete_path - return removed message only
      (:removed-from result)
      {:result [(str "Removed value at path " (:removed-from result))]
       :error false}

      ;; inspect - return pprinted truncated view only
      (:tree result)
      {:result [(:tree result)]
       :error false})))

;; this is needed because of the special handling of edn in the default handler
(defmethod tool-system/registration-map :scratch-pad [tool-config]
  {:name (tool-system/tool-name tool-config)
   :description (tool-system/tool-description tool-config)
   :schema (tool-system/tool-schema tool-config)
   :tool-fn (fn [_ params callback]
              (try
                (let [converted-params (tool-system/convert-java-collections params)
                      keywordized-params (-> converted-params
                                             (dissoc "value")
                                             tool-system/keywordize-keys-preserve-underscores)
                      {:keys [result error]}
                      (->> (get converted-params "value")
                           (assoc keywordized-params :value)
                           (tool-system/validate-inputs tool-config)
                           (tool-system/execute-tool tool-config)
                           (tool-system/format-results tool-config))]
                  (callback result error))
                (catch Exception e
                  (log/error e)
                  ;; On error, create a sequence of error messages
                  (let [error-msg (or (ex-message e) "Unknown error")
                        data (ex-data e)
                        ;; Construct error messages sequence
                        error-msgs (cond-> [error-msg]
                                     ;; Add any error-details from ex-data if available
                                     (and data (:error-details data))
                                     (concat (if (sequential? (:error-details data))
                                               (:error-details data)
                                               [(:error-details data)])))]
                    (callback error-msgs true)))))})

(defn create-scratch-pad-tool [nrepl-client-atom]
  {:tool-type :scratch-pad
   :nrepl-client-atom nrepl-client-atom})

(defn scratch-pad-tool
  "Returns the registration map for the scratch pad tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-scratch-pad-tool nrepl-client-atom)))

(comment
  ;; Usage examples with JSON values:

  ;; Store a simple string
  {:op "set_path"
   :path ["user" "name"]
   :value "Alice"
   :explanation "Setting user name"}

  ;; Store an object (JSON object becomes Clojure map with string keys)
  {:op "set_path"
   :path ["config"]
   :value {"theme" "dark" "fontSize" 14}
   :explanation "Storing user preferences"}

  ;; Store an array
  {:op "set_path"
   :path ["tags"]
   :value ["clojure" "mcp" "tools"]
   :explanation "Setting project tags"}

  ;; === TODO LIST WORKFLOW EXAMPLE ===

  ;; 1. Add first todo item (object with string keys)
  {:op "set_path"
   :path ["todos" 0]
   :value {"task" "Implement user authentication" "done" false "priority" "high"}
   :todo "todos"
   :explanation "Starting authentication work"}

  ;; 2. Add second todo
  {:op "set_path"
   :path ["todos" 1]
   :value {"task" "Write unit tests" "done" false}
   :todo "todos"
   :explanation "Adding testing task"}

  ;; 3. Check off first task (boolean value)
  {:op "set_path"
   :path ["todos" 0 "done"]
   :value true
   :todo "todos"
   :explanation "Completed authentication implementation"}

  ;; 4. Update task with additional context
  {:op "set_path"
   :path ["todos" 0 "context"]
   :value "Used OAuth2 with JWT tokens"
   :explanation "Adding implementation details"}

  ;; 5. Remove completed task
  {:op "delete_path"
   :path ["todos" 0]
   :explanation "Cleaning up completed task"}

  ;; 6. View all todos
  {:op "inspect"
   :explanation "Reviewing remaining tasks"}

  ;; === OTHER OPERATIONS ===

  ;; Get a specific value
  {:op "get_path"
   :path ["user" "name"]
   :explanation "Retrieving user name"}

  ;; Store nested data
  {:op "set_path"
   :path ["metrics" "performance"]
   :value {"cpu" 45.2 "memory" 78 "disk" 92}
   :explanation "Recording system metrics"}

  ;; Remove a value
  {:op "delete_path"
   :path ["config" "old-setting"]
   :explanation "Removing deprecated config"})
