(ns clojure-mcp.tool-system
  "Core system for defining and registering MCP tools.
   This namespace provides multimethods for implementing tools 
   in a modular, extensible way."
  (:require
   [clojure.string :as string]))

;; Core multimethods for tool behavior

(defmulti tool-name 
  "Returns the name of the tool as a string. Dispatches on :tool-type."
  :tool-type)

(defmethod tool-name :default [tool-config]
  (-> tool-config
      :tool-type
      name
      (string/replace "-" "_")))

(defmulti tool-description 
  "Returns the description of the tool as a string. Dispatches on :tool-type."
  :tool-type)

(defmulti tool-schema 
  "Returns the parameter validation schema for the tool. Dispatches on :tool-type."
  :tool-type)

(defmulti validate-inputs 
  "Validates inputs against the schema and returns validated/coerced inputs.
   Throws exceptions for invalid inputs.
   Dispatches on :tool-type in the tool-config."
  (fn [tool-config inputs] (:tool-type tool-config)))

(defmulti execute-tool 
  "Executes the tool with the validated inputs and returns the result.
   Dispatches on :tool-type in the tool-config."
  (fn [tool-config inputs] (:tool-type tool-config)))

(defmulti format-results 
  "Formats the results from tool execution into the expected MCP response format.
   Dispatches on :tool-type in the tool-config."
  (fn [tool-config result] (:tool-type tool-config)))

;; Multimethod to assemble the registration map

(defmulti registration-map 
  "Creates the MCP registration map for a tool. 
   Dispatches on :tool-type."
  :tool-type)

;; Default implementation for registration-map

(defmethod registration-map :default [tool-config]
  {:name (tool-name tool-config)
   :description (tool-description tool-config)
   :schema (tool-schema tool-config)
   :handler (fn [params callback]
             (try
               (let [validated (validate-inputs tool-config params)
                     result (execute-tool tool-config validated)
                     formatted (format-results tool-config result)]
                 (callback formatted))
               (catch Exception e
                 (callback {:error (.getMessage e)}))))})