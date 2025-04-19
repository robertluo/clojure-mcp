(ns clojure-mcp.tool-system
  "Core system for defining and registering MCP tools.
   This namespace provides multimethods for implementing tools 
   in a modular, extensible way."
  (:require
   [clojure.string :as string]
   [clojure.walk :as walk]
   [clojure.data.json :as json]))

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
   Must return a map with :result (a string or vector of strings) and :error (boolean).
   
   This standardized format is then used by the tool-fn to call the callback with:
   (callback (:result formatted) (:error formatted))
   
   Dispatches on :tool-type in the tool-config."
  (fn [tool-config result] (:tool-type tool-config)))

;; Multimethod to assemble the registration map

(defmulti registration-map 
  "Creates the MCP registration map for a tool. 
   Dispatches on :tool-type."
  :tool-type)

;; Helper function to keywordize map keys while preserving underscores
(defn keywordize-keys-preserve-underscores 
  "Recursively transforms string map keys into keywords.
   Unlike clojure.walk/keywordize-keys, this preserves underscores."
  [m]
  (let [f (fn [[k v]]
            (if (string? k)
              [(keyword k) v]
              [k v]))]
    (walk/postwalk 
     (fn [x]
       (if (map? x)
         (into {} (map f x))
         x))
     m)))

;; Default implementation for registration-map
(defmethod registration-map :default [tool-config]
  {:name (tool-name tool-config)
   :description (tool-description tool-config)
   :schema (json/write-str (tool-schema tool-config))
   :tool-fn (fn [_ params callback]
              (try
                (let [;; Keywordize params from string keys to keywords 
                      ;; while preserving underscores
                      keywordized-params (keywordize-keys-preserve-underscores params)
                      validated (validate-inputs tool-config keywordized-params)
                      result (execute-tool tool-config validated)
                      ;; format-results must return {:result data :error boolean}
                      formatted (format-results tool-config result)]
                  ;; Use the formatted result with the callback
                  (callback (:result formatted) (:error formatted)))
                (catch Exception e
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