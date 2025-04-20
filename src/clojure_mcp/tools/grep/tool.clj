(ns clojure-mcp.tools.grep.tool
  "Implementation of the grep tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.grep.core :as core]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure.data.json :as json]))

;; Factory function to create the tool configuration
(defn create-grep-tool
  "Creates the grep tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :grep
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the grep tool
(defmethod tool-system/tool-name :grep [_]
  "fs_grep")

(defmethod tool-system/tool-description :grep [_]
  "Fast content search tool that works with any codebase size.
Searches file contents using regular expressions.
Supports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.).
Filter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\").
Returns matching file paths sorted by modification time.
Use this tool when you need to find files containing specific patterns.")

(defmethod tool-system/tool-schema :grep [_]
  {:type :object
   :properties {:path {:type :string
                       :description "The directory to search in"}
                :pattern {:type :string
                          :description "The regular expression pattern to search for in file contents"}
                :include {:type :string
                          :description "File pattern to include in the search (e.g. \"*.clj\", \"*.{clj,cljs}\")"}
                :max_results {:type :integer
                              :description "Maximum number of results to return (default: 1000)"}}
   :required [:path :pattern]})

(defmethod tool-system/validate-inputs :grep [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path pattern include max_results]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    (when-not pattern
      (throw (ex-info "Missing required parameter: pattern" {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (utils/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      (cond-> {:path validated-path
               :pattern pattern}
        ;; Only include optional parameters if provided
        include (assoc :include include)
        max_results (assoc :max-results max_results)))))

(defmethod tool-system/execute-tool :grep [_ inputs]
  (let [{:keys [path pattern include max-results]} inputs]
    (core/grep-files path pattern
                     :include include
                     :max-results (or max-results 1000))))

(defmethod tool-system/format-results :grep [_ result]
  (if (:error result)
    ;; If there's an error, return it with error flag true
    {:result [(:error result)]
     :error true}
    ;; Otherwise, format the results as JSON
    (let [output (json/write-str
                  (if (nil? (:filenames result))
                    {:filenames []
                     :numFiles 0
                     :durationMs (:durationMs result)
                     :message "No matching files found. Try broadening your search pattern or path."}
                    {:filenames (:filenames result)
                     :numFiles (:numFiles result)
                     :durationMs (:durationMs result)})
                  :escape-slash false)]
      {:result [output]
       :error false})))

;; Backward compatibility function that returns the registration map
(defn grep-tool
  "Returns the registration map for the grep tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-grep-tool nrepl-client-atom)))