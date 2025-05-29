(ns clojure-mcp.tools.directory-tree.tool
  "Implementation of the directory-tree tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.directory-tree.core :as core]
   [clojure-mcp.utils.valid-paths :as valid-paths]))

;; Factory function to create the tool configuration
(defn create-directory-tree-tool
  "Creates the directory-tree tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :directory-tree
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the directory-tree tool
(defmethod tool-system/tool-name :directory-tree [_]
  "LS")

(defmethod tool-system/tool-description :directory-tree [_]
  "Returns a recursive tree view of files and directories starting from the specified path. The path parameter must be an absolute path, not a relative path. You should generally prefer the `glob_files` and `fx_grep` tools, if you know which directories to search.")

(defmethod tool-system/tool-schema :directory-tree [_]
  {:type :object
   :properties {:path {:type :string}
                :max_depth {:type :integer
                            :description "Maximum depth to traverse (optional)"}
                :limit {:type :integer
                        :description "Maximum number of entries to show (default: 100)"}}
   :required [:path]})

(defmethod tool-system/validate-inputs :directory-tree [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path max_depth limit]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (valid-paths/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      (cond-> {:path validated-path}
        ;; Only include max_depth if provided
        max_depth (assoc :max_depth max_depth)
        ;; Only include limit if provided
        limit (assoc :limit limit)))))

(defmethod tool-system/execute-tool :directory-tree [_ inputs]
  (let [{:keys [path max_depth limit]} inputs]
    ;; We call our own implementation now, not fs-core
    (core/directory-tree path :max-depth max_depth :limit limit)))

(defmethod tool-system/format-results :directory-tree [_ result]
  (if (and (map? result) (:error result))
    ;; If there's an error, return it with error flag true
    {:result [(:error result)]
     :error true}
    ;; Otherwise, return the directory tree string
    {:result [result]
     :error false}))

;; Backward compatibility function that returns the registration map
(defn directory-tree-tool
  "Returns the registration map for the directory-tree tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-directory-tree-tool nrepl-client-atom)))
