(ns clojure-mcp.repl-tools
  "MCP tools for interacting with a Clojure REPL environment.
   This namespace centralizes access to all the REPL tool functions."
  (:require
   [clojure-mcp.repl-tools.eval :as eval-tools]
   [clojure-mcp.repl-tools.history :as history-tools]
   [clojure-mcp.repl-tools.namespace :as namespace-tools]
   [clojure-mcp.repl-tools.symbol :as symbol-tools]
   [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as edit-tools]
   [clojure-mcp.repl-tools.project.inspect :as project-inspect]
   [clojure-mcp.repl-tools.filesystem.tools :as filesystem-tools]
   ;; New tool-system tools
   [clojure-mcp.tools.eval.tool :as new-eval-tool]
   [clojure-mcp.tools.read-file.tool :as new-read-file-tool]
   [clojure-mcp.tools.directory-tree.tool :as new-directory-tree-tool]
   [clojure-mcp.tools.grep.tool :as new-grep-tool]))

;; Centralized function for tool registration
(defn get-all-tools
  "Returns a list of all defined tools for registration with the MCP server."
  [nrepl-client-atom]
  (concat
   [#_(eval-tools/eval-code nrepl-client-atom)
    (namespace-tools/current-namespace nrepl-client-atom)
    (symbol-tools/symbol-completions nrepl-client-atom)
    (symbol-tools/symbol-metadata nrepl-client-atom)
    (symbol-tools/symbol-documentation nrepl-client-atom)
    (symbol-tools/source-code nrepl-client-atom)
    (symbol-tools/symbol-search nrepl-client-atom)
    (namespace-tools/list-namespaces nrepl-client-atom)
    (namespace-tools/list-vars-in-namespace nrepl-client-atom)
    (history-tools/eval-history nrepl-client-atom)
    (edit-tools/top-level-form-edit-tool nrepl-client-atom)
    (edit-tools/top-level-form-insert-before-tool nrepl-client-atom)
    (edit-tools/top-level-form-insert-after-tool nrepl-client-atom)
    (edit-tools/clojure-file-outline-tool nrepl-client-atom)
    (edit-tools/comment-block-edit-tool nrepl-client-atom)
    (edit-tools/docstring-edit-tool nrepl-client-atom)
    (project-inspect/inspect-project-tool nrepl-client-atom)
    ;; New tool-system tools - commented out for now to avoid name conflicts
    ;; Uncomment when ready to replace the original tools
    (new-eval-tool/eval-code nrepl-client-atom)
    (new-read-file-tool/read-file-tool nrepl-client-atom)
    (new-directory-tree-tool/directory-tree-tool nrepl-client-atom)
    (new-grep-tool/grep-tool nrepl-client-atom)]
   (filesystem-tools/get-all-filesystem-tools nrepl-client-atom)))

(comment
  ;; Example of testing tools directly
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  (clojure-mcp.nrepl/stop-polling @client-atom)

  (defn make-test-tool [{:keys [tool-fn] :as _tool-map}]
    (fn [arg-map]
      (let [prom (promise)]
        (tool-fn nil arg-map
                 (fn [res error]
                   (deliver prom {:res res :error error})))
        @prom)))

  ;; Example using tools directly from their namespaces
  (def edit-tester (make-test-tool (edit-tools/top-level-form-edit-tool client-atom)))
  (def eval-tester (make-test-tool (eval-tools/eval-code client-atom))))
