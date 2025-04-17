(ns clojure-mcp.repl-tools
  "MCP tools for interacting with a Clojure REPL environment.
   This namespace centralizes access to all the REPL tool functions."
  (:require
   [clojure-mcp.repl-tools.eval :as eval-tools]
   [clojure-mcp.repl-tools.history :as history-tools]
   [clojure-mcp.repl-tools.namespace :as namespace-tools]
   [clojure-mcp.repl-tools.symbol :as symbol-tools]
   [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as edit-tools]
   [clojure-mcp.repl-tools.project.inspect :as project-inspect]))

;; Re-export all tool functions

;; Eval tools
(def eval-code eval-tools/eval-code)
#_(def eval-history-push eval-tools/eval-history-push)
#_(def eval-history-reset eval-tools/eval-history-reset)

;; History tools
#_(def eval-history history-tools/eval-history)

;; Namespace tools
(def current-namespace namespace-tools/current-namespace)
(def list-namespaces namespace-tools/list-namespaces)
(def list-vars-in-namespace namespace-tools/list-vars-in-namespace)

;; Symbol exploration tools
(def symbol-completions symbol-tools/symbol-completions)
(def symbol-metadata symbol-tools/symbol-metadata)
(def symbol-documentation symbol-tools/symbol-documentation)
(def source-code symbol-tools/source-code)
(def symbol-search symbol-tools/symbol-search)

;; Code editing tools
(def clojure-edit-replace-form
  edit-tools/top-level-form-edit-tool)

(def clojure-edit-insert-before-form
  edit-tools/top-level-form-insert-before-tool)

(def clojure-edit-insert-after-form
  edit-tools/top-level-form-insert-after-tool)

(def clojure-file-outline
  edit-tools/clojure-file-outline-tool)

(def clojure-edit-comment-block
  edit-tools/comment-block-edit-tool)

(def clojure-edit-replace-docstring
  edit-tools/docstring-edit-tool)

;; Project inspection tool
(def clojure-inspect-project
  project-inspect/inspect-project-tool)

 ;; Function to get all tools for registration with the MCP server
(defn get-all-tools
  "Returns a list of all defined tools for registration with the MCP server."
  [nrepl-client-atom]
  [(eval-code nrepl-client-atom)
   (current-namespace nrepl-client-atom)
   (symbol-completions nrepl-client-atom)
   (symbol-metadata nrepl-client-atom)
   (symbol-documentation nrepl-client-atom)
   (source-code nrepl-client-atom)
   (symbol-search nrepl-client-atom)
   (list-namespaces nrepl-client-atom)
   (list-vars-in-namespace nrepl-client-atom)
   (history-tools/eval-history nrepl-client-atom) ;; Using history-tools directly since it's not re-exported
   (clojure-edit-replace-form nrepl-client-atom)
   (clojure-edit-insert-before-form nrepl-client-atom)
   (clojure-edit-insert-after-form nrepl-client-atom)
   (clojure-file-outline nrepl-client-atom)
   (clojure-edit-comment-block nrepl-client-atom)
   (clojure-edit-replace-docstring nrepl-client-atom)
   (clojure-inspect-project nrepl-client-atom)])

(comment
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

  (def edit-tester (make-test-tool (top-level-form-edit-tool client-atom)))
  (def eval-tester (make-test-tool (eval-code client-atom))))
