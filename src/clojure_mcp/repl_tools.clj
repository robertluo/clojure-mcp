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
   ;; New tool-system tools
   [clojure-mcp.tools.eval.tool :as new-eval-tool]
   [clojure-mcp.tools.read-file.tool :as new-read-file-tool]
   [clojure-mcp.tools.directory-tree.tool :as new-directory-tree-tool]
   [clojure-mcp.tools.grep.tool :as new-grep-tool]
   [clojure-mcp.tools.glob-files.tool :as new-glob-files-tool]
   [clojure-mcp.tools.file-write.tool :as new-file-write-tool]
   [clojure-mcp.tools.list-directory.tool :as new-list-directory-tool]
   [clojure-mcp.tools.namespace.tool :as new-namespace-tool]
   [clojure-mcp.tools.symbol.tool :as new-symbol-tool]
   [clojure-mcp.tools.form-edit.tool :as new-form-edit-tool]
   [clojure-mcp.tools.project.tool :as new-project-tool]))

;; Centralized function for tool registration
(defn get-all-tools
  "Returns a list of all defined tools for registration with the MCP server."
  [nrepl-client-atom]
  (concat
   [#_(eval-tools/eval-code nrepl-client-atom)
    #_(namespace-tools/current-namespace nrepl-client-atom)
    #_(symbol-tools/symbol-completions nrepl-client-atom)
    #_(symbol-tools/symbol-metadata nrepl-client-atom)
    #_(symbol-tools/symbol-documentation nrepl-client-atom)
    #_(symbol-tools/source-code nrepl-client-atom)
    #_(symbol-tools/symbol-search nrepl-client-atom)
    #_(namespace-tools/list-namespaces nrepl-client-atom)
    #_(namespace-tools/list-vars-in-namespace nrepl-client-atom)
    (history-tools/eval-history nrepl-client-atom)
    ;; Commented out old form editing tools in favor of new multimethod-based implementations
    #_(edit-tools/top-level-form-edit-tool nrepl-client-atom)
    #_(edit-tools/top-level-form-insert-before-tool nrepl-client-atom)
    #_(edit-tools/top-level-form-insert-after-tool nrepl-client-atom)
    #_(edit-tools/clojure-file-outline-tool nrepl-client-atom)
    #_(edit-tools/comment-block-edit-tool nrepl-client-atom)
    #_(edit-tools/docstring-edit-tool nrepl-client-atom)
    #_(project-inspect/inspect-project-tool nrepl-client-atom) ;; Deprecated version
    ;; New tool-system tools
    (new-eval-tool/eval-code nrepl-client-atom)
    (new-read-file-tool/read-file-tool nrepl-client-atom)
    (new-directory-tree-tool/directory-tree-tool nrepl-client-atom)
    (new-grep-tool/grep-tool nrepl-client-atom)
    (new-glob-files-tool/glob-files-tool nrepl-client-atom)
    (new-file-write-tool/file-write-tool nrepl-client-atom)
    (new-list-directory-tool/list-directory-tool nrepl-client-atom)
    (new-namespace-tool/current-namespace-tool nrepl-client-atom)
    (new-namespace-tool/list-namespaces-tool nrepl-client-atom)
    (new-namespace-tool/list-vars-in-namespace-tool nrepl-client-atom)
    (new-symbol-tool/symbol-completions-tool nrepl-client-atom)
    (new-symbol-tool/symbol-metadata-tool nrepl-client-atom)
    (new-symbol-tool/symbol-documentation-tool nrepl-client-atom)
    (new-symbol-tool/source-code-tool nrepl-client-atom)
    (new-symbol-tool/symbol-search-tool nrepl-client-atom)
    ;; New form editing tools
    (new-form-edit-tool/top-level-form-edit-tool nrepl-client-atom)
    (new-form-edit-tool/top-level-form-insert-before-tool nrepl-client-atom)
    (new-form-edit-tool/top-level-form-insert-after-tool nrepl-client-atom)
    (new-form-edit-tool/docstring-edit-tool nrepl-client-atom)
    (new-form-edit-tool/comment-block-edit-tool nrepl-client-atom)
    (new-form-edit-tool/clojure-file-outline-tool nrepl-client-atom)
    ;; New project inspection tool
    (new-project-tool/inspect-project-tool nrepl-client-atom)]
   ;; All tools have been refactored to the new architecture!
   []))

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

  ;; Example using old tools directly from their namespaces
  (def old-edit-tester (make-test-tool (edit-tools/top-level-form-edit-tool client-atom)))
  (def old-eval-tester (make-test-tool (eval-tools/eval-code client-atom)))
  
  ;; Example using new multimethod-based tools
  (def edit-tester (make-test-tool (new-form-edit-tool/top-level-form-edit-tool client-atom)))
  (def docstring-tester (make-test-tool (new-form-edit-tool/docstring-edit-tool client-atom)))
  (def comment-tester (make-test-tool (new-form-edit-tool/comment-block-edit-tool client-atom)))
  (def outline-tester (make-test-tool (new-form-edit-tool/clojure-file-outline-tool client-atom)))
  
  ;; Example usage of new form editing tools
  (edit-tester {"file_path" "/path/to/file.clj"
                "form_name" "example-fn"
                "form_type" "defn"
                "content" "(defn example-fn [x] (* x 2))"})
  
  (docstring-tester {"file_path" "/path/to/file.clj"
                     "form_name" "example-fn"
                     "form_type" "defn"
                     "docstring" "Updated documentation for the function"})
  
  (comment-tester {"file_path" "/path/to/file.clj"
                   "comment_substring" "TODO"
                   "new_content" ";; DONE: implemented feature"})
  
  (outline-tester {"file_path" "/path/to/file.clj"}))
