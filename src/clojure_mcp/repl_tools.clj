(ns clojure-mcp.repl-tools
  "MCP tools for interacting with a Clojure REPL environment.
   This namespace centralizes access to all the REPL tool functions."
  (:require
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
   [clojure-mcp.tools.project.tool :as new-project-tool]
   [clojure-mcp.tools.move-file.tool :as new-move-file-tool]
   [clojure-mcp.tools.create-directory.tool :as new-create-directory-tool]
   [clojure-mcp.tools.file-edit.tool :as new-file-edit-tool]
   [clojure-mcp.tools.unified-read-file.tool :as new-unified-read-file-tool]
   [clojure-mcp.tools.unified-clojure-edit.tool :as new-unified-clojure-edit-tool]
   [clojure-mcp.tools.think.tool :as new-think-tool]
   [clojure-mcp.tools.code-critique.tool :as new-code-critique-tool]
   [clojure-mcp.tools.dispatch-agent.tool :as agent-tool]
   [clojure-mcp.tools.dispatch-agent.tool :as new-dispatch-agent-tool]))

;; Centralized function for tool registration
(defn get-all-tools
  "Returns a list of all defined tools for registration with the MCP server."
  [nrepl-client-atom]

  [(new-directory-tree-tool/directory-tree-tool nrepl-client-atom)

   (new-unified-read-file-tool/unified-read-file-tool nrepl-client-atom)

   #_(new-unified-file-edit-tool/unified-file-edit-tool nrepl-client-atom)

   (new-unified-clojure-edit-tool/clojure-edit-tool nrepl-client-atom)

   (new-eval-tool/eval-code nrepl-client-atom)

   (new-think-tool/think-tool nrepl-client-atom)

   (new-code-critique-tool/code-critique-tool nrepl-client-atom)
   (agent-tool/dispatch-agent-tool nrepl-client-atom)
   #_(new-dispatch-agent-tool/dispatch-agent-tool nrepl-client-atom)

   (new-symbol-tool/symbol-search-tool nrepl-client-atom)

   (new-form-edit-tool/top-level-form-edit-tool nrepl-client-atom)
   (new-form-edit-tool/top-level-form-insert-before-tool nrepl-client-atom)
   (new-form-edit-tool/top-level-form-insert-after-tool nrepl-client-atom)
   (new-form-edit-tool/sexp-replace-tool nrepl-client-atom)
   (new-form-edit-tool/docstring-edit-tool nrepl-client-atom)
   (new-form-edit-tool/comment-block-edit-tool nrepl-client-atom)

   (new-project-tool/inspect-project-tool nrepl-client-atom)

   (new-move-file-tool/move-file-tool nrepl-client-atom)
   (new-create-directory-tool/create-directory-tool-registration nrepl-client-atom)

   (new-grep-tool/grep-tool nrepl-client-atom)
   (new-glob-files-tool/glob-files-tool nrepl-client-atom)

   #_(new-namespace-tool/current-namespace-tool nrepl-client-atom)
   #_(new-namespace-tool/list-namespaces-tool nrepl-client-atom)
   #_(new-namespace-tool/list-vars-in-namespace-tool nrepl-client-atom)
   #_(new-symbol-tool/symbol-completions-tool nrepl-client-atom)
   #_(new-symbol-tool/symbol-metadata-tool nrepl-client-atom)
   #_(new-symbol-tool/symbol-documentation-tool nrepl-client-atom)
   #_(new-symbol-tool/source-code-tool nrepl-client-atom)
   #_(new-form-edit-tool/clojure-file-outline-tool nrepl-client-atom)
   #_(new-read-file-tool/read-file-tool nrepl-client-atom)
   (new-file-edit-tool/file-edit-tool nrepl-client-atom)
   (new-file-write-tool/file-write-tool nrepl-client-atom)

   #_(new-list-directory-tool/list-directory-tool nrepl-client-atom)])

(comment
  ;; Example of testing tools directly
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)
  (clojure-mcp.nrepl/stop-polling @client-atom)
  (new-code-critique-tool/code-critique-tool client-atom)
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
  (def unified-read-tester (make-test-tool (new-unified-read-file-tool/unified-read-file-tool client-atom)))

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

  (outline-tester {"file_path" "/path/to/file.clj"})

  ;; Example usage of unified read file tool
  ;; Reading a Clojure file with intelligent structure awareness
  (unified-read-tester {"path" "/path/to/source.clj"})

  ;; Reading a Clojure file with specific functions expanded
  (unified-read-tester {"path" "/path/to/source.clj"
                        "expand_symbols" ["my-function" "another-function"]})

  ;; Reading a non-Clojure file with raw content
  (unified-read-tester {"path" "/path/to/config.json"})

  ;; Forcing raw mode for a Clojure file
  (unified-read-tester {"path" "/path/to/source.clj"
                        "clojure_mode" "off"})

  ;; Reading part of a file with line offset and limit
  (unified-read-tester {"path" "/path/to/log.txt"
                        "line_offset" 100
                        "limit" 50}))
