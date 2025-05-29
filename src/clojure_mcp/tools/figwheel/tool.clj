(ns clojure-mcp.tools.figwheel.tool
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as string]
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.tools.eval.tool :as eval-tool]))

;; *ns* doesn't work on ClojureScript and its confusing for the LLM
(defn handle-ns-query [nrepl-client-atom figwheel-session f]
  (fn [x args callback]
    (if-let [code (:code args (get args "code"))]
      (if (= (string/trim code) "*ns*")
        (let [ns (nrepl/current-ns @nrepl-client-atom figwheel-session)]
          (callback [ns] false))
        (f x args callback))
      (f x args callback))))

(defn clojurescript-eval [nrepl-client-atom build]
  (let [figwheel-session (nrepl/new-session @nrepl-client-atom)
        start-code (format
                    "(do (require (quote figwheel.main)) (figwheel.main/start %s))"
                    (pr-str build))]
    (nrepl/eval-code-msg
     @nrepl-client-atom start-code {:session figwheel-session}
     (->> identity
          (nrepl/out-err #(log/info %) #(log/info %))
          (nrepl/value #(log/info %))
          (nrepl/done (fn [_] (log/info "done")))
          (nrepl/error (fn [args]
                         (log/info (pr-str args))
                         (log/info "ERROR in figwheel start")))))
    (-> (eval-tool/eval-code nrepl-client-atom {:nrepl-session figwheel-session})
        (update :tool-fn #(handle-ns-query nrepl-client-atom figwheel-session %))
        (assoc :name "clojurescript_eval")
        ;; We should validate the multiple expression case and throw an error
        (assoc :description
               "Takes a ClojureScript Expression and evaluates it in the current namespace. For example, providing `(+ 1 2)` will evaluate to 3.

**Project File Access**: Can load and use any ClojureScript file from your project with `(require '[your-namespace.core :as core] :reload)`. Always use `:reload` to ensure you get the latest version of files. Access functions, examine state with `@your-atom`, and manipulate application data for debugging and testing. 

**Important**: Both `require` and `ns` `:require` clauses can only reference actual files from your project, not namespaces created in the same REPL session.

**CRITICAL CONSTRAINT**: This ClojureScript REPL can only evaluate ONE expression. If multiple expressions are submitted the rest will be ignored. You can submit multiple expressions joined in a `(do ...)` block.

**Namespace Rules**: Namespaces must be declared at the top level separately:
```clojure
;; First evaluate namespace
(ns example.namespace)

;; Then evaluate functions  
(do 
  (defn add [a b] (+ a b))
  (add 5 9))
```

**WILL NOT WORK**:
```clojure
(do
  (ns example.namespace)
  (defn add [a b] (+ a b)))
```

JavaScript interop is fully supported including `js/console.log`, `js/setTimeout`, DOM APIs, etc.

**IMPORTANT**: This repl is intended for CLOJURESCRIPT CODE only."))))
