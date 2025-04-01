(ns clojure-mcp.linting
  (:require
   [clj-kondo.core :as kondo]
   [clojure.string :as string]))

(defn lint 
  "Lints Clojure code string using clj-kondo.
   Returns nil if no issues found, or map with :report and :error? keys."
  [form-str]
  (let [res (with-in-str form-str
              (kondo/run! {:lint "-" 
                           :config {:ignore [:unresolved-symbol
                                             :clj-kondo-config
                                             :deprecated-var
                                             :deprecated-namespace
                                             :deps.edn
                                             :bb.edn-undefined-task
                                             :bb.edn-cyclic-task-dependency
                                             :bb.edn-unexpected-key
                                             :bb.edn-task-missing-docstring
                                             :docstring-blank
                                             :docstring-no-summary
                                             :docstring-leading-trailing-whitespace
                                             :file
                                             #_:reduce-without-init
                                             :line-length
                                             :missing-docstring
                                             :namespace-name-mismatch
                                             :non-arg-vec-return-type-hint
                                             :private-call
                                             :redefined-var
                                             :redundant-ignore
                                             :schema-misplaced-return
                                             :java-static-field-call
                                             :unused-alias
                                             ;; :unused-binding ;; <-- Removed this line to enable the warning
                                             :unused-import
                                             :unresolved-namespace
                                             :unresolved-symbol
                                             :unresolved-var
                                             :unused-namespace
                                             :unused-private-var
                                             :unused-referred-var
                                             :use]}}))]
    (when (not-empty (:findings res))
      {:report (with-out-str
                 (kondo/print! res))
       :error? (some-> res :summary :error (> 0))})))
