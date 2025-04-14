(ns clojure-mcp.repl-tools.comment-edit-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as edit]))

;; UPDATED: Test comments block
;; This is a modified multi-line comment
;; with new content

(defn dummy-function []
  "Just a placeholder function")

(comment
  ;; UPDATED: Example test usage
  (dummy-function)
  (println "Updated comment block")
  (+ 1 2 3))

;; Another comment block
;; for testing multiple comment blocks