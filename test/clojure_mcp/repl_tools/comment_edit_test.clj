(ns clojure-mcp.repl-tools.comment-edit-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.repl-tools.top-level-form-edit-pipeline :as edit]))

;; UPDATED: Test comments block
;; This is a modified multi-line comment
;; with new content

(defn dummy-function []
  "Just a placeholder function")

;; FINAL TEST BLOCK
;; This is the final test content
;; using the refactored pipeline

;; Another comment block
;; for testing multiple comment blocks