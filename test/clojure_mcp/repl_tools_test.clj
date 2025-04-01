(ns clojure-mcp.repl-tools-test
  (:require
   ;; Require all the individual test namespaces to ensure they're loaded
   [clojure-mcp.repl-tools.eval-test]
   [clojure-mcp.repl-tools.symbol-test]
   [clojure-mcp.repl-tools.namespace-test]
   [clojure-mcp.repl-tools.history-test]))

;; This file exists to maintain backward compatibility with the existing test structure.
;; All tests have been moved to individual files within the repl_tools directory.