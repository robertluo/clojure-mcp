(ns clojure-mcp.tools.file-write.tool-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.file-write.tool :as file-write-tool]
   [clojure-mcp.tools.file-write.core :as file-write-core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Setup test fixtures
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-client-atom* nil)

(defn create-test-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-tool-test")]
    ;; Create test directory
    (.mkdirs test-dir)
    
    ;; Create mock client atom with allowed directories for validation
    (let [client-atom (atom {:clojure-mcp.core/nrepl-user-dir (.getCanonicalPath test-dir)
                             :clojure-mcp.core/allowed-directories [(.getCanonicalPath test-dir)]})]
      ;; Bind dynamic vars for test
      (binding [*test-dir* test-dir
                *test-client-atom* client-atom]
        (try
          (f)
          (finally
            ;; Clean up
            (.delete test-dir)))))))

(use-fixtures :each create-test-fixture)

(deftest tool-metadata-test
  (testing "Tool metadata multimethods"
    (let [tool-config (file-write-tool/create-file-write-tool (atom {}))]
      ;; Test tool-name
      (is (= "file_write" (tool-system/tool-name tool-config)))

      ;; Test tool-description
      (let [description (tool-system/tool-description tool-config)]
        (is (string? description))
        (is (str/includes? description "local filesystem"))
        (is (str/includes? description "Content will be linted")))

      ;; Test tool-schema
      (let [schema (tool-system/tool-schema tool-config)]
        (is (map? schema))
        (is (= :object (:type schema)))
        (is (contains? (:properties schema) :file_path))
        (is (contains? (:properties schema) :content))
        (is (some #(= % :file_path) (:required schema)))
        (is (some #(= % :content) (:required schema)))))))

(deftest validate-inputs-test
  (testing "Validate inputs with missing parameters"
    (let [tool-config (file-write-tool/create-file-write-tool (atom {}))
          validate-fn #(tool-system/validate-inputs tool-config %)]

      ;; Test missing file_path
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Missing required parameter: file_path"
                            (validate-fn {:content "test content"})))

      ;; Test missing content
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Missing required parameter: content"
                            (validate-fn {:file_path "/tmp/test.txt"}))))))

(deftest format-results-test
  (testing "Format success results"
    (let [tool-config (file-write-tool/create-file-write-tool (atom {}))]

      ;; Test formatting success without diff
      (let [result {:error false
                    :type "create"
                    :file-path "/test/path/file.clj"
                    :diff ""}
            formatted (tool-system/format-results tool-config result)]
        (is (map? formatted))
        (is (vector? (:result formatted)))
        (is (= 1 (count (:result formatted))))
        (is (str/includes? (first (:result formatted)) "Clojure file created"))
        (is (not (:error formatted))))

      ;; Test formatting success with diff
      (let [result {:error false
                    :type "update"
                    :file-path "/test/path/file.txt"
                    :diff "- old content\n+ new content"}
            formatted (tool-system/format-results tool-config result)]
        (is (map? formatted))
        (is (vector? (:result formatted)))
        (is (= 1 (count (:result formatted))))
        (is (str/includes? (first (:result formatted)) "Text file updated"))
        (is (str/includes? (first (:result formatted)) "Changes:"))
        (is (str/includes? (first (:result formatted)) "- old content"))
        (is (not (:error formatted))))))

  (testing "Format error results"
    (let [tool-config (file-write-tool/create-file-write-tool (atom {}))]

      ;; Test formatting error
      (let [result {:error true
                    :message "Error writing file: Permission denied"}
            formatted (tool-system/format-results tool-config result)]
        (is (map? formatted))
        (is (vector? (:result formatted)))
        (is (= 1 (count (:result formatted))))
        (is (str/includes? (first (:result formatted)) "Error writing file"))
        (is (:error formatted))))))

(deftest registration-map-test
  (testing "Registration map"
    (let [reg-map (file-write-tool/file-write-tool (atom {}))]
      (is (map? reg-map))
      (is (= "file_write" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (string? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))

(deftest tool-fn-integration-test
  (testing "Tool-fn callback with valid Clojure code"
    (let [tool-config (file-write-tool/create-file-write-tool *test-client-atom*)
          reg-map (tool-system/registration-map tool-config)
          tool-fn (:tool-fn reg-map)
          file-path (str (.getCanonicalPath *test-dir*) "/valid-callback.clj")
          valid-content "(ns test.callback)\n\n(defn callback-fn [x]\n  (+ x 100))"
          
          ;; Create promise for callback result
          p (promise)
          callback (fn [result error] (deliver p {:result result :error error}))]
      
      ;; Execute tool function with callback
      (tool-fn nil {:file_path file-path :content valid-content} callback)
      
      ;; Wait for result (with timeout)
      (let [result (deref p 5000 {:error true :result ["Timeout waiting for callback"]})]
        ;; Validate result
        (is (not (:error result)))
        (is (vector? (:result result)))
        (is (or (str/includes? (first (:result result)) "Clojure file created")
                (str/includes? (first (:result result)) "Clojure file updated")))
        
        ;; Verify file exists and has correct content
        (is (.exists (io/file file-path)))
        (is (str/includes? (slurp file-path) "callback-fn")))))
  
  (testing "Tool-fn callback with invalid Clojure code"
    (let [tool-config (file-write-tool/create-file-write-tool *test-client-atom*)
          reg-map (tool-system/registration-map tool-config)
          tool-fn (:tool-fn reg-map)
          file-path (str (.getCanonicalPath *test-dir*) "/invalid-callback.clj")
          
          ;; First create a file with valid content
          _ (spit file-path "(ns test.original)\n\n(defn original-fn [] :ok)")
          original-content (slurp file-path)
          
          ;; Invalid content with syntax error
          invalid-content "(ns test.invalid-callback\n\n(defn syntax-error [] (+ 1 2)"
          
          ;; Create promise for callback result
          p (promise)
          callback (fn [result error] (deliver p {:result result :error error}))]
      
      ;; Execute tool function with callback
      (tool-fn nil {:file_path file-path :content invalid-content} callback)
      
      ;; Wait for result (with timeout)
      (let [result (deref p 5000 {:error true :result ["Timeout waiting for callback"]})]
        ;; Validate error result
        (is (:error result))
        (is (vector? (:result result)))
        (is (str/includes? (first (:result result)) "Syntax errors"))
        
        ;; Verify file still exists but was NOT modified
        (is (.exists (io/file file-path)))
        (is (= original-content (slurp file-path)))))))

(deftest integration-test
  (testing "End-to-end valid Clojure file write"
    (let [tool-config (file-write-tool/create-file-write-tool *test-client-atom*)
          file-path (str (.getCanonicalPath *test-dir*) "/valid.clj")
          valid-content "(ns test.valid)\n\n(defn valid-function [x]\n  (+ x 5))"
          
          ;; Execute the full pipeline from validation through execution to formatting results
          validated-inputs (tool-system/validate-inputs tool-config 
                                                     {:file_path file-path 
                                                      :content valid-content})
          execution-result (tool-system/execute-tool tool-config validated-inputs)
          formatted-result (tool-system/format-results tool-config execution-result)]
      
      ;; Test that tool-system validated and created proper structured inputs
      (is (string? (:file-path validated-inputs)))
      (is (= valid-content (:content validated-inputs)))
      
      ;; Test successful execution
      (is (not (:error execution-result)))
      (is (contains? #{"create" "update"} (:type execution-result)))
      (is (= file-path (:file-path execution-result)))
      (is (string? (:diff execution-result)))
      
      ;; Test formatted results 
      (is (map? formatted-result))
      (is (not (:error formatted-result)))
      (is (vector? (:result formatted-result)))
      (is (or (str/includes? (first (:result formatted-result)) "Clojure file created")
              (str/includes? (first (:result formatted-result)) "Clojure file updated")))
      
      ;; Verify file exists and has correct content
      (is (.exists (io/file file-path)))
      (is (= valid-content (slurp file-path)))))
      
  (testing "End-to-end invalid Clojure file write (linting failure)"
    (let [tool-config (file-write-tool/create-file-write-tool *test-client-atom*)
          file-path (str (.getCanonicalPath *test-dir*) "/invalid.clj")
          
          ;; Create a file first so we can test that it doesn't get overwritten
          _ (spit file-path "(ns test.valid)\n\n(defn original-fn [x] (* x 2))")
          original-content (slurp file-path)
          
          ;; Now try to overwrite with invalid content
          invalid-content "(ns test.invalid\n\n(defn broken-function [x]\n  (let [y (inc x]\n    (println y)))"
          
          ;; Execute the full pipeline from validation through execution to formatting results  
          validated-inputs (tool-system/validate-inputs tool-config 
                                                     {:file_path file-path 
                                                      :content invalid-content})
          execution-result (tool-system/execute-tool tool-config validated-inputs)
          formatted-result (tool-system/format-results tool-config execution-result)]
      
      ;; Test that tool-system validated and created proper structured inputs
      (is (string? (:file-path validated-inputs)))
      (is (= invalid-content (:content validated-inputs)))
      
      ;; Test error reported in execution
      (is (:error execution-result))
      (is (string? (:message execution-result)))
      (is (str/includes? (:message execution-result) "Syntax errors detected"))
      
      ;; Test formatted results reflect the error
      (is (map? formatted-result))
      (is (:error formatted-result))
      (is (vector? (:result formatted-result)))
      (is (str/includes? (first (:result formatted-result)) "Syntax errors"))
      
      ;; Verify file still exists but was NOT modified with the invalid content
      (is (.exists (io/file file-path)))
      (is (= original-content (slurp file-path)))
      (is (not= invalid-content (slurp file-path))))))
