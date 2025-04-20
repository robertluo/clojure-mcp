(ns clojure-mcp.tools.file-write.tool-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [clojure-mcp.tools.file-write.tool :as file-write-tool]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(deftest tool-metadata-test
  (testing "Tool metadata multimethods"
    (let [tool-config (file-write-tool/create-file-write-tool (atom {}))]
      ;; Test tool-name
      (is (= "file_write" (tool-system/tool-name tool-config)))

      ;; Test tool-description
      (let [description (tool-system/tool-description tool-config)]
        (is (string? description))
        (is (str/includes? description "local filesystem"))
        (is (str/includes? description "linted and formatted")))

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
