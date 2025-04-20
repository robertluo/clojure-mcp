(ns clojure-mcp.tools.grep.tool-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.grep.tool :as sut]
            [clojure-mcp.tool-system :as tool-system]
            [clojure.data.json :as json]))

(deftest tool-name-test
  (testing "tool-name returns the correct name"
    (is (= "fs_grep"
           (tool-system/tool-name {:tool-type :grep})))))

(deftest tool-description-test
  (testing "tool-description returns a non-empty description"
    (let [description (tool-system/tool-description {:tool-type :grep})]
      (is (string? description))
      (is (not (empty? description))))))

(deftest tool-schema-test
  (testing "tool-schema returns a valid schema with required parameters"
    (let [schema (tool-system/tool-schema {:tool-type :grep})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :path))
      (is (contains? (:properties schema) :pattern))
      (is (contains? (:properties schema) :include))
      (is (contains? (:properties schema) :max_results))
      (is (= [:path :pattern] (:required schema))))))

(deftest validate-inputs-test
  (testing "validate-inputs properly validates and transforms inputs"
    (let [nrepl-client {:clojure-mcp.core/nrepl-user-dir "/base/dir"
                        :clojure-mcp.core/allowed-directories ["/base/dir"]}
          nrepl-client-atom (atom nrepl-client)
          tool-config {:tool-type :grep
                       :nrepl-client-atom nrepl-client-atom}]

      (with-redefs [clojure-mcp.repl-tools.utils/validate-path-with-client
                    (fn [path _]
                      (str "/validated" path))]

        (testing "with only required parameters"
          (let [result (tool-system/validate-inputs
                        tool-config
                        {:path "/test/path" :pattern "test.*pattern"})]
            (is (= {:path "/validated/test/path"
                    :pattern "test.*pattern"} result))))

        (testing "with all parameters"
          (let [result (tool-system/validate-inputs
                        tool-config
                        {:path "/test/path"
                         :pattern "test.*pattern"
                         :include "*.clj"
                         :max_results 500})]
            (is (= {:path "/validated/test/path"
                    :pattern "test.*pattern"
                    :include "*.clj"
                    :max-results 500} result))))

        (testing "missing required path parameter"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Missing required parameter: path"
                                (tool-system/validate-inputs tool-config {:pattern "test"}))))

        (testing "missing required pattern parameter"
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Missing required parameter: pattern"
                                (tool-system/validate-inputs tool-config {:path "/test/path"}))))))))

(deftest execute-tool-test
  (testing "execute-tool calls core function with correct parameters"
    (with-redefs [clojure-mcp.tools.grep.core/grep-files
                  (fn [path pattern & {:keys [include max-results]}]
                    {:called-with {:path path
                                   :pattern pattern
                                   :include include
                                   :max-results max-results}})]

      (testing "with only required parameters"
        (let [result (tool-system/execute-tool
                      {:tool-type :grep}
                      {:path "/test/path" :pattern "test.*pattern"})]
          (is (= {:called-with {:path "/test/path"
                                :pattern "test.*pattern"
                                :include nil
                                :max-results 1000}} result))))

      (testing "with all parameters"
        (let [result (tool-system/execute-tool
                      {:tool-type :grep}
                      {:path "/test/path"
                       :pattern "test.*pattern"
                       :include "*.clj"
                       :max-results 500})]
          (is (= {:called-with {:path "/test/path"
                                :pattern "test.*pattern"
                                :include "*.clj"
                                :max-results 500}} result)))))))

(deftest format-results-test
  (testing "format-results correctly formats successful results with files"
    (let [result {:filenames ["file1.clj" "file2.clj"]
                  :numFiles 2
                  :durationMs 123}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result [(json/write-str result :escape-slash false)]
              :error false} formatted))))

  (testing "format-results correctly formats successful results with no files"
    (let [result {:filenames nil
                  :durationMs 42}
          formatted (tool-system/format-results {:tool-type :grep} result)
          parsed-json (json/read-str (first (:result formatted)))]
      (is (= false (:error formatted)))
      (is (= [] (get parsed-json "filenames")))
      (is (= 0 (get parsed-json "numFiles")))
      (is (= 42 (get parsed-json "durationMs")))
      (is (string? (get parsed-json "message")))))

  (testing "format-results correctly formats error results"
    (let [result {:error "Some error occurred"}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result ["Some error occurred"]
              :error true} formatted)))))

(deftest registration-map-test
  (testing "grep-tool returns a valid registration map"
    (let [nrepl-client-atom (atom {})
          reg-map (sut/grep-tool nrepl-client-atom)]
      (is (= "fs_grep" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (string? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))
