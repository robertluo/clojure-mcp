(ns clojure-mcp.tools.glob-files.tool-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.glob-files.tool :as sut]
            [clojure-mcp.tool-system :as tool-system]
            [clojure.data.json :as json]))

(defn create-test-client
  "Creates a test client with the required settings"
  []
  (atom {:clojure-mcp.core/nrepl-user-dir (System/getProperty "user.dir")
         :clojure-mcp.core/allowed-directories [(System/getProperty "user.dir")]}))

(deftest tool-implementation-test
  (testing "Tool name, description and schema"
    (let [tool-config {:tool-type :glob-files}]
      (is (= "glob_files" (tool-system/tool-name tool-config)))
      (is (string? (tool-system/tool-description tool-config)))
      (is (> (count (tool-system/tool-description tool-config)) 10))
      (let [schema (tool-system/tool-schema tool-config)]
        (is (map? schema))
        (is (= #{:path :pattern} (set (:required schema))))
        (is (= #{:path :pattern :max_results} (set (keys (:properties schema))))))))

  (testing "Input validation"
    (let [client-atom (create-test-client)
          tool-config (sut/create-glob-files-tool client-atom)]

      ;; Valid input test
      (let [validated (tool-system/validate-inputs
                       tool-config
                       {:path "src" :pattern "**/*.clj" :max_results 10})]
        (is (map? validated))
        (is (= #{:path :pattern :max-results} (set (keys validated))))
        (is (string? (:path validated))))

      ;; Missing path test
      (is (thrown-with-msg?
           Exception #"Missing required parameter: path"
           (tool-system/validate-inputs tool-config {:pattern "**/*.clj"})))

      ;; Missing pattern test
      (is (thrown-with-msg?
           Exception #"Missing required parameter: pattern"
           (tool-system/validate-inputs tool-config {:path "src"})))

      ;; Path validation test
      (is (thrown?
           Exception
           (tool-system/validate-inputs tool-config
                                        {:path "/invalid/path" :pattern "**/*.clj"})))))

  (testing "Result formatting"
    (let [tool-config {:tool-type :glob-files}

          ;; Test successful result
          success-result {:filenames ["file1.clj" "file2.clj"]
                          :numFiles 2
                          :durationMs 10
                          :truncated false}
          formatted-success (tool-system/format-results tool-config success-result)
          parsed-success (json/read-str (first (:result formatted-success)))

          ;; Test error result
          error-result {:error "Test error message"}
          formatted-error (tool-system/format-results tool-config error-result)]

      ;; Check successful result format
      (is (false? (:error formatted-success)))
      (is (= 1 (count (:result formatted-success))))
      (is (string? (first (:result formatted-success))))
      (is (= 2 (get parsed-success "numFiles")))

      ;; Check error result format
      (is (true? (:error formatted-error)))
      (is (= ["Test error message"] (:result formatted-error))))))

(deftest registration-map-test
  (testing "Registration map generation"
    (let [client-atom (create-test-client)
          reg-map (sut/glob-files-tool client-atom)]
      (is (= "glob_files" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (string? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map)))

      ;; Test the actual function execution with a promise
      (let [p (promise)
            callback-fn (fn [result error] (deliver p {:result result :error error}))]

        ;; Call the tool-fn with valid params
        ((:tool-fn reg-map)
         nil
         {"path" (System/getProperty "user.dir") "pattern" "**/*.clj" "max_results" 2}
         callback-fn)

        ;; Check the result format
        (let [result @p
              parsed (json/read-str (first (:result result)))]
          (is (= 2 (count (get parsed "filenames"))))
          (is (= 2 (get parsed "numFiles")))
          (is (false? (:error result))))))))
