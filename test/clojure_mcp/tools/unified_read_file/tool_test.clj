(ns clojure-mcp.tools.unified-read-file.tool-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.test-utils :as test-utils :refer [*nrepl-client-atom*]]
   [clojure-mcp.tools.unified-read-file.tool :as unified-read-file-tool]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Setup test fixtures
(test-utils/apply-fixtures *ns*)

;; Setup test files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)
(def ^:dynamic *clojure-test-file* nil)

(defn setup-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-unified-read-test")
        test-file (io/file test-dir "test-file.txt")
        clojure-test-file (io/file test-dir "test-file.clj")]

    ;; Create test directory and files
    (.mkdirs test-dir)

    ;; Create plain text file
    (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")

    ;; Create Clojure test file with some functions
    (spit clojure-test-file
          "(ns test-namespace
  \"Test namespace docstring\"
  (:require [clojure.string :as str]))

(defn test-function
  \"Test function docstring\"
  [x y]
  (+ x y))

(defn another-function
  [a b]
  (* a b))

(defmethod my-multimethod :test-dispatch
  [x]
  (inc x))

(def my-var 42)

(comment
  (test-function 1 2)
  (another-function 3 4))
")

    ;; Set allowed directories for path validation
    (swap! *nrepl-client-atom* assoc
           :clojure-mcp.core/nrepl-user-dir (.getAbsolutePath test-dir)
           :clojure-mcp.core/allowed-directories [(.getAbsolutePath test-dir)])

    ;; Run test with fixtures bound
    (binding [*test-dir* test-dir
              *test-file* test-file
              *clojure-test-file* clojure-test-file]
      (try
        (f)
        (finally
          ;; Clean up
          (when (.exists test-file)
            (.delete test-file))
          (when (.exists clojure-test-file)
            (.delete clojure-test-file))
          (when (.exists test-dir)
            (.delete test-dir)))))))

(use-fixtures :each setup-test-files-fixture)

(deftest tool-registration-test
  (testing "Tool registration produces a proper map"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          reg-map (tool-system/registration-map tool-instance)]
      (is (map? reg-map))
      (is (= "read_file" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (string? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))

(deftest validate-inputs-test
  (testing "Validate accepts valid input with minimal parameters"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          result (tool-system/validate-inputs
                  tool-instance
                  {:path (.getAbsolutePath *test-file*)})]
      (is (map? result))
      (is (string? (:path result)))
      (is (= "auto" (:clojure_mode result)))
      (is (vector? (:expand_symbols result)))
      (is (empty? (:expand_symbols result)))
      (is (zero? (:line_offset result)))
      (is (nil? (:limit result)))))

  (testing "Validate accepts valid input with all parameters"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          result (tool-system/validate-inputs
                  tool-instance
                  {:path (.getAbsolutePath *test-file*)
                   :clojure_mode "off"
                   :expand_symbols ["test-function"]
                   :line_offset 2
                   :limit 10})]
      (is (map? result))
      (is (string? (:path result)))
      (is (= "off" (:clojure_mode result)))
      (is (vector? (:expand_symbols result)))
      (is (= ["test-function"] (:expand_symbols result)))
      (is (= 2 (:line_offset result)))
      (is (= 10 (:limit result)))))

  (testing "Validate rejects missing path parameter"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)]
      (is (thrown? Exception (tool-system/validate-inputs tool-instance {})))))

  (testing "Validate rejects invalid clojure_mode parameter"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:path (.getAbsolutePath *test-file*)
                     :clojure_mode "invalid"})))))

  (testing "Validate rejects paths outside allowed directories"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:path "/etc/passwd"}))))))

(deftest execute-tool-test
  (testing "Execute returns regular file contents in raw mode"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *test-file*)
                     :clojure_mode "off"
                     :expand_symbols []
                     :line_offset 0}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (= :raw (:mode result)))
      (is (contains? result :content))
      (is (contains? result :path))
      (is (str/includes? (:content result) "Line 1"))
      (is (not (:error result)))))

  (testing "Execute returns Clojure file contents in Clojure mode"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *clojure-test-file*)
                     :clojure_mode "on"
                     :expand_symbols []
                     :line_offset 0}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (= :clojure (:mode result)))
      (is (contains? result :content))
      (is (contains? result :path))
      (is (or (str/includes? (:content result) "(defn test-function [x y] ...)")
              (str/includes? (:content result) "(defn test-function")))
      (is (not (:error result)))))

  (testing "Execute with expand_symbols option shows complete function"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *clojure-test-file*)
                     :clojure_mode "on"
                     :expand_symbols ["test-function"]
                     :line_offset 0}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (= :clojure (:mode result)))
      (is (contains? result :content))
      ;; Full implementation should be present, not collapsed
      (is (str/includes? (:content result) "(defn test-function"))
      (is (str/includes? (:content result) "\"Test function docstring\""))
      (is (str/includes? (:content result) "(+ x y)"))
      ;; Other functions should still be collapsed
      (is (or (str/includes? (:content result) "(defn another-function [a b] ...)")
              (str/includes? (:content result) "(defn another-function")))
      (is (not (:error result)))))

  (testing "Execute uses auto mode detection for file types"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)]

      ;; Test with regular file (should use raw mode)
      (let [validated {:path (.getAbsolutePath *test-file*)
                       :clojure_mode "auto"
                       :expand_symbols []
                       :line_offset 0}
            result (tool-system/execute-tool tool-instance validated)]
        (is (= :raw (:mode result)))
        (is (not (:error result))))

      ;; Test with Clojure file (should use Clojure mode)
      (let [validated {:path (.getAbsolutePath *clojure-test-file*)
                       :clojure_mode "auto"
                       :expand_symbols []
                       :line_offset 0}
            result (tool-system/execute-tool tool-instance validated)]
        (is (= :clojure (:mode result)))
        (is (not (:error result))))))

  (testing "Execute with offset and limit for raw mode"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          validated {:path (.getAbsolutePath *test-file*)
                     :clojure_mode "off"
                     :expand_symbols []
                     :line_offset 2
                     :limit 2}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (= :raw (:mode result)))
      (is (contains? result :content))
      (is (not (str/includes? (:content result) "Line 1")))
      (is (not (str/includes? (:content result) "Line 2")))
      (is (str/includes? (:content result) "Line 3"))
      (is (str/includes? (:content result) "Line 4"))
      (is (not (str/includes? (:content result) "Line 5")))
      (is (not (:error result))))))

(deftest format-results-test
  (testing "Format results for raw content adds correct XML wrapper"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          content-result {:mode :raw
                          :path (.getAbsolutePath *test-file*)
                          :content "Line 1\nLine 2\nLine 3"
                          :truncated? false
                          :line-count 3
                          :offset 0}
          result (tool-system/format-results tool-instance content-result)]
      (is (map? result))
      (is (contains? result :result))
      (is (contains? result :error))
      (is (false? (:error result)))
      (is (vector? (:result result)))
      (is (= 1 (count (:result result))))
      (let [formatted-content (first (:result result))]
        (is (str/starts-with? formatted-content "<file-content"))
        (is (str/includes? formatted-content "Line 1"))
        (is (str/ends-with? formatted-content "</file-content>")))))

  (testing "Format results for Clojure content adds correct XML wrapper and advice"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          content-result {:mode :clojure
                          :path (.getAbsolutePath *clojure-test-file*)
                          :content "(ns test-namespace)\n\n(defn test-function [x y] ...)"
                          :clojure-mode "auto"
                          :expand-symbols []}
          result (tool-system/format-results tool-instance content-result)]
      (is (map? result))
      (is (contains? result :result))
      (is (contains? result :error))
      (is (false? (:error result)))
      (is (vector? (:result result)))
      (is (= 2 (count (:result result)))) ;; Should include XML content and advice
      (let [formatted-content (first (:result result))
            advice (second (:result result))]
        (is (str/starts-with? formatted-content "<collapsed-clojure-view"))
        (is (str/includes? formatted-content "test-function"))
        (is (str/ends-with? formatted-content "</collapsed-clojure-view>"))
        (is (str/includes? advice "<!-- This is a COLLAPSED VIEW"))
        (is (str/includes? advice "expand_symbols")))))

  (testing "Format results handles errors"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          error-result {:error true
                        :message "File not found"}
          result (tool-system/format-results tool-instance error-result)]
      (is (map? result))
      (is (true? (:error result)))
      (is (vector? (:result result)))
      (is (= ["File not found"] (:result result))))))

(deftest tool-execution-test
  (testing "Basic text file reading"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath *test-file*)})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (is (= 1 (count (:result result))))
      (let [formatted-content (first (:result result))]
        (is (str/starts-with? formatted-content "<file-content"))
        (is (str/includes? formatted-content "Line 1"))
        (is (str/includes? formatted-content "Line 5"))
        (is (str/ends-with? formatted-content "</file-content>")))))

  (testing "Clojure file reading with auto mode detection"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath *clojure-test-file*)})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (is (= 2 (count (:result result)))) ;; Should include XML content and advice
      (let [formatted-content (first (:result result))
            advice (second (:result result))]
        (is (str/starts-with? formatted-content "<collapsed-clojure-view"))
        (is (str/includes? formatted-content "test-function"))
        (is (str/ends-with? formatted-content "</collapsed-clojure-view>"))
        (is (str/includes? advice "<!-- This is a COLLAPSED VIEW")))))

  (testing "Clojure file reading with expanded function"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath *clojure-test-file*)
                               "expand_symbols" ["test-function"]})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (let [formatted-content (first (:result result))]
        (is (str/includes? formatted-content "\"Test function docstring\""))
        (is (str/includes? formatted-content "(+ x y)")))))

  (testing "Reading non-existent file"
    (let [tool-instance (unified-read-file-tool/create-unified-read-file-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"path" (.getAbsolutePath (io/file *test-dir* "nonexistent.txt"))})]
      (is (true? (:error? result)))
      (is (vector? (:result result)))
      (is (str/includes? (first (:result result)) "not exist")))))
