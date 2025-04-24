(ns clojure-mcp.tools.unified-file-edit.tool-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.test-utils :as test-utils :refer [*nrepl-client-atom*]]
   [clojure-mcp.tools.unified-file-edit.tool :as unified-file-edit]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Setup test fixtures
(test-utils/apply-fixtures *ns*)

;; Setup test files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)
(def ^:dynamic *clojure-test-file* nil)
(def ^:dynamic *multi-occurrence-file* nil)

(defn setup-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-unified-file-edit-test")
        test-file (io/file test-dir "test-file.txt")
        clojure-test-file (io/file test-dir "test-file.clj")
        multi-occurrence-file (io/file test-dir "multi-occurrence.txt")]

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
")

    ;; Create file with multiple occurrences of the same string
    (spit multi-occurrence-file
          "This is a test file with multiple occurrences.
This line contains TARGET_STRING to replace.
Another line with TARGET_STRING in the middle.
Last line with TARGET_STRING at the end.")

    ;; Set allowed directories for path validation
    (swap! *nrepl-client-atom* assoc
           :clojure-mcp.core/nrepl-user-dir (.getAbsolutePath test-dir)
           :clojure-mcp.core/allowed-directories [(.getAbsolutePath test-dir)])

    ;; Run test with fixtures bound
    (binding [*test-dir* test-dir
              *test-file* test-file
              *clojure-test-file* clojure-test-file
              *multi-occurrence-file* multi-occurrence-file]
      (try
        (f)
        (finally
          ;; Clean up
          (doseq [file [test-file clojure-test-file multi-occurrence-file]]
            (when (.exists file)
              (.delete file)))
          (when (.exists test-dir)
            (.delete test-dir)))))))

(use-fixtures :each setup-test-files-fixture)

(deftest tool-registration-test
  (testing "Tool registration produces a proper map"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          reg-map (tool-system/registration-map tool-instance)]
      (is (map? reg-map))
      (is (= "edit_file" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (string? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))

(deftest determine-edit-mode-test
  (testing "determine-edit-mode correctly identifies the mode"
    (is (= :write (unified-file-edit/determine-edit-mode nil)))
    (is (= :write (unified-file-edit/determine-edit-mode "")))
    (is (= :edit (unified-file-edit/determine-edit-mode "some string")))))

(deftest validate-inputs-test
  (testing "Validate accepts valid input with minimal parameters"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          result (tool-system/validate-inputs
                  tool-instance
                  {:file_path (.getAbsolutePath *test-file*)
                   :new_string "new content"})]
      (is (map? result))
      (is (string? (:file-path result)))
      (is (nil? (:old-string result)))
      (is (= "new content" (:new-string result)))
      (is (= :write (:mode result)))))

  (testing "Validate accepts valid input with all parameters"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          result (tool-system/validate-inputs
                  tool-instance
                  {:file_path (.getAbsolutePath *test-file*)
                   :old_string "Line 2"
                   :new_string "Modified Line 2"})]
      (is (map? result))
      (is (string? (:file-path result)))
      (is (= "Line 2" (:old-string result)))
      (is (= "Modified Line 2" (:new-string result)))
      (is (= :edit (:mode result)))))

  (testing "Validate rejects missing file_path parameter"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:old_string "old"
                     :new_string "new"})))))

  (testing "Validate rejects missing new_string parameter"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:file_path (.getAbsolutePath *test-file*)
                     :old_string "old"})))))

  (testing "Validate rejects paths outside allowed directories"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:file_path "/etc/passwd"
                     :new_string "new content"})))))

  (testing "Validate rejects identical old_string and new_string"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)]
      (is (thrown? Exception
                   (tool-system/validate-inputs
                    tool-instance
                    {:file_path (.getAbsolutePath *test-file*)
                     :old_string "same text"
                     :new_string "same text"}))))))

(deftest execute-tool-test
  (testing "Execute in write mode creates a new file"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          ;; Make sure to delete the file first if it exists
          new-file-path (let [file (io/file *test-dir* "new-file.txt")]
                          (when (.exists file)
                            (.delete file))
                          (.getAbsolutePath file))
          content "This is a new file created by the unified file edit tool"
          validated {:file-path new-file-path
                     :old-string ""
                     :new-string content
                     :mode :write}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (false? (:error result)))
      (is (or (= "create" (:type result))
              (and (= "update" (:type result)) (str/includes? (:diff result) content))))
      (is (.exists (io/file new-file-path)))
      (is (= content (slurp new-file-path)))))

  (testing "Execute in write mode overwrites an existing file"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          file-path (.getAbsolutePath *test-file*)
          content "This content completely replaces the original content"
          validated {:file-path file-path
                     :old-string ""
                     :new-string content
                     :mode :write}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (false? (:error result)))
      (is (= "update" (:type result)))
      (is (= content (slurp file-path)))))

  (testing "Execute in edit mode replaces specific text"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          ;; Reset the test file for this test
          _ (spit *test-file* "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")
          file-path (.getAbsolutePath *test-file*)
          validated {:file-path file-path
                     :old-string "Line 3"
                     :new-string "MODIFIED LINE 3"
                     :mode :edit}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (false? (:error result)))
      (is (= "update" (:type result)))
      (is (str/includes? (:diff result) "MODIFIED LINE 3"))
      (is (str/includes? (slurp file-path) "MODIFIED LINE 3"))))

  (testing "Execute returns error when string to replace is not found"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          file-path (.getAbsolutePath *test-file*)
          validated {:file-path file-path
                     :old-string "Non-existent string"
                     :new-string "Replacement"
                     :mode :edit}
          result (tool-system/execute-tool tool-instance validated)]
      (is (map? result))
      (is (:error result))
      (is (str/includes? (:message result) "not found")))))

(deftest format-results-test
  (testing "Format results for successful edit operation"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          result {:error false
                  :type "update"
                  :file-path (.getAbsolutePath *test-file*)
                  :diff "--- old\n+++ new\n@@ -1,1 +1,1 @@\n-old line\n+new line"}
          formatted (tool-system/format-results tool-instance result)]
      (is (map? formatted))
      (is (contains? formatted :result))
      (is (contains? formatted :error))
      (is (false? (:error formatted)))
      (is (vector? (:result formatted)))
      (is (= 1 (count (:result formatted))))
      (is (str/includes? (first (:result formatted)) "old\n+++ new"))))

  (testing "Format results for successful create operation"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          result {:error false
                  :type "create"
                  :file-path (.getAbsolutePath (io/file *test-dir* "new-file.txt"))
                  :diff "--- old\n+++ new\n@@ -0,0 +1,1 @@\n+new content"}
          formatted (tool-system/format-results tool-instance result)]
      (is (map? formatted))
      (is (false? (:error formatted)))
      (is (vector? (:result formatted)))
      (is (= 1 (count (:result formatted))))
      (is (str/includes? (first (:result formatted)) "new\n@@ -0,0 +1,1"))))

  (testing "Format results handles errors correctly"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          error-result {:error true
                        :message "String to replace not found in file."}
          formatted (tool-system/format-results tool-instance error-result)]
      (is (map? formatted))
      (is (true? (:error formatted)))
      (is (vector? (:result formatted)))
      (is (= ["String to replace not found in file."] (:result formatted))))))

(deftest end-to-end-test
  (testing "End-to-end test for write mode"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          new-file-path (.getAbsolutePath (io/file *test-dir* "end-to-end-write.txt"))
          result (tool-tester {"file_path" new-file-path
                               "new_string" "This is an end-to-end test for write mode"})]
      (is (not (:error? result)))
      (is (vector? (:result result)))
      (is (.exists (io/file new-file-path)))
      (is (= "This is an end-to-end test for write mode" (slurp new-file-path)))))

  (testing "End-to-end test for edit mode"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"file_path" (.getAbsolutePath *test-file*)
                               "old_string" "Line 1"
                               "new_string" "This is line one"})]
      (is (not (:error? result)))
      (is (vector? (:result result)))
      (is (str/includes? (slurp *test-file*) "This is line one"))))

  (testing "End-to-end test for non-existent string"
    (let [tool-instance (unified-file-edit/create-unified-file-edit-tool *nrepl-client-atom*)
          tool-tester (test-utils/make-tool-tester tool-instance)
          result (tool-tester {"file_path" (.getAbsolutePath *test-file*)
                               "old_string" "String that does not exist"
                               "new_string" "Replacement text"})]
      (is (:error? result))
      (is (vector? (:result result)))
      (is (str/includes? (first (:result result)) "not found")))))
