(ns clojure-mcp.tools.file-write.core-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [clojure-mcp.tools.file-write.core :as file-write-core]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Setup test fixtures
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-clj-file* nil)
(def ^:dynamic *test-txt-file* nil)

(defn create-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") "clojure-mcp-test")
        test-clj-file (io/file test-dir "test-file.clj")
        test-txt-file (io/file test-dir "test-file.txt")]

    ;; Create test directory
    (.mkdirs test-dir)

    ;; Create initial Clojure file with valid content
    (spit test-clj-file "(ns test.namespace)\n\n(defn test-function [x]\n  (* x 2))")

    ;; Create initial text file
    (spit test-txt-file "This is a test file.\nIt has multiple lines.\nFor testing purposes.")

    ;; Bind dynamic vars for test
    (binding [*test-dir* test-dir
              *test-clj-file* test-clj-file
              *test-txt-file* test-txt-file]
      (try
        (f)
        (finally
          ;; Clean up
          (doseq [file [test-clj-file test-txt-file]]
            (when (.exists file)
              (.delete file)))
          (.delete test-dir))))))

(use-fixtures :each create-test-files-fixture)

(deftest is-clojure-file-test
  (testing "Detecting Clojure file extensions"
    (is (file-write-core/is-clojure-file? "test.clj"))
    (is (file-write-core/is-clojure-file? "test.cljs"))
    (is (file-write-core/is-clojure-file? "test.cljc"))
    (is (file-write-core/is-clojure-file? "test.edn"))
    (is (file-write-core/is-clojure-file? "/path/to/file.clj"))
    (is (file-write-core/is-clojure-file? "C:\\Path\\To\\File.CLJ")) ; Case insensitive
    (is (not (file-write-core/is-clojure-file? "test.txt")))
    (is (not (file-write-core/is-clojure-file? "test.md")))
    (is (not (file-write-core/is-clojure-file? "test.js")))))

(deftest write-text-file-test
  (testing "Creating a new text file"
    (let [new-file (io/file *test-dir* "new-text-file.txt")
          content "This is a brand new file."
          result (file-write-core/write-text-file (.getPath new-file) content)]
      (is (not (:error result)))
      (is (= "create" (:type result)))
      (is (= (.getPath new-file) (:file-path result)))
      (is (str/blank? (:diff result))) ; No diff for new files
      (is (.exists new-file))
      (is (= content (slurp new-file)))
      (.delete new-file)))

  (testing "Updating an existing text file"
    (let [path (.getPath *test-txt-file*)
          original-content (slurp *test-txt-file*)
          new-content "This is updated content.\nWith new lines."
          result (file-write-core/write-text-file path new-content)]
      (is (not (:error result)))
      (is (= "update" (:type result)))
      (is (= path (:file-path result)))
      (is (not (str/blank? (:diff result)))) ; Should have a diff
      (is (= new-content (slurp *test-txt-file*))))))

(deftest write-clojure-file-test
  (testing "Creating a new Clojure file"
    (let [new-file (io/file *test-dir* "new-file.clj")
          content "(ns new.namespace)\n\n(defn new-function [x]\n  (+ x 10))"
          result (file-write-core/write-clojure-file (.getPath new-file) content)]
      (is (not (:error result)))
      (is (= "create" (:type result)))
      (is (= (.getPath new-file) (:file-path result)))
      (is (.exists new-file))
      (.delete new-file)))

  (testing "Updating an existing Clojure file"
    (let [path (.getPath *test-clj-file*)
          original-content (slurp *test-clj-file*)
          new-content "(ns test.namespace)\n\n(defn test-function [x]\n  (+ x 10))"
          result (file-write-core/write-clojure-file path new-content)]
      (is (not (:error result)))
      (is (= "update" (:type result)))
      (is (= path (:file-path result)))
      (is (not (str/blank? (:diff result)))) ; Should have a diff
      (is (not (= original-content (slurp *test-clj-file*)))))) ; Content should be updated

  (testing "Formatting Clojure code during write"
    (let [path (.getPath *test-clj-file*)
          unformatted-content "(ns test.namespace)( defn poorly-formatted-fn[x]( + x 5) )"
          result (file-write-core/write-clojure-file path unformatted-content)]
      (is (not (:error result)))
      (is (= "update" (:type result)))
      (is (not (str/includes? (slurp *test-clj-file*) "poorly-formatted-fn[x]")))
      (is (str/includes? (slurp *test-clj-file*) "poorly-formatted-fn [x]"))))
  
  (testing "Linting catches syntax errors in Clojure files"
    (let [path (.getPath *test-clj-file*)
          ;; Missing closing parenthesis - syntax error
          invalid-content "(ns test.namespace\n\n(defn broken-function [x]\n  (+ x 10)"
          result (file-write-core/write-clojure-file path invalid-content)]
      (is (:error result))
      (is (str/includes? (:message result) "Syntax errors detected in Clojure code"))
      (is (str/includes? (:message result) "opening ( with no matching"))
      ;; File should not be modified when there are syntax errors
      (is (not (= invalid-content (slurp *test-clj-file*))))))
      
  (testing "Linting catches unbalanced brackets"
    (let [path (.getPath *test-clj-file*)
          ;; Mismatched brackets - syntax error
          invalid-content "(ns test.namespace)\n\n(defn mismatched-fn [x]\n  (let [y (+ x 1)]\n    (println y]))"
          result (file-write-core/write-clojure-file path invalid-content)]
      (is (:error result))
      (is (str/includes? (:message result) "Syntax errors detected in Clojure code"))
      (is (or (str/includes? (:message result) "Mismatched bracket") 
              (str/includes? (:message result) "bracket"))
          "Error message should mention bracket issue")
      ;; File should not be modified when there are syntax errors
      (is (not (= invalid-content (slurp *test-clj-file*)))))))

(deftest write-file-test
  (testing "Write dispatcher for Clojure files"
    (let [clj-file (io/file *test-dir* "dispatch-test.clj")
          content "(ns dispatch.test)"
          result (file-write-core/write-file (.getPath clj-file) content)]
      (is (not (:error result)))
      (is (= "create" (:type result)))
      (is (.exists clj-file))
      (.delete clj-file)))

  (testing "Write dispatcher for text files"
    (let [txt-file (io/file *test-dir* "dispatch-test.txt")
          content "Plain text content"
          result (file-write-core/write-file (.getPath txt-file) content)]
      (is (not (:error result)))
      (is (= "create" (:type result)))
      (is (.exists txt-file))
      (.delete txt-file))))
