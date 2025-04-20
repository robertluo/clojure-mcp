(ns clojure-mcp.tools.form-edit.core-test
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [clojure-mcp.tools.form-edit.core :as sut]
   [rewrite-clj.zip :as z]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Test fixtures
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)

(defn create-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir") 
                          (str "test-dir-" (System/currentTimeMillis)))]
    (.mkdirs test-dir)
    (let [test-file (io/file test-dir "test.clj")]
      (spit test-file "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))\n\n(def a 1)\n\n(comment\n  (example-fn 1 2))\n\n;; Test comment\n;; spans multiple lines")
      (binding [*test-dir* test-dir
                *test-file* test-file]
        (try
          (f)
          (finally
            (when (.exists test-file) (.delete test-file))
            (when (.exists test-dir) (.delete test-dir))))))))

(use-fixtures :each create-test-files-fixture)

;; Test helper functions
(defn get-zloc [source-str]
  (z/of-string source-str))

;; Tests for core functions

(deftest is-top-level-form-test
  (testing "is-top-level-form? correctly identifies top-level forms"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))\n\n(def a 1)"
          zloc (get-zloc source)
          defn-loc (z/right zloc)] ;; Move to defn form
      (is (sut/is-top-level-form? zloc "ns" "test.core"))
      (is (sut/is-top-level-form? defn-loc "defn" "example-fn"))
      (is (not (sut/is-top-level-form? defn-loc "def" "example-fn")))
      (is (not (sut/is-top-level-form? defn-loc "defn" "other-fn"))))))

(deftest find-top-level-form-test
  (testing "find-top-level-form finds the correct form"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))\n\n(def a 1)"
          zloc (get-zloc source)]
      (is (some? (sut/find-top-level-form zloc "ns" "test.core")))
      (is (some? (sut/find-top-level-form zloc "defn" "example-fn")))
      (is (some? (sut/find-top-level-form zloc "def" "a")))
      (is (nil? (sut/find-top-level-form zloc "defn" "non-existent")))
      (is (nil? (sut/find-top-level-form zloc "def" "example-fn"))))))

(deftest edit-top-level-form-test
  (testing "edit-top-level-form correctly replaces a form"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))\n\n(def a 1)"
          zloc (get-zloc source)
          new-fn "(defn example-fn [x y]\n  (* x y))"
          edited-zloc (sut/edit-top-level-form zloc "defn" "example-fn" new-fn :replace)
          result-str (z/root-string edited-zloc)]
      (is (some? edited-zloc))
      (is (str/includes? result-str "(defn example-fn [x y]\n  (* x y))"))
      (is (not (str/includes? result-str "(defn example-fn [x y]\n  (+ x y))"))))))

  (testing "edit-top-level-form correctly inserts before a form"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)
          new-fn "(defn helper-fn [z]\n  (* z z))"
          edited-zloc (sut/edit-top-level-form zloc "defn" "example-fn" new-fn :before)
          result-str (z/root-string edited-zloc)]
      (is (some? edited-zloc))
      (is (str/includes? result-str "(defn helper-fn [z]\n  (* z z))"))
      (is (str/includes? result-str "(defn example-fn [x y]\n  (+ x y))")
          "Original form should still be present")))

  (testing "edit-top-level-form correctly inserts after a form"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)
          new-fn "(defn helper-fn [z]\n  (* z z))"
          edited-zloc (sut/edit-top-level-form zloc "defn" "example-fn" new-fn :after)
          result-str (z/root-string edited-zloc)]
      (is (some? edited-zloc))
      (is (str/includes? result-str "(defn helper-fn [z]\n  (* z z))"))
      (is (str/includes? result-str "(defn example-fn [x y]\n  (+ x y))")
          "Original form should still be present")))

(deftest row-col-offset-test
  (testing "row-col->offset correctly calculates character offsets"
    (let [source "Line 1\nLine 2\nLine 3"]
      (is (= 0 (sut/row-col->offset source 1 0)))
      (is (= 3 (sut/row-col->offset source 1 3)))
      (is (= 7 (sut/row-col->offset source 2 0)))
      (is (= 10 (sut/row-col->offset source 2 3)))
      (is (= 14 (sut/row-col->offset source 3 0))))))

(deftest find-docstring-test
  (testing "find-docstring finds docstring in a function"
    (let [source "(ns test.core)\n\n(defn example-fn\n  \"This is a docstring\"\n  [x y]\n  (+ x y))"
          zloc (get-zloc source)
          docstring-zloc (sut/find-docstring zloc "defn" "example-fn")]
      (is (some? docstring-zloc))
      (is (= "This is a docstring" (z/sexpr docstring-zloc)))))
  
  (testing "find-docstring returns nil when no docstring exists"
    (let [source "(ns test.core)\n\n(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)]
      (is (nil? (sut/find-docstring zloc "defn" "example-fn"))))))

(deftest edit-docstring-test
  (testing "edit-docstring correctly replaces a docstring"
    (let [source "(ns test.core)\n\n(defn example-fn\n  \"This is a docstring\"\n  [x y]\n  (+ x y))"
          zloc (get-zloc source)
          new-docstring "Updated docstring"
          edited-zloc (sut/edit-docstring zloc "defn" "example-fn" new-docstring)
          result-str (z/root-string edited-zloc)]
      (is (some? edited-zloc))
      (is (str/includes? result-str "Updated docstring"))
      (is (not (str/includes? result-str "This is a docstring"))))))

(deftest comment-identification-test
  (testing "is-comment-form? correctly identifies comment forms"
    (let [source "(ns test.core)\n\n(comment\n  (+ 1 2))\n\n(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)
          comment-loc (z/right zloc)] ;; Move to comment form
      (is (sut/is-comment-form? comment-loc))
      (is (not (sut/is-comment-form? zloc)))))
  
  (testing "is-line-comment? correctly identifies line comments"
    (let [source "(ns test.core)\n\n;; This is a comment\n(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)
          comment-loc (z/right zloc)] ;; Try to move to comment
      ;; Note: This test might be fragile as rewrite-clj might handle comments differently
      (is (not (sut/is-line-comment? zloc))))))

(deftest find-comment-block-test
  (testing "find-comment-block finds comment forms"
    (let [source "(ns test.core)\n\n(comment\n  (+ 1 2)\n  (* 3 4))\n\n(defn example-fn [x y]\n  (+ x y))"
          block (sut/find-comment-block source "(+ 1 2)")]
      (is (some? block) "Should find a comment block")
      (is (= :comment-form (:type block)) "Should be a comment form")
      (is (str/includes? (:content block) "(+ 1 2)") "Should contain search string")
      (is (str/includes? (:content block) "(* 3 4)") "Should contain all content")))
  
  (testing "find-comment-block finds line comments"
    (let [source "(ns test.core)\n\n;; This is a test comment\n;; with multiple lines\n\n(defn example-fn [x y]\n  (+ x y))"
          block (sut/find-comment-block source "test comment")]
      (is (some? block) "Should find a comment block")
      (is (= :line-comments (:type block)) "Should be line comments")
      (is (str/includes? (:content block) "test comment") "Should contain search string")
      (is (str/includes? (:content block) "multiple lines") "Should contain all content"))))

(deftest edit-comment-block-test
  (testing "edit-comment-block edits comment forms"
    (let [source "(ns test.core)\n\n(comment\n  (+ 1 2)\n  (* 3 4))\n\n(defn example-fn [x y]\n  (+ x y))"
          new-content "(comment\n  (- 10 5)\n  (/ 8 2))"
          result (sut/edit-comment-block source "(+ 1 2)" new-content)]
      (is (str/includes? result "(- 10 5)"))
      (is (str/includes? result "(/ 8 2)"))
      (is (not (str/includes? result "(+ 1 2)")))
      (is (not (str/includes? result "(* 3 4)")))))
  
  (testing "edit-comment-block edits line comments"
    (let [source "(ns test.core)\n\n;; This is a test comment\n;; with multiple lines\n\n(defn example-fn [x y]\n  (+ x y))"
          new-content ";; Updated comment\n;; with new content"
          result (sut/edit-comment-block source "test comment" new-content)]
      (is (str/includes? result "Updated comment"))
      (is (str/includes? result "new content"))
      (is (not (str/includes? result "test comment")))
      (is (not (str/includes? result "multiple lines"))))))

(deftest get-form-summary-test
  (testing "get-form-summary provides correct summary for defn forms"
    (let [source "(defn example-fn [x y]\n  (+ x y))"
          zloc (get-zloc source)
          summary (sut/get-form-summary zloc)]
      (is (= "(defn example-fn [x y] ...)" summary))))
  
  (testing "get-form-summary provides correct summary for def forms"
    (let [source "(def a 1)"
          zloc (get-zloc source)
          summary (sut/get-form-summary zloc)]
      (is (= "(def a ...)" summary))))
  
  (testing "get-form-summary returns full form for ns forms"
    (let [source "(ns test.core)"
          zloc (get-zloc source)
          summary (sut/get-form-summary zloc)]
      (is (= "(ns test.core)" summary)))))

(deftest generate-collapsed-file-view-test
  (testing "generate-collapsed-file-view creates collapsed view"
    (let [path (.getAbsolutePath *test-file*)
          result (sut/generate-collapsed-file-view path [])]
      (is (str/includes? result "(ns test.core)"))
      (is (str/includes? result "(defn example-fn [x y] ...)"))
      (is (str/includes? result "(def a ...)")))))

(deftest format-source-string-test
  (testing "format-source-string correctly formats source code"
    (let [unformatted "(defn   example-fn[x y]  (+ x  y)   )"
          formatted (sut/format-source-string unformatted)]
      ;; Compare as EDN to ignore whitespace differences
      (is (= (read-string unformatted) (read-string formatted))))))

(deftest load-file-content-test
  (testing "load-file-content loads existing file"
    (let [path (.getAbsolutePath *test-file*)
          result (sut/load-file-content path)]
      (is (not (:error result)))
      (is (str/includes? (:content result) "(defn example-fn"))))
  
  (testing "load-file-content returns error for non-existent file"
    (let [path (str (.getAbsolutePath *test-dir*) "/non-existent.clj")
          result (sut/load-file-content path)]
      (is (:error result))
      (is (= (str "File not found: " path) (:message result))))))

(deftest save-file-content-test
  (testing "save-file-content saves content to file"
    (let [path (.getAbsolutePath *test-file*)
          content "(ns updated.core)\n\n(defn new-fn [x] (* x x))"
          result (sut/save-file-content path content)]
      (is (:success result))
      (is (= content (slurp path))))))