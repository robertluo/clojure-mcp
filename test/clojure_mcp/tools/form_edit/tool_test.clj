(ns clojure-mcp.tools.form-edit.tool-test
  (:require
   [clojure.test :refer [deftest testing is use-fixtures]]
   [clojure-mcp.tools.form-edit.tool :as sut]
   [clojure-mcp.tools.form-edit.pipeline :as pipeline]   
   [clojure-mcp.tool-system :as tool-system]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Test fixtures
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)
(def ^:dynamic *client-atom* nil)

(defn create-test-files-fixture [f]
  (let [test-dir (io/file (System/getProperty "java.io.tmpdir")
                          (str "test-dir-" (System/currentTimeMillis)))]
    (.mkdirs test-dir)
    (let [test-file (io/file test-dir "test.clj")]
      (spit test-file (str "(ns test.core)\n\n"
                           "(defn example-fn\n  \"Original docstring\"\n  [x y]\n  (+ x y))\n\n"
                           "(def a 1)\n\n"
                           "(comment\n  (example-fn 1 2))\n\n"
                           ";; Test comment\n;; spans multiple lines"))
      (binding [*test-dir* test-dir
                *test-file* test-file
                ;; Use a mock nrepl client atom for isolated testing
                *client-atom* (atom {:clojure-mcp.core/nrepl-user-dir (.getAbsolutePath test-dir)
                                     :clojure-mcp.core/allowed-directories [(.getAbsolutePath test-dir)]})]
        (try
          (f)
          (finally
            (when (.exists test-file) (.delete test-file))
            (when (.exists test-dir) (.delete test-dir))))))))

(use-fixtures :each create-test-files-fixture)

;; Test helper functions
(defn get-file-path []
  (.getAbsolutePath *test-file*))

(defn make-callback []
  (let [p (promise)]
    [p (fn [result error]
         (deliver p {:result result :error error}))]))

;; Tests for tool configurations
(deftest tool-configurations-test
  (testing "Tool factory functions create correct configurations"
    (let [client-atom *client-atom*
          replace-tool (sut/create-edit-replace-form-tool client-atom)
          before-tool (sut/create-edit-insert-before-form-tool client-atom)
          after-tool (sut/create-edit-insert-after-form-tool client-atom)
          docstring-tool (sut/create-edit-docstring-tool client-atom)
          comment-tool (sut/create-edit-comment-block-tool client-atom)
          structure-tool (sut/create-file-structure-tool client-atom)]

      (testing "Replace form tool has correct configuration"
        (is (= :clojure-edit-replace-form (:tool-type replace-tool)))
        (is (= client-atom (:nrepl-client-atom replace-tool))))

      (testing "Insert before tool has correct configuration"
        (is (= :clojure-edit-insert-before-form (:tool-type before-tool)))
        (is (= client-atom (:nrepl-client-atom before-tool))))

      (testing "Insert after tool has correct configuration"
        (is (= :clojure-edit-insert-after-form (:tool-type after-tool)))
        (is (= client-atom (:nrepl-client-atom after-tool))))

      (testing "Docstring tool has correct configuration"
        (is (= :clojure-edit-replace-docstring (:tool-type docstring-tool)))
        (is (= client-atom (:nrepl-client-atom docstring-tool))))

      (testing "Comment block tool has correct configuration"
        (is (= :clojure-edit-comment-block (:tool-type comment-tool)))
        (is (= client-atom (:nrepl-client-atom comment-tool))))

      (testing "File structure tool has correct configuration"
        (is (= :clojure-file-structure (:tool-type structure-tool)))
        (is (= client-atom (:nrepl-client-atom structure-tool)))))))

;; Tests for multimethod implementations
(deftest tool-multimethod-test
  (testing "Tool multimethods are implemented correctly"
    (let [client-atom *client-atom*
          replace-tool (sut/create-edit-replace-form-tool client-atom)
          before-tool (sut/create-edit-insert-before-form-tool client-atom)
          docstring-tool (sut/create-edit-docstring-tool client-atom)
          comment-tool (sut/create-edit-comment-block-tool client-atom)
          structure-tool (sut/create-file-structure-tool client-atom)]

      (testing "Tool names are correct"
        (is (= "clojure_edit_replace_form" (tool-system/tool-name replace-tool)))
        (is (= "clojure_edit_insert_before_form" (tool-system/tool-name before-tool)))
        (is (= "clojure_edit_replace_docstring" (tool-system/tool-name docstring-tool)))
        (is (= "clojure_edit_replace_comment_block" (tool-system/tool-name comment-tool)))
        (is (= "clojure_file_structure" (tool-system/tool-name structure-tool))))

      (testing "Tool descriptions are not empty"
        (is (string? (tool-system/tool-description replace-tool)))
        (is (not (str/blank? (tool-system/tool-description replace-tool))))
        (is (string? (tool-system/tool-description before-tool)))
        (is (not (str/blank? (tool-system/tool-description before-tool)))))

      (testing "Tool schemas are correctly defined"
        (let [replace-schema (tool-system/tool-schema replace-tool)
              docstring-schema (tool-system/tool-schema docstring-tool)
              comment-schema (tool-system/tool-schema comment-tool)
              structure-schema (tool-system/tool-schema structure-tool)]
          (is (map? replace-schema))
          (is (= :object (:type replace-schema)))
          (is (contains? (:properties replace-schema) :file_path))
          (is (contains? (:properties replace-schema) :form_name))
          (is (contains? (:properties replace-schema) :form_type))
          (is (contains? (:properties replace-schema) :content))

          (is (map? docstring-schema))
          (is (= :object (:type docstring-schema)))
          (is (contains? (:properties docstring-schema) :docstring))

          (is (map? comment-schema))
          (is (= :object (:type comment-schema)))
          (is (contains? (:properties comment-schema) :comment_substring))
          (is (contains? (:properties comment-schema) :new_content))

          (is (map? structure-schema))
          (is (= :object (:type structure-schema)))
          (is (contains? (:properties structure-schema) :file_path))
          (is (contains? (:properties structure-schema) :expand_symbols)))))))

;; Tests for validation
(deftest validation-test
  (testing "Input validation functions correctly"
    (let [client-atom *client-atom*
          replace-tool (sut/create-edit-replace-form-tool client-atom)
          docstring-tool (sut/create-edit-docstring-tool client-atom)
          comment-tool (sut/create-edit-comment-block-tool client-atom)
          structure-tool (sut/create-file-structure-tool client-atom)]

      (testing "Replace form validation checks required parameters"
        (let [valid-inputs {:file_path (get-file-path)
                            :form_name "example-fn"
                            :form_type "defn"
                            :content "(defn example-fn [x] (* x 2))"}
              validated (tool-system/validate-inputs replace-tool valid-inputs)]
          ;; Only check that we have a valid file path, not the exact value
          ;; since validate-path-with-client may normalize/canonicalize the path
          (is (string? (:file_path validated)))
          (is (= "example-fn" (:form_name validated)))
          (is (= "defn" (:form_type validated)))
          (is (= "(defn example-fn [x] (* x 2))" (:content validated))))

        (is (thrown? clojure.lang.ExceptionInfo
                     (tool-system/validate-inputs replace-tool
                                                  {:form_name "example-fn"
                                                   :form_type "defn"
                                                   :content "(defn example-fn [x] (* x 2))"}))
            "Should throw exception when file_path is missing")

        (is (thrown? clojure.lang.ExceptionInfo
                     (tool-system/validate-inputs replace-tool
                                                  {:file_path (get-file-path)
                                                   :form_type "defn"
                                                   :content "(defn example-fn [x] (* x 2))"}))
            "Should throw exception when form_name is missing"))

      (testing "Docstring validation checks required parameters"
        (let [valid-inputs {:file_path (get-file-path)
                            :form_name "example-fn"
                            :form_type "defn"
                            :docstring "New docstring"}
              validated (tool-system/validate-inputs docstring-tool valid-inputs)]
          ;; Only check that we have a valid file path, not the exact value
          ;; since validate-path-with-client may normalize/canonicalize the path
          (is (string? (:file_path validated)))
          (is (= "example-fn" (:form_name validated)))
          (is (= "defn" (:form_type validated)))
          (is (= "New docstring" (:docstring validated))))

        (is (thrown? clojure.lang.ExceptionInfo
                     (tool-system/validate-inputs docstring-tool
                                                  {:file_path (get-file-path)
                                                   :form_name "example-fn"
                                                   :form_type "defn"}))
            "Should throw exception when docstring is missing"))

      (testing "Comment block validation checks required parameters"
        (let [valid-inputs {:file_path (get-file-path)
                            :comment_substring "Test comment"
                            :new_content ";; Updated comment"}
              validated (tool-system/validate-inputs comment-tool valid-inputs)]
          ;; Only check that we have a valid file path, not the exact value
          ;; since validate-path-with-client may normalize/canonicalize the path
          (is (string? (:file_path validated)))
          (is (= "Test comment" (:comment_substring validated)))
          (is (= ";; Updated comment" (:new_content validated))))

        (is (thrown? clojure.lang.ExceptionInfo
                     (tool-system/validate-inputs comment-tool
                                                  {:file_path (get-file-path)
                                                   :new_content ";; Updated comment"}))
            "Should throw exception when comment_substring is missing"))

      (testing "File structure validation handles optional parameters"
        (let [valid-inputs-1 {:file_path (get-file-path)}
              valid-inputs-2 {:file_path (get-file-path)
                              :expand_symbols ["example-fn"]}
              validated-1 (tool-system/validate-inputs structure-tool valid-inputs-1)
              validated-2 (tool-system/validate-inputs structure-tool valid-inputs-2)]
          ;; Only check that we have a valid file path, not the exact value
          ;; since validate-path-with-client may normalize/canonicalize the path
          (is (string? (:file_path validated-1)))
          (is (= [] (:expand_symbols validated-1)))
          ;; Only check that we have a valid file path, not the exact value
          ;; since validate-path-with-client may normalize/canonicalize the path
          (is (string? (:file_path validated-2)))
          (is (= ["example-fn"] (:expand_symbols validated-2))))))))

;; Integration tests for backward compatibility functions
(deftest backward-compatibility-test
  (testing "Backward compatibility functions create valid registration maps"
    (let [client-atom *client-atom*
          replace-reg (sut/top-level-form-edit-tool client-atom)
          before-reg (sut/top-level-form-insert-before-tool client-atom)
          after-reg (sut/top-level-form-insert-after-tool client-atom)
          docstring-reg (sut/docstring-edit-tool client-atom)
          comment-reg (sut/comment-block-edit-tool client-atom)
          structure-reg (sut/clojure-file-outline-tool client-atom)]

      (testing "Registration maps have correct structure"
        (is (string? (:name replace-reg)))
        (is (string? (:description replace-reg)))
        (is (string? (:schema replace-reg))) ;; Schema is serialized as JSON string
        (is (fn? (:tool-fn replace-reg)))

        (is (string? (:name before-reg)))
        (is (string? (:description before-reg)))
        (is (string? (:schema before-reg))) ;; Schema is serialized as JSON string
        (is (fn? (:tool-fn before-reg)))

        (is (string? (:name comment-reg)))
        (is (string? (:description comment-reg)))
        (is (string? (:schema comment-reg))) ;; Schema is serialized as JSON string
        (is (fn? (:tool-fn comment-reg)))))))

;; Functional tests with tool execution
(deftest tool-execution-test
  (testing "Tool execution works correctly"
    (let [client-atom *client-atom*
          replace-tool (sut/create-edit-replace-form-tool client-atom)
          docstring-tool (sut/create-edit-docstring-tool client-atom)
          comment-tool (sut/create-edit-comment-block-tool client-atom)
          structure-tool (sut/create-file-structure-tool client-atom)]

      (testing "Replace form tool can modify files"
        (let [file-path (get-file-path)
              inputs {:file_path file-path
                      :form_name "example-fn"
                      :form_type "defn"
                      :content "(defn example-fn [x]\n  (* x 2))"}
              validated (tool-system/validate-inputs replace-tool inputs)
              result (tool-system/execute-tool replace-tool validated)
              formatted (tool-system/format-results replace-tool result)
              file-content (slurp file-path)]
          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (= (:result formatted) [(:diff result)]))
          (is (not (nil? (:diff result))))
          (is (str/includes? file-content "(defn example-fn [x]"))
          (is (str/includes? file-content "(* x 2)"))
          (is (not (str/includes? file-content "(+ x y)")))))

      (testing "Docstring tool can update docstrings"
        (let [file-path (get-file-path)
              ;; Write a file with proper docstring to fix test setup
              _ (spit file-path (str "(ns test.core)\n\n"
                                     "(defn example-fn\n  \"Original docstring\"\n  [x y]\n  (+ x y))\n\n"
                                     "(def a 1)\n\n"
                                     "(comment\n  (example-fn 1 2))\n\n"
                                     ";; Test comment\n;; spans multiple lines"))
              inputs {:file_path file-path
                      :form_name "example-fn"
                      :form_type "defn"
                      :docstring "Updated docstring for testing"}
              validated (tool-system/validate-inputs docstring-tool inputs)
              result (tool-system/execute-tool docstring-tool validated)
              formatted (tool-system/format-results docstring-tool result)
              file-content (slurp file-path)]
          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (= (:result formatted) [(:diff result)]))
          (is (str/includes? file-content "Updated docstring for testing"))
          (is (not (str/includes? file-content "Original docstring")))))

      (testing "Comment tool can update comment blocks"
        (let [file-path (get-file-path)
              inputs {:file_path file-path
                      :comment_substring "Test comment"
                      :new_content ";; Updated test comment\n;; with multiple lines"}
              validated (tool-system/validate-inputs comment-tool inputs)
              result (tool-system/execute-tool comment-tool validated)
              formatted (tool-system/format-results comment-tool result)
              file-content (slurp file-path)]
          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (= (:result formatted) [(:diff result)]))
          (is (str/includes? file-content "Updated test comment"))
          (is (str/includes? file-content "with multiple lines"))
          (is (not (str/includes? file-content "Test comment\n;; spans multiple")))))

      (testing "File structure tool can generate outlines"
        (let [file-path (get-file-path)
              inputs {:file_path file-path}
              validated (tool-system/validate-inputs structure-tool inputs)
              result (tool-system/execute-tool structure-tool validated)
              formatted (tool-system/format-results structure-tool result)
              outline (first (:result formatted))
              _ (println "Outline content:" outline)] ;; Add debugging
          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (= (:result formatted) (:result result)))
          (is (string? outline))
          (is (str/includes? outline "(ns test.core)"))
          ;; Update expectation - file has [x y] not [x]
          (is (str/includes? outline "(defn example-fn [x y]"))
          (is (str/includes? outline "(def a ...)"))
          (is (str/includes? outline "(comment"))
          (is (not (str/includes? outline "(+ x y)"))))))))

(deftest defmethod-handling-test
  (testing "Tool correctly handles defmethod forms"
    (let [client-atom *client-atom*
          replace-tool (sut/create-edit-replace-form-tool client-atom)
          file-path (get-file-path)

          ;; First create a file with defmulti and defmethod forms
          _ (spit file-path (str "(ns test.multimethods)\n\n"
                                 "(defmulti area :shape)\n\n"
                                 "(defmethod area :rectangle [rect]\n  (* (:width rect) (:height rect)))\n\n"
                                 "(defmethod area :circle [circle]\n  (* Math/PI (:radius circle) (:radius circle)))\n"))]

      (testing "Can update defmethod with just the multimethod name"
        (let [inputs {:file_path file-path
                      :form_name "area" ;; Just the multimethod name
                      :form_type "defmethod"
                      :content "(defmethod area :rectangle [rect]\n  ;; Updated implementation\n  (let [w (:width rect)\n        h (:height rect)]\n    (* w h)))"}
              validated (tool-system/validate-inputs replace-tool inputs)
              result (tool-system/execute-tool replace-tool validated)
              formatted (tool-system/format-results replace-tool result)
              file-content (slurp file-path)]

          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (str/includes? file-content "Updated implementation"))
          (is (str/includes? file-content "(let [w (:width rect)"))
          (is (not (str/includes? file-content "(* (:width rect) (:height rect))")))))

      (testing "Can update defmethod with multimethod name and dispatch value"
        (let [inputs {:file_path file-path
                      :form_name "area :circle" ;; Compound name with dispatch value
                      :form_type "defmethod"
                      :content "(defmethod area :circle [circle]\n  ;; Updated circle implementation\n  (let [r (:radius circle)]\n    (* Math/PI r r)))"}
              validated (tool-system/validate-inputs replace-tool inputs)
              result (tool-system/execute-tool replace-tool validated)
              formatted (tool-system/format-results replace-tool result)
              file-content (slurp file-path)]

          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (str/includes? file-content "Updated circle implementation"))
          (is (str/includes? file-content "(let [r (:radius circle)]"))
          (is (not (str/includes? file-content "(* Math/PI (:radius circle) (:radius circle))")))))

      (testing "Inserting new defmethod implementation"
        (let [inputs {:file_path file-path
                      :form_name "area :circle" ;; Insert after circle implementation
                      :form_type "defmethod"
                      :content "(defmethod area :triangle [triangle]\n  (* 0.5 (:base triangle) (:height triangle)))"}
              before-tool (sut/create-edit-insert-after-form-tool client-atom)
              validated (tool-system/validate-inputs before-tool inputs)
              result (tool-system/execute-tool before-tool validated)
              formatted (tool-system/format-results before-tool result)
              file-content (slurp file-path)]

          (is (false? (:error formatted)))
          (is (vector? (:result formatted)))
          (is (str/includes? file-content "area :triangle"))
          (is (str/includes? file-content "(* 0.5 (:base triangle) (:height triangle))")))))))

;; Tool-fn tests through the callback interface
(deftest tool-fn-test
  (testing "Tool-fn works with callbacks"
    (let [client-atom *client-atom*
          replace-reg (sut/top-level-form-edit-tool client-atom)
          replace-fn (:tool-fn replace-reg)
          [p1 cb1] (make-callback)

          comment-reg (sut/comment-block-edit-tool client-atom)
          comment-fn (:tool-fn comment-reg)
          [p2 cb2] (make-callback)

          structure-reg (sut/clojure-file-outline-tool client-atom)
          structure-fn (:tool-fn structure-reg)
          [p3 cb3] (make-callback)]

      (testing "Replace form tool works via callback"
        (replace-fn nil
                    {"file_path" (get-file-path)
                     "form_name" "example-fn"
                     "form_type" "defn"
                     "content" "(defn example-fn [x]\n  (str \"result: \" (* x 3)))"}
                    cb1)
        (let [result @p1]
          (is (false? (:error result))) ;; Error is false, not nil
          (is (vector? (:result result)))
          (is (str/includes? (slurp (get-file-path)) "(str \"result: \" (* x 3))"))))

      (testing "Comment tool works via callback"
        (comment-fn nil
                    {"file_path" (get-file-path)
                     "comment_substring" "(example-fn"
                     "new_content" "(comment\n  (example-fn 10 20))"}
                    cb2)
        (let [result @p2]
          (is (false? (:error result))) ;; Error is false, not nil
          (is (vector? (:result result)))
          (is (str/includes? (slurp (get-file-path)) "(example-fn 10 20)"))))

      (testing "Structure tool works via callback"
        (structure-fn nil
                      {"file_path" (get-file-path)}
                      cb3)
        (let [result @p3
              outline (first (:result result))]
          (is (false? (:error result))) ;; Error is false, not nil
          (is (vector? (:result result)))
          (is (string? outline))
          (is (str/includes? outline "(ns test.core)"))
          (is (str/includes? outline "(defn example-fn")))))))
