(ns clojure-mcp.repl-tools.top-level-form-edit-pipeline
  (:require
   [rewrite-clj.zip :as z]  
   [rewrite-clj.parser :as p]
   [rewrite-clj.node :as n]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.spec.alpha :as s]
   [clojure-mcp.linting :as linting]
   [clojure-mcp.utils.emacs-integration :as emacs]
   [clojure-mcp.repl-tools.top-level-form-edit :as tfe]))

;; Base spec for our context map
(s/def ::file-path string?)
(s/def ::source string?)
(s/def ::new-source-code string?)
(s/def ::top-level-def-name string?)
(s/def ::top-level-def-type string?)
(s/def ::edit-type #{:replace :before :after})
(s/def ::error keyword?)
(s/def ::message string?)
(s/def ::zloc any?) ;; zloc is opaque, so we'll use any?
(s/def ::offsets (s/tuple int? int?))
(s/def ::lint-result (s/nilable map?))

;; Context map that flows through the pipeline
(s/def ::context
  (s/keys :req [::file-path]
          :opt [::source ::new-source-code ::top-level-def-name 
                ::top-level-def-type ::edit-type ::error ::message 
                ::zloc ::offsets ::lint-result]))

;; Function specs
(s/fdef load-source
  :args (s/cat :ctx (s/keys :req [::file-path]))
  :ret (s/or :success (s/keys :req [::file-path ::source])
             :failure (s/keys :req [::error ::message])))

(s/fdef lint-code
  :args (s/cat :ctx (s/keys :req [::new-source-code]))
  :ret (s/or :success (s/keys :req [::new-source-code ::lint-result])
             :failure (s/keys :req [::error ::message])))

(s/fdef find-form
  :args (s/cat :ctx (s/keys :req [::source ::top-level-def-name ::top-level-def-type]))
  :ret (s/or :success (s/keys :req [::zloc])
             :failure (s/keys :req [::error ::message])))

(s/fdef edit-form
  :args (s/cat :ctx (s/keys :req [::zloc ::new-source-code ::edit-type]))
  :ret (s/or :success (s/keys :req [::zloc])
             :failure (s/keys :req [::error ::message])))

(s/fdef save-file
  :args (s/cat :ctx (s/keys :req [::file-path ::zloc]))
  :ret (s/or :success (s/keys :req [::offsets])
             :failure (s/keys :req [::error ::message])))

(s/fdef highlight-form
  :args (s/cat :ctx (s/keys :req [::file-path ::offsets]))
  :ret (s/or :success (s/keys :req [::file-path ::offsets])
             :failure (s/keys :req [::error ::message])))

;; Pipeline Implementation

(defn load-source
  "Loads the content of a file into the context map.
   Adds ::source to the context."
  [ctx]
  (try
    (assoc ctx ::source (slurp (::file-path ctx)))
    (catch java.io.FileNotFoundException _
      {::error :file-not-found
       ::message (str "File not found: " (::file-path ctx))})
    (catch java.io.IOException e
      {::error :io-error
       ::message (str "IO error while reading file: " (.getMessage e))})))

(defn lint-code
  "Lints the new source code to be inserted.
   Adds ::lint-result to the context."
  [ctx]
  (let [lint-result (linting/lint (::new-source-code ctx))]
    (if (and lint-result (:error? lint-result))
      {::error :lint-failure
       ::message (str "Linting issues in code:\n" (:report lint-result))}
      (assoc ctx ::lint-result lint-result))))

(defn find-form
  "Finds a top-level form in the source code.
   Adds ::zloc to the context with the zipper location."
  [ctx]
  (let [zloc (z/of-string (::source ctx) {:track-position? true})
        form-zloc (tfe/find-top-level-form zloc 
                                         (::top-level-def-type ctx) 
                                         (::top-level-def-name ctx))]
    (if form-zloc
      (assoc ctx ::zloc form-zloc)
      {::error :form-not-found
       ::message (str "Could not find form '" (::top-level-def-name ctx) 
                      "' of type '" (::top-level-def-type ctx) 
                      "' in file " (::file-path ctx))})))

(defn edit-form
  "Edits the form according to the specified edit type.
   Updates ::zloc in the context with the updated zipper."
  [ctx]
  (let [edit-type (::edit-type ctx)
        form-zloc (::zloc ctx)
        content-str (::new-source-code ctx)
        updated-zloc (tfe/edit-top-level-form 
                       form-zloc
                       (::top-level-def-type ctx)
                       (::top-level-def-name ctx)
                       content-str
                       edit-type)]
    (if updated-zloc
      (assoc ctx ::zloc updated-zloc)
      {::error :edit-failed
       ::message (str "Failed to " (name edit-type) " form.")})))

(defn save-file
  "Saves the updated source to the file and calculates offsets.
   Adds ::offsets to the context."
  [ctx]
  (try
    (let [updated-zloc (::zloc ctx)
          positions (z/position-span updated-zloc)
          final-source (z/root-string updated-zloc)
          offsets (tfe/zloc-offsets final-source positions)]
      (spit (::file-path ctx) final-source)
      (assoc ctx ::offsets offsets))
    (catch Exception e
      {::error :save-failed
       ::message (str "Failed to save file: " (.getMessage e))})))

(defn highlight-form
  "Highlights the edited form in Emacs.
   Returns the context unchanged or with error information."
  [ctx]
  (try
    (let [[start end] (::offsets ctx)]
      (emacs/temporary-highlight 
       (::file-path ctx) start end 2.0)
      ctx)
    (catch Exception e
      (assoc ctx ::highlight-error (.getMessage e)))))

;; Helper for threading functions with error handling
(defn thread-ctx
  "Thread a context map through a series of functions, short-circuiting on error.
   
   Arguments:
   - ctx: The initial context map
   - fns: A sequence of functions to apply to the context
   
   Returns:
   - The final context, possibly containing an error"
  [ctx & fns]
  (reduce (fn [c f]
            (if (::error c)
              (reduced c)  ; Short-circuit on error using reduced
              (f c)))
          ctx
          fns))

;; Full pipeline function
(defn edit-top-level-form-pipeline
  "Complete pipeline for editing a top-level form.
   
   Arguments:
   - name: Name of the form
   - file-path: Path to the file
   - tag: Type of form (e.g., 'defn', 'def')
   - content-str: New content to insert or replace with
   - edit-type: Keyword indicating edit type (:replace, :before, or :after)
   
   Returns:
   - A context map with the result or error information"
  [name file-path tag content-str edit-type]
  (thread-ctx 
   {::file-path file-path
    ::top-level-def-name name
    ::top-level-def-type tag
    ::new-source-code content-str
    ::edit-type edit-type}
   load-source
   lint-code
   find-form
   edit-form
   save-file
   highlight-form))

;; Format result for tool consumption
(defn format-result
  "Format the result of the pipeline for tool consumption.
   
   Arguments:
   - ctx: The final context map from the pipeline
   
   Returns:
   - A map with :error, :message, and possibly :offsets keys"
  [ctx]
  (if (::error ctx)
    {:error true
     :message (::message ctx)}
    {:error false
     :offsets (::offsets ctx)}))

;; Combining the pipeline with result formatting
(defn edit-form-in-file
  "Edit a top-level form in a file using the pipeline pattern.
   
   Arguments:
   - name: Name of the form
   - file-path: Path to the file
   - tag: Type of form (e.g., 'defn', 'def')
   - content-str: New content to insert or replace with
   - edit-type: Keyword indicating edit type (:replace, :before, or :after)
   
   Returns:
   - [start end] offsets if the edit was successful
   - false if the form was not found
   - A map with :error and :message keys if there were errors"
  [name file-path tag content-str edit-type]
  (let [result (edit-top-level-form-pipeline name file-path tag content-str edit-type)
        formatted (format-result result)]
    (cond
      (:error formatted) {:error (:error result)
                          :message (::message result)}
      (:offsets formatted) (::offsets result)
      :else false)))

;; Tool Factory for creating edit tools using the pipeline
(defn create-form-edit-tool
  "Creates a tool function for the specified edit type using the pipeline pattern.
   
   Arguments:
   - edit-type: Keyword indicating the edit type (:replace, :before, or :after)
   - service-atom: Service atom
   
   Returns:
   - A tool map with name, description, schema and tool-fn"
  [edit-type service-atom]
  (let [config (get tfe/edit-type-config edit-type)
        {:keys [tool-name short-description intro param-name param-description
                content-param-name content-description action-prefix success-message]} config
        long-description (str short-description "\n   \n" intro
                             " It preserves formatting and whitespace in the rest of the file.")]
    
    {:name tool-name
     :description long-description
     :schema
     (json/write-str
      {:type :object
       :properties
       {param-name {:type :string
                    :description param-description}
        :file_path {:type :string
                    :description "Path to the file containing the form"}
        :form_type {:type :string
                    :description "The type of form (e.g., 'defn', 'def', 'ns', 'deftest' ...). Required."}
        content-param-name {:type :string
                            :description content-description}}
       :required [param-name :file_path :form_type content-param-name]})
     :tool-fn (fn [_ arg-map clj-result-k]
                (let [form-name (get arg-map param-name)
                      file-path (get arg-map "file_path")
                      form-type (get arg-map "form_type")
                      content (get arg-map content-param-name)
                      result (edit-top-level-form-pipeline 
                              form-name file-path form-type content edit-type)
                      formatted (format-result result)]
                  (if (:error formatted)
                    (clj-result-k [(::message result)] true)
                    (let [[start end] (::offsets result)]
                      (try
                        (emacs/temporary-highlight file-path start end 2.0)
                        (catch Exception _
                          ;; Ignore highlight errors
                          nil))
                      (clj-result-k 
                       [(format success-message form-name file-path)]
                       false)))))}))

;; Tool functions using the pipeline
(defn top-level-form-edit-tool [service-atom]
  (create-form-edit-tool :replace service-atom))

(defn top-level-form-insert-before-tool [service-atom]
  (create-form-edit-tool :before service-atom))

(defn top-level-form-insert-after-tool [service-atom]
  (create-form-edit-tool :after service-atom))

(comment
  ;; Examples of using the pipeline
  (def result (edit-top-level-form-pipeline
               "example-function"
               "/path/to/file.clj"
               "defn"
               "(defn example-function [x y]\n  (+ x y))"
               :replace))
  
  ;; Check for errors
  (::error result)
  (::message result)
  
  ;; Get formatted result
  (format-result result)
  
  ;; Example of tool creation with pipeline
  (def edit-tool (top-level-form-edit-tool (atom {})))
  (def before-tool (top-level-form-insert-before-tool (atom {})))
  (def after-tool (top-level-form-insert-after-tool (atom {})))
)
