(ns clojure-mcp.emacs-mcp-tools.core
  "MCP tools for file editing through Emacs.
   Provides a set of tools that expose Emacs functionality as MCP tools,
   replacing the need for artifacts when working with Claude."
  (:require
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure-mcp.emacs-tools.emacs-client :as emacs]
   [clojure-mcp.emacs-tools.emacs-highlight :as highlight]))

;; -------------------------------------------------------------------------
;; Core API Utilities
;; -------------------------------------------------------------------------

(defn extract-args
  "Safely extract arguments from an MCP arguments map with proper validation."
  [arg-map & arg-keys]
  (mapv #(get arg-map %) arg-keys))

(defn success-result
  "Create a successful result for the MCP tool callback."
  [message]
  (fn [clj-result-k]
    (clj-result-k [message] false)))

(defn error-result
  "Create an error result for the MCP tool callback."
  [message]
  (fn [clj-result-k]
    (clj-result-k [message] true)))

(defn make-mcp-tool
  "Helper function to create an MCP tool map with common structure.
   
   Arguments:
   - name: Name of the tool (string)
   - description: Tool description (string)
   - schema: JSON schema for the tool parameters (can be string or map)
   - impl-fn: Implementation function that takes arguments map and returns a function
              that accepts a callback and produces the result
   
   Returns a complete tool map ready to be added to an MCP server."
  [name description schema impl-fn]
  {:name name
   :description description
   :schema (if (string? schema) schema (json/write-str schema))
   :tool-fn (fn [_ arg-map clj-result-k]
              (try
                (let [result-fn (impl-fn arg-map)]
                  (result-fn clj-result-k))
                (catch Exception e
                  (clj-result-k [(str "Error: " (.getMessage e))] true))))})

;; -------------------------------------------------------------------------
;; Buffer and File Management Tools
;; -------------------------------------------------------------------------

(defn open-file-tool []
  (make-mcp-tool
   "emacs_open_file"
   "Opens a file in Emacs with visual confirmation."
   {:type :object
    :properties {:path {:type :string
                        :description "Path to the file to open"}
                 :flash {:type :boolean
                        :description "Whether to flash the modeline after opening (default: true)"
                        :default true}}
    :required [:path]}
   (fn [arg-map]
     (let [[file-path flash] (extract-args arg-map "path" "flash")
           flash (if (nil? flash) true flash)]
       (try
         (emacs/emacs-open-file file-path)
         (when flash
           (highlight/flash-modeline (emacs/emacs-current-buffer)))
         (success-result (str "Successfully opened file: " file-path))
         (catch Exception e
           (error-result (str "Failed to open file: " (.getMessage e)))))))))

(defn save-file-tool []
  (make-mcp-tool
   "emacs_save_file"
   "Saves the current buffer to a file with visual confirmation."
   {:type :object
    :properties {:buffer {:type :string
                           :description "Buffer to save (use 'current' for active buffer, default: 'current')"}
                 :path {:type :string
                       :description "Path where to save the file"}
                 :flash {:type :boolean
                        :description "Whether to flash the modeline after saving (default: true)"
                        :default true}}
    :required [:path]}
   (fn [arg-map]
     (let [[buffer-name file-path flash] (extract-args arg-map "buffer" "path" "flash")
           buffer-name (if (or (nil? buffer-name) (= buffer-name "current"))
                         (emacs/emacs-current-buffer)
                         buffer-name)
           flash (if (nil? flash) true flash)]
       (try
         (emacs/emacs-save-buffer buffer-name file-path)
         (when flash
           (highlight/flash-modeline buffer-name))
         (success-result (str "Successfully saved buffer to file: " file-path))
         (catch Exception e
           (error-result (str "Failed to save file: " (.getMessage e)))))))))

(defn buffer-content-tool []
  (make-mcp-tool
   "emacs_buffer_content"
   "Gets the entire content of a buffer."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer to get content from (use 'current' for active buffer, default: 'current')"}}
    :required []}
   (fn [arg-map]
     (let [buffer-name (let [name (get arg-map "buffer")]
                         (if (or (nil? name) (= name "current"))
                           (emacs/emacs-current-buffer)
                           name))]
       (try
         (let [content (emacs/emacs-buffer-content buffer-name)]
           (fn [clj-result-k]
             (clj-result-k [content] false)))
         (catch Exception e
           (error-result (str "Failed to get buffer content: " (.getMessage e)))))))))

;; -------------------------------------------------------------------------
;; Content Editing Tools
;; -------------------------------------------------------------------------

(defn replace-buffer-content-tool []
  (make-mcp-tool
   "emacs_replace_buffer_content"
   "Replaces the entire content of a buffer with highlighting."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer to modify (use 'current' for active buffer, default: 'current')"}
                 :content {:type :string
                           :description "New content for the buffer"}
                 :highlight_duration {:type :number
                                     :description "Duration of highlight effect in seconds (default: 2.0)"
                                     :default 2.0}}
    :required [:content]}
   (fn [arg-map]
     (let [[buffer-name content highlight-duration] 
           (extract-args arg-map "buffer" "content" "highlight_duration")
           buffer-name (if (or (nil? buffer-name) (= buffer-name "current"))
                         (emacs/emacs-current-buffer)
                         buffer-name)
           highlight-duration (or highlight-duration 2.0)]
       (try
         (emacs/emacs-replace-buffer-content-with-highlight 
          buffer-name content :highlight-duration highlight-duration)
         (success-result 
          (str "Successfully replaced content in buffer: " buffer-name 
               " (" (count content) " characters)"))
         (catch Exception e
           (error-result (str "Failed to replace buffer content: " (.getMessage e)))))))))

(defn update-buffer-content-tool []
  (make-mcp-tool
   "emacs_update_buffer_content"
   "Updates all occurrences of a string with a new string in the buffer with highlighting."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer to modify (use 'current' for active buffer, default: 'current')"}
                 :old_text {:type :string
                           :description "Text to be replaced"}
                 :new_text {:type :string
                           :description "Replacement text"}
                 :highlight_duration {:type :number
                                     :description "Duration of highlight effect in seconds (default: 2.0)"
                                     :default 2.0}}
    :required [:old_text :new_text]}
   (fn [arg-map]
     (let [[buffer-name old-string new-string highlight-duration] 
           (extract-args arg-map "buffer" "old_text" "new_text" "highlight_duration")
           buffer-name (if (or (nil? buffer-name) (= buffer-name "current"))
                         (emacs/emacs-current-buffer)
                         buffer-name)
           highlight-duration (or highlight-duration 2.0)]
       (try
         (highlight/update-with-highlight 
          buffer-name old-string new-string 
          :highlight-duration highlight-duration
          :flash true)
         (success-result 
          (str "Successfully updated content in buffer: " buffer-name))
         (catch Exception e
           (error-result (str "Failed to update buffer content: " (.getMessage e)))))))))

(defn append-to-buffer-tool []
  (make-mcp-tool
   "emacs_append_to_buffer"
   "Appends content to the end of a buffer with highlighting."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer to modify (use 'current' for active buffer, default: 'current')"}
                 :content {:type :string
                           :description "Content to append to the buffer"}
                 :highlight_duration {:type :number
                                     :description "Duration of highlight effect in seconds (default: 2.0)"
                                     :default 2.0}}
    :required [:content]}
   (fn [arg-map]
     (let [[buffer-name content highlight-duration] 
           (extract-args arg-map "buffer" "content" "highlight_duration")
           buffer-name (if (or (nil? buffer-name) (= buffer-name "current"))
                         (emacs/emacs-current-buffer)
                         buffer-name)
           highlight-duration (or highlight-duration 2.0)]
       (try
         (highlight/append-with-highlight 
          buffer-name content 
          :highlight-duration highlight-duration
          :flash true)
         (success-result 
          (str "Successfully appended content to buffer: " buffer-name 
               " (" (count content) " characters)"))
         (catch Exception e
           (error-result (str "Failed to append to buffer: " (.getMessage e)))))))))

;; -------------------------------------------------------------------------
;; Buffer Creation and Management Tools
;; -------------------------------------------------------------------------

(defn create-buffer-tool []
  (make-mcp-tool
   "emacs_create_buffer"
   "Creates a new buffer in Emacs and optionally fills it with content."
   {:type :object
    :properties {:name {:type :string
                        :description "Name of the buffer to create"}
                 :content {:type :string
                           :description "Initial content for the buffer (optional)"}
                 :switch_to {:type :boolean
                            :description "Whether to switch to the buffer after creation (default: true)"
                            :default true}}
    :required [:name]}
   (fn [arg-map]
     (let [[buffer-name content switch-to] 
           (extract-args arg-map "name" "content" "switch_to")
           switch-to (if (nil? switch-to) true switch-to)]
       (try
         (emacs/emacs-create-buffer buffer-name)
         (when content
           (emacs/emacs-replace-buffer-content buffer-name content))
         (when switch-to
           (emacs/emacs-switch-to-buffer buffer-name)
           (highlight/flash-modeline buffer-name))
         (success-result (str "Successfully created buffer: " buffer-name))
         (catch Exception e
           (error-result (str "Failed to create buffer: " (.getMessage e)))))))))

(defn switch-to-buffer-tool []
  (make-mcp-tool
   "emacs_switch_to_buffer"
   "Switches to a buffer in Emacs and brings the window to focus."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Name of the buffer to switch to"}
                 :flash {:type :boolean
                        :description "Whether to flash the modeline after switching (default: true)"
                        :default true}}
    :required [:buffer]}
   (fn [arg-map]
     (let [[buffer-name flash] (extract-args arg-map "buffer" "flash")
           flash (if (nil? flash) true flash)]
       (try
         (emacs/emacs-switch-to-buffer buffer-name)
         (when flash
           (highlight/flash-modeline buffer-name))
         (success-result (str "Successfully switched to buffer: " buffer-name))
         (catch Exception e
           (error-result (str "Failed to switch to buffer: " (.getMessage e)))))))))

;; -------------------------------------------------------------------------
;; Region and Position Tools
;; -------------------------------------------------------------------------

(defn highlight-region-tool []
  (make-mcp-tool
   "emacs_highlight_region"
   "Highlights a region of text in a buffer."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer containing the text (use 'current' for active buffer, default: 'current')"}
                 :start {:type :integer
                         :description "Start position of the region (character offset)"}
                 :end {:type :integer
                       :description "End position of the region (character offset)"}
                 :face {:type :string
                        :description "Face to use for highlighting (default: 'highlight')"
                        :default "highlight"}
                 :duration {:type :number
                           :description "Duration of highlight effect in seconds (0 for permanent)"
                           :default 3.0}}
    :required [:start :end]}
   (fn [arg-map]
     (let [[buffer-name start end face duration] 
           (extract-args arg-map "buffer" "start" "end" "face" "duration")
           buffer-name (if (or (nil? buffer-name) (= buffer-name "current"))
                         (emacs/emacs-current-buffer)
                         buffer-name)
           face (if (nil? face) 'highlight (symbol face))
           duration (or duration 3.0)]
       (try
         (emacs/emacs-highlight-region 
          buffer-name start end 
          :face face
          :duration (when (> duration 0) duration))
         (success-result 
          (str "Successfully highlighted region in buffer: " buffer-name 
               " (positions " start " to " end ")"))
         (catch Exception e
           (error-result (str "Failed to highlight region: " (.getMessage e)))))))))

(defn clear-highlights-tool []
  (make-mcp-tool
   "emacs_clear_highlights"
   "Clears all highlights in a buffer."
   {:type :object
    :properties {:buffer {:type :string
                          :description "Buffer to clear highlights from (use 'current' for active buffer, default: 'current')"}}
    :required []}
   (fn [arg-map]
     (let [buffer-name (let [name (get arg-map "buffer")]
                         (if (or (nil? name) (= name "current"))
                           (emacs/emacs-current-buffer)
                           name))]
       (try
         (emacs/emacs-clear-highlights buffer-name)
         (success-result (str "Successfully cleared highlights in buffer: " buffer-name))
         (catch Exception e
           (error-result (str "Failed to clear highlights: " (.getMessage e)))))))))

;; -------------------------------------------------------------------------
;; Artifact-like Tools
;; -------------------------------------------------------------------------

(defn create-artifact-buffer []
  (make-mcp-tool
   "emacs_create_artifact"
   "Creates a new artifact-like buffer with syntax highlighting based on type."
   {:type :object
    :properties {:id {:type :string
                      :description "Unique identifier for the artifact"}
                 :title {:type :string
                         :description "Title of the artifact (optional)"}
                 :type {:type :string
                        :description "MIME type (e.g., 'text/markdown', 'application/vnd.ant.code')"}
                 :language {:type :string
                           :description "Language for code (e.g., 'clojure', 'javascript')"}
                 :content {:type :string
                          :description "Content of the artifact"}}
    :required [:id :content]}
   (fn [arg-map]
     (let [[id title type language content] 
           (extract-args arg-map "id" "title" "type" "language" "content")
           buffer-name (str "*artifact:" id "*")
           mode (cond
                  (and (= type "application/vnd.ant.code") language)
                  (format "(let ((major-mode-to-use (or (and (string= \"%s\" \"clojure\") 'clojure-mode)
                                                     (and (string= \"%s\" \"javascript\") 'js-mode)
                                                     (and (string= \"%s\" \"python\") 'python-mode)
                                                     (and (string= \"%s\" \"html\") 'html-mode)
                                                     (and (string= \"%s\" \"css\") 'css-mode)
                                                     (and (string= \"%s\" \"java\") 'java-mode)
                                                     (and (string= \"%s\" \"rust\") 'rust-mode)
                                                     (and (string= \"%s\" \"go\") 'go-mode)
                                                     (and (string= \"%s\" \"typescript\") 'typescript-mode)
                                                     'fundamental-mode)))
                         (funcall major-mode-to-use))" 
                          language language language language language language language language language)
                  
                  (= type "text/markdown")
                  "(when (fboundp 'markdown-mode) (markdown-mode))"
                  
                  (= type "image/svg+xml")
                  "(when (fboundp 'xml-mode) (xml-mode))"
                  
                  (= type "application/vnd.ant.mermaid")
                  "(when (fboundp 'mermaid-mode) (mermaid-mode))"
                  
                  :else
                  "(fundamental-mode)")]
       (try
         ;; Create and prepare the buffer
         (emacs/emacs-create-buffer buffer-name)
         (emacs/emacs-switch-to-buffer buffer-name)
         
         ;; Insert the header with title if available
         (when title
           (emacs/emacs-eval-and-focus
            (format "(with-current-buffer \"%s\"
                       (insert \"# %s\\n\\n\"))"
                    buffer-name
                    (str/replace title "\"" "\\\""))))
         
         ;; Insert the content
         (emacs/emacs-replace-buffer-content buffer-name content)
         
         ;; Set appropriate mode for syntax highlighting
         (emacs/emacs-eval-and-focus
          (format "(with-current-buffer \"%s\" %s)" buffer-name mode))
         
         ;; Flash the buffer to indicate completion
         (highlight/flash-modeline buffer-name)
         
         (success-result
          (str "Successfully created artifact: " id 
               (when title (str " (" title ")"))
               " in buffer " buffer-name))
         (catch Exception e
           (error-result (str "Failed to create artifact: " (.getMessage e)))))))))

(defn update-artifact-buffer []
  (make-mcp-tool
   "emacs_update_artifact"
   "Updates an existing artifact buffer with new content."
   {:type :object
    :properties {:id {:type :string
                      :description "Unique identifier of the artifact to update"}
                 :old_text {:type :string
                           :description "Text in the artifact to be replaced"}
                 :new_text {:type :string
                           :description "New text to replace with"}
                 :highlight_duration {:type :number
                                     :description "Duration of highlight effect in seconds (default: 2.0)"
                                     :default 2.0}}
    :required [:id :old_text :new_text]}
   (fn [arg-map]
     (let [[id old-str new-str highlight-duration] 
           (extract-args arg-map "id" "old_text" "new_text" "highlight_duration")
           buffer-name (str "*artifact:" id "*")
           highlight-duration (or highlight-duration 2.0)]
       (try
         (if (emacs/emacs-buffer-exists? buffer-name)
           (do
             (highlight/update-with-highlight
              buffer-name old-str new-str
              :highlight-duration highlight-duration
              :flash true)
             (success-result
              (str "Successfully updated artifact: " id)))
           (error-result (str "Artifact not found: " id)))
         (catch Exception e
           (error-result (str "Failed to update artifact: " (.getMessage e)))))))))

(defn rewrite-artifact-buffer []
  (make-mcp-tool
   "emacs_rewrite_artifact"
   "Completely rewrites an existing artifact buffer."
   {:type :object
    :properties {:id {:type :string
                      :description "Unique identifier of the artifact to rewrite"}
                 :content {:type :string
                          :description "New content for the artifact"}
                 :highlight_duration {:type :number
                                     :description "Duration of highlight effect in seconds (default: 2.0)"
                                     :default 2.0}}
    :required [:id :content]}
   (fn [arg-map]
     (let [[id content highlight-duration] 
           (extract-args arg-map "id" "content" "highlight_duration")
           buffer-name (str "*artifact:" id "*")
           highlight-duration (or highlight-duration 2.0)]
       (try
         (if (emacs/emacs-buffer-exists? buffer-name)
           (do
             (emacs/emacs-replace-buffer-content-with-highlight
              buffer-name content :highlight-duration highlight-duration)
             (emacs/emacs-switch-to-buffer buffer-name)
             (highlight/flash-modeline buffer-name)
             (success-result
              (str "Successfully rewrote artifact: " id)))
           (error-result (str "Artifact not found: " id)))
         (catch Exception e
           (error-result (str "Failed to rewrite artifact: " (.getMessage e)))))))))

;; Collection of all tools
(defn all-tools
  "Returns a collection of all Emacs MCP tools."
  []
  [(open-file-tool)
   (save-file-tool)
   (buffer-content-tool)
   (replace-buffer-content-tool)
   (update-buffer-content-tool)
   (append-to-buffer-tool)
   (create-buffer-tool)
   (switch-to-buffer-tool)
   (highlight-region-tool)
   (clear-highlights-tool)
   (create-artifact-buffer)
   (update-artifact-buffer)
   (rewrite-artifact-buffer)])
