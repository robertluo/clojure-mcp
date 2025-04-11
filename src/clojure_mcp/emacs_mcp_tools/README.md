# Emacs MCP Tools Documentation

This module provides MCP tools that expose Emacs editing functionality through the Model Context Protocol, allowing Claude and other LLMs to directly manipulate Emacs buffers with visual feedback.

## Key Features

- **Visual Feedback**: All editing operations provide visual highlighting and modeline flashing to make changes obvious
- **Full Buffer Control**: Create, modify, and manage multiple buffers
- **Artifact-like Interface**: Create and update "artifact" buffers with proper syntax highlighting
- **File Management**: Open, edit, and save files directly from the LLM
- **Region Operations**: Highlight specific regions for focused attention

## Available Tools

### Buffer and File Management

| Tool Name | Description |
| --- | --- |
| `emacs_open_file` | Opens a file in Emacs with visual confirmation |
| `emacs_save_file` | Saves the current buffer to a file with visual confirmation |
| `emacs_buffer_content` | Gets the entire content of a buffer |
| `emacs_create_buffer` | Creates a new buffer in Emacs and optionally fills it with content |
| `emacs_switch_to_buffer` | Switches to a buffer in Emacs and brings the window to focus |

### Content Editing

| Tool Name | Description |
| --- | --- |
| `emacs_replace_buffer_content` | Replaces the entire content of a buffer with highlighting |
| `emacs_update_buffer_content` | Updates all occurrences of text with new text with highlighting |
| `emacs_append_to_buffer` | Appends content to the end of a buffer with highlighting |

### Region Operations

| Tool Name | Description |
| --- | --- |
| `emacs_highlight_region` | Highlights a region of text in a buffer |
| `emacs_clear_highlights` | Clears all highlights in a buffer |

### Artifact-like Interface

| Tool Name | Description |
| --- | --- |
| `emacs_create_artifact` | Creates a new artifact-like buffer with syntax highlighting based on type |
| `emacs_update_artifact` | Updates an existing artifact buffer with new content |
| `emacs_rewrite_artifact` | Completely rewrites an existing artifact buffer |

## Usage Examples

### Creating and Editing Artifacts

```clojure
;; Create a new artifact with Clojure code
(def args {"id" "my-function"
           "title" "Example Function"
           "type" "application/vnd.ant.code"
           "language" "clojure"
           "content" "(defn example-fn [x]\n  (+ x 5))"})

;; Update specific parts of the artifact
(def update-args {"id" "my-function"
                  "old_text" "(+ x 5)"
                  "new_text" "(* x 2)"})

;; Completely rewrite the artifact
(def rewrite-args {"id" "my-function"
                   "content" "(defn improved-example [x y]\n  (+ (* x 2) y))"})
```

### File Operations

```clojure
;; Open a file
(def open-args {"path" "/path/to/file.clj"})

;; Get buffer content
(def content-args {"buffer" "current"})

;; Save to file
(def save-args {"buffer" "current"
                "path" "/path/to/save.clj"})
```

### Content Manipulation

```clojure
;; Update specific content
(def update-args {"buffer" "current"
                  "old_text" "defn example"
                  "new_text" "defn improved-example"})

;; Append content
(def append-args {"buffer" "current"
                  "content" "\n\n;; Additional code here\n(defn helper [] nil)"})
```

## Integration with Claude

These tools are designed to replace the artifact functionality when working with Claude. Some key equivalences:

| Claude Artifact | Emacs MCP Tool |
| --- | --- |
| `create` | `emacs_create_artifact` |
| `update` | `emacs_update_artifact` |
| `rewrite` | `emacs_rewrite_artifact` |

Claude can now directly manipulate Emacs buffers with visual feedback, making it easier to see what changes are being made.

## Field Reference

### Common Parameters

| Parameter | Description |
| --- | --- |
| `buffer` | Buffer to operate on (use 'current' for active buffer) |
| `path` | File path for reading or writing files |
| `content` | Text content for buffer operations |
| `highlight_duration` | Duration of highlight effect in seconds (default: 2.0) |
| `flash` | Whether to flash modeline after operation (default: true) |

### Artifact Parameters

| Parameter | Description |
| --- | --- |
| `id` | Unique identifier for the artifact buffer |
| `title` | Optional title for the artifact (shown at top) |
| `type` | MIME type (e.g., 'text/markdown', 'application/vnd.ant.code') |
| `language` | Language for code artifacts (e.g., 'clojure', 'javascript') |
| `old_text` | Text to replace in update operations |
| `new_text` | Replacement text for update operations |