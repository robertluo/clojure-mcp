# Emacs File Operations Library

This directory contains a modular library for performing file operations via Emacs.

## Organization

The library is organized into the following components:

- `core.clj`: Contains core functionality for Emacs communication and file operations
- `operations/`: Directory containing specialized operations modules:
  - `read.clj`: File reading operations
  - `write.clj`: File writing operations
  - `edit.clj`: File editing operations
  - `manage.clj`: File management operations (find, delete, rename, copy)
  - `visual.clj`: Visual feedback operations (flash, highlight)

## Usage

The main API is exposed through the `clojure-mcp.emacs-tools-enhanced.file-api` namespace.
All functions are available through the `files` map.

```clojure
(require '[clojure-mcp.emacs-tools-enhanced.file-api :refer [files]])

;; Read a file
(files :read-file "/path/to/file.txt")

;; Write to a file
(files :write-file "/path/to/file.txt" "New content")

;; Edit a file
(files :edit-file "/path/to/file.txt" 
       [{:old-text "foo" :new-text "bar"}
        {:old-text "baz" :new-text "qux"}])
```

## Adding New Operations

To add new operations:

1. Identify the appropriate category for your operation
2. Add your function to the corresponding file in `operations/`
3. Expose the function in the public API in `file_api.clj`
