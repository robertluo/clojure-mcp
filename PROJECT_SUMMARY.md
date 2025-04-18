# Clojure MCP - LLM Assistant Guide

## Project Overview

Clojure MCP is a Model Context Protocol (MCP) server that enables LLM assistants to interact directly with a Clojure REPL environment. The core concept is to facilitate REPL-driven development where AI assistants can evaluate Clojure code in real-time, providing immediate feedback and enabling step-by-step verification of solutions.

## Key Project Files

### Core Implementation
- **`src/clojure_mcp/core.clj`**: Main entry point and server setup, creates the MCP server and registers tools
- **`src/clojure_mcp/nrepl.clj`**: Manages communication with nREPL server
- **`src/clojure_mcp/repl_tools.clj`**: Centralizes tool registration via the `get-all-tools` function
- **`src/clojure_mcp/resources.clj`**: Provides resource definitions for the MCP server
- **`src/clojure_mcp/prompts.clj`**: Defines prompts and centralizes prompt registration

### Tool Implementations
- **`src/clojure_mcp/repl_tools/eval.clj`**: Code evaluation tools
- **`src/clojure_mcp/repl_tools/history.clj`**: Evaluation history management
- **`src/clojure_mcp/repl_tools/namespace.clj`**: Namespace exploration and management
- **`src/clojure_mcp/repl_tools/symbol.clj`**: Symbol documentation, metadata and search
- **`src/clojure_mcp/repl_tools/utils.clj`**: Common utility functions like path validation and diff generation
- **`src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`**: Code editing tools
- **`src/clojure_mcp/repl_tools/project/inspect.clj`**: Project inspection tools
- **`src/clojure_mcp/repl_tools/filesystem/core.clj`**: Core filesystem operations
- **`src/clojure_mcp/repl_tools/filesystem/tools.clj`**: Filesystem tool implementations
- **`src/clojure_mcp/repl_tools/filesystem/grep.clj`**: File content search implementation

### Configuration
- **`deps.edn`**: Project dependencies and aliases
- **`.mcp.json`**: MCP server configuration

## Dependencies

The project relies on the following key dependencies (from `deps.edn`):

```clojure
{:deps {clj-kondo/clj-kondo {:mvn/version \"2024.03.13\"} ;; Static analysis
        org.clojure/clojure {:mvn/version \"1.11.1\"}
        org.clojure/data.json {:mvn/version \"2.5.1\"}
        io.modelcontextprotocol.sdk/mcp {:mvn/version \"0.8.1\"} ;; MCP SDK
        org.slf4j/slf4j-nop {:mvn/version \"2.0.3\"}
        nrepl/nrepl {:mvn/version \"1.3.1\"} ;; nREPL client
        com.fasterxml.jackson.core/jackson-databind {:mvn/version \"2.15.2\"}
        rewrite-clj/rewrite-clj {:mvn/version \"1.1.47\"} ;; Code parsing/editing
        dev.weavejester/cljfmt {:mvn/version \"0.13.1\"} ;; Code formatting
        }}
```

## Available MCP Tools

### Code Evaluation
- **`clojure_eval`**: Evaluates Clojure code in the REPL
  - Input: `{\"expression\": \"(+ 1 2)\"}`
  - Output: Evaluation result and/or error messages
  - Implementation: `src/clojure_mcp/repl_tools/eval.clj`

### REPL History
- **`clojure_eval_history`**: Retrieves previous evaluation results
  - Input: `{\"number-to-fetch\": 5}`
  - Implementation: `src/clojure_mcp/repl_tools/history.clj`

### Namespace Management
- **`current_namespace`**: Gets the active namespace
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`
- **`clojure_list_namespaces`**: Lists all available namespaces
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`
- **`clojure_list_vars_in_namespace`**: Lists vars in a namespace
  - Input: `{\"namespace\": \"clojure.string\"}`
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`

### Symbol Information
- **`symbol_completions`**: Gets completions for symbol prefixes
  - Input: `{\"prefix\": \"map\"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol_metadata`**: Retrieves detailed metadata for symbols
  - Input: `{\"symbol\": \"map\"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol_documentation`**: Gets docstrings and usage information
  - Input: `{\"symbol\": \"map\"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`source_code`**: Views source code implementation
  - Input: `{\"symbol\": \"map\"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol-search`**: Searches for symbols across namespaces
  - Input: `{\"search-str\": \"seq\"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`

### File Editing
- **`clojure_file_structure`**: Shows file structure with top-level forms
  - Input: `{\"file_path\": \"src/my_namespace/core.clj\", \"expand\": [\"my-function\"]}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_replace_form`**: Updates top-level forms
  - Input: `{\"file_path\": \"...\", \"form_name\": \"my-function\", \"form_type\": \"defn\", \"new_implementation\": \"...\"}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
  - Supports replacing a single form with multiple forms in one operation
- **`clojure_edit_insert_before_form`**: Inserts code before existing forms
  - Input: `{\"file_path\": \"...\", \"before_form_name\": \"main-fn\", \"form_type\": \"defn\", \"new_form_str\": \"...\"}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
  - Supports inserting multiple forms in one operation
- **`clojure_edit_insert_after_form`**: Inserts code after existing forms
  - Input: `{\"file_path\": \"...\", \"after_form_name\": \"helper-fn\", \"form_type\": \"defn\", \"new_form_str\": \"...\"}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
  - Supports inserting multiple forms in one operation
- **`clojure_edit_comment_block`**: Edits or updates comment blocks
  - Input: `{\"file_path\": \"...\", \"comment_substring\": \"Example usage\", \"new_content\": \"...\"}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_replace_docstring`**: Updates function docstrings
  - Input: `{\"file_path\": \"...\", \"form_name\": \"my-function\", \"form_type\": \"defn\", \"new_docstring\": \"...\"}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`

### File Operations
- **`file_write`**: Writes content to a file with formatting and linting
  - Input: `{\"file_path\": \"/path/to/file.clj\", \"content\": \"(ns my.namespace)\
\
(defn my-function [x]\
  (* x 2))\"}`
  - Output: Status indicating creation or update, plus a diff showing changes
  - Performs Clojure-aware linting and formatting on the content
  - Returns a detailed diff showing the changes made to the file
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
  - Best practices:
    - Use `read_file` first to understand file contents and context
    - Use `list_directory` to verify parent directories exist for new files

### Filesystem Tools
- **`directory_tree`**: Shows a recursively indented tree view of files and directories
  - Input: `{\"path\": \"/path/to/directory\", \"max_depth\": 2}`
  - Output: Text representation of the directory tree with proper indentation
  - Filters out temporary files like Emacs backups and hidden files
  - Adds \"...\" indicators for directories truncated due to max_depth
  - Implementation: `src/clojure_mcp/repl_tools/filesystem/tools.clj`

- **`fs_list_directory`**: Lists files and directories at a specified path
  - Input: `{\"path\": \"/path/to/directory\"}`
  - Output: Formatted directory listing with files and subdirectories
  - Implementation: `src/clojure_mcp/repl_tools/filesystem/tools.clj`

- **`fs_read_file`**: Reads file contents with optional line limits and offsets
  - Input: `{\"path\": \"/path/to/file\", \"offset\": 0, \"limit\": 2000}`
  - Output: File contents wrapped in XML tags with metadata
  - Implementation: `src/clojure_mcp/repl_tools/filesystem/tools.clj`

- **`glob_files`**: Fast file pattern matching using glob patterns
  - Input: `{\"path\": \"/path/to/directory\", \"pattern\": \"**/*.clj\", \"max_results\": 1000}`
  - Output: JSON with filenames (sorted by modification time), count, duration, and truncation flag
  - Supports standard glob patterns like \"**/*.js\" (recursive) or \"src/*.ts\" (single directory)
  - Implementation: `src/clojure_mcp/repl_tools/filesystem/tools.clj`

- **`fs_grep`**: Fast content search tool for finding text patterns in files
  - Input: `{\"path\": \"/path/to/directory\", \"pattern\": \"function\\\\s+\\\\w+\", \"include\": \"*.{js,ts}\", \"max_results\": 1000}`
  - Output: JSON with matching files (sorted by modification time), count, and duration
  - Uses system grep command with extended regex support when available
  - Provides pure Java fallback implementation for cross-platform compatibility
  - Supports multiple file types through brace expansion (e.g., \"*.{clj,md}\")
  - Implementation: `src/clojure_mcp/repl_tools/filesystem/grep.clj`

### Project Tools
- **`clojure_inspect_project`**: Analyzes project structure and dependencies
  - Implementation: `src/clojure_mcp/repl_tools/project/inspect.clj`

## Common Utility Functions

The project provides several utility functions in `src/clojure_mcp/repl_tools/utils.clj`:

- **`validate-path`**: Validates that a given path is within allowed directories
  - Normalizes relative paths to absolute paths
  - Uses Java File APIs for proper path resolution and canonicalization
  - Prevents directory traversal attacks and unauthorized access
  
- **`validate-path-with-client`**: Higher-level path validation using nREPL client settings
  - Uses the ::nrepl-user-dir and ::allowed-directories from the nREPL client
  - Throws clear exceptions when validation fails or settings are missing
  
- **`generate-diff-via-shell`**: Generates a unified diff between two strings
  - Uses the system diff command for accurate, detailed diffs
  - Provides configurable context lines around changes
  - Handles temporary file creation and cleanup automatically

## Available MCP Resources

The MCP server provides several resources that can be accessed by clients:

- **`custom://project-summary`**: Serves the PROJECT_SUMMARY.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://readme`**: Serves the README.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://project-info`**: Provides dynamic information about the project structure
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

### Resource Implementation

Resources in the MCP server are implemented using:

1. **Resource Definition**:
   ```clojure
   {:url \"custom://resource-name\"
    :name \"Human-readable Name\"
    :description \"Resource description\"
    :mime-type \"text/plain\"
    :resource-fn (fn [_ _ callback] (callback [\"Content as vector of strings\"]))}
   ```

2. **Resource Registration**:
   - All resources are defined in the `get-all-resources` function in `resources.clj`
   - Resources are registered in the `nrepl-mcp-server` function using a doseq loop with `add-resource`
   - The server must have resources enabled in its capabilities

3. **Resource Utility Functions**:
   - `create-file-resource`: Creates a resource that serves a file's contents
   - `create-string-resource`: Creates a resource that serves a static string

## Architecture

1. **MCP Server Layer**:
   - Implemented in `src/clojure_mcp/core.clj`
   - Handles MCP protocol communication via `io.modelcontextprotocol.sdk/mcp`
   - Creates and registers tools, prompts, and resources
   - Uses `StdioServerTransportProvider` for communication
   - Maintains the nREPL client atom with ::nrepl-user-dir and ::allowed-directories settings

2. **nREPL Client Layer**:
   - Implemented in `src/clojure_mcp/nrepl.clj`
   - Manages connection and communication with nREPL
   - Handles code evaluation and result formatting

3. **Tool Implementation Layer**:
   - Organized in `src/clojure_mcp/repl_tools/` directory
   - Each tool handles specific functionality like evaluation, symbol lookup, etc.
   - Uses `rewrite-clj` for code parsing and manipulation

4. **Resource Layer**:
   - Implemented in `src/clojure_mcp/resources.clj`
   - Defines resources that can be served by the MCP server
   - Provides utility functions for creating different types of resources

5. **Prompt Layer**:
   - Implemented in `src/clojure_mcp/prompts.clj`
   - Defines prompts for the MCP server
   - Provides access to template-based prompts

6. **Filesystem Layer**:
   - Implemented in `src/clojure_mcp/repl_tools/filesystem/` directory
   - Provides file and directory operations with configurable limits
   - Implements secure path validation to prevent unauthorized access
   - Supports XML-wrapped output format for file contents with metadata
   - Features glob pattern matching and directory tree visualization

## Pipeline Pattern

The project heavily utilizes a pipeline pattern for operations, particularly in the file editing tools:

1. **Context Map**: Operations use a context map (with ::namespaced keys) to pass data through a series of processing functions
2. **Thread-Ctx**: The `thread-ctx` function chains pipeline functions while short-circuiting on error
3. **Pipeline Components**: Individual functions perform specific operations and update the context map:
   - `load-source`: Loads file content into the context
   - `lint-code`: Validates code syntax and style
   - `format-source`: Formats code according to Clojure conventions
   - `generate-diff`: Creates a human-readable diff of changes
   - `determine-file-type`: Identifies if operation is create or update
   - `save-file`: Writes content to file with offset tracking
   - `highlight-form`: Provides visual feedback in Emacs

This pattern enables:
- Clear separation of concerns
- Consistent error handling
- Reusable components across different tools
- Easy extension with new processing steps

## Registration Pattern

The project uses a consistent pattern for registering tools, prompts, and resources:

1. **Centralized Collection Functions**:
   - `repl-tools/get-all-tools`: Returns a list of all tool instances
   - `prompts/get-all-prompts`: Returns a list of all prompt definitions
   - `resources/get-all-resources`: Returns a list of all resource definitions

2. **Helper Functions**:
   - `add-tool`: Registers a tool with the MCP server
   - `add-prompt`: Registers a prompt with the MCP server
   - `add-resource`: Registers a resource with the MCP server

3. **Registration Process**:
   - All registration happens in the `nrepl-mcp-server` function
   - Each category uses a doseq loop to iterate over the collection
   - This pattern makes it easy to add new items without modifying core.clj

## Development Workflow

1. The `nrepl-mcp-server` function in `core.clj` is the main entry point
2. It creates both an nREPL client and MCP server
3. It initializes the nREPL client with ::nrepl-user-dir and ::allowed-directories settings
4. Tools, prompts, and resources are registered using their respective collection functions
5. Each tool, prompt, and resource has a standard structure using continuation functions

## Recommended REPL-Driven Development Pattern

When developing with this tool:

1. **Start with exploration**: Use namespace and symbol tools to understand available functionality
2. **Develop incrementally**: Evaluate small pieces of code to verify correctness
3. **Build up solutions**: Chain successful evaluations into complete solutions
4. **Edit with care**: Use the specialized editing tools that maintain correct syntax
5. **Verify saved code**: After editing files, re-evaluate to ensure correctness
6. **Access documentation**: Use resource URLs to access project documentation when needed
7. **File access**: Use filesystem tools to navigate and read code files when needed

## Important Implementation Notes

1. **Error Handling**: Always check for errors in evaluation results
2. **Namespace Context**: Be aware of the current namespace when evaluating code
3. **File Editing**: The file editing tools are designed to maintain correct Clojure syntax
   - All editing tools now support multiple forms in a single operation
   - The `file_write` tool provides linting, formatting, and diffing
4. **Path Validation**: All filesystem operations enforce path validation
   - The nREPL client maintains ::nrepl-user-dir and ::allowed-directories settings
   - Use validate-path-with-client for secure path handling
5. **Async Tools**: All tool implementations use a callback continuation style for async results
6. **Prompts**: The system includes several built-in prompts for guiding development
7. **Resources**: Resources should return vectors of strings, which are wrapped in TextResourceContents
8. **File Reading**: The `fs_read_file` tool handles large files with configurable limits and offset to avoid memory issues
9. **Directory Traversal**: The `directory_tree` tool provides a clean, LLM-friendly view of file hierarchies
10. **Pattern Matching**: The `glob_files` tool enables efficient file searching with standard glob patterns
11. **Content Searching**: The `fs_grep` tool provides fast content searching with regular expression support
12. **Diff Generation**: The utils.clj provides a robust shell-based diff generator for showing changes

## Project Goals

The primary goal is to enable high-quality collaborative development between humans and AI through:

1. Immediate feedback via REPL evaluation
2. Incremental development with step-by-step verification
3. Human oversight for maintaining code quality
4. Functional programming patterns that produce more maintainable code
5. Access to contextual project information via resources
6. Efficient file and directory navigation and manipulation
7. Secure file access with proper path validation
8. Pattern-based file searching for quickly locating relevant code

## Extension Points

If you need to extend this project:

1. Add new tools in `src/clojure_mcp/repl_tools/` and include them in `get-all-tools`
2. Add new prompts in `src/clojure_mcp/prompts.clj` and include them in `get-all-prompts`
3. Add new resources in `src/clojure_mcp/resources.clj` and include them in `get-all-resources`
4. Add new filesystem operations in `src/clojure_mcp/repl_tools/filesystem/core.clj`
5. Add new utility functions in `src/clojure_mcp/repl_tools/utils.clj`
6. Create new pipeline components for the editing pipeline in `top_level_form_edit_pipeline.clj`

This centralized pattern makes it easy to extend the system without modifying the core server setup code.
