# Clojure MCP - LLM Assistant Guide

## Project Overview

Clojure MCP is a Model Context Protocol (MCP) server that enables LLM assistants (like Claude) to interact directly with a Clojure REPL environment. The core concept is to facilitate REPL-driven development where AI assistants can evaluate Clojure code in real-time, providing immediate feedback and enabling step-by-step verification of solutions.

## Key Project Files

### Core Implementation
- **`src/clojure_mcp/core.clj`**: Main entry point and server setup, creates the MCP server and registers tools, prompts, and resources
- **`src/clojure_mcp/nrepl.clj`**: Manages communication with nREPL server
- **`src/clojure_mcp/repl_tools.clj`**: Centralizes tool registration via the `get-all-tools` function
- **`src/clojure_mcp/resources.clj`**: Provides resource definitions for the MCP server
- **`src/clojure_mcp/prompts.clj`**: Defines prompts and centralizes prompt registration
- **`src/clojure_mcp/tool_system.clj`**: Core multimethod-based system for tools implementation

### Tool Implementations
The project has been refactored to use a modular multimethod-based architecture:

#### Tool System Structure
Each tool follows this pattern:
- **`src/clojure_mcp/tools/{tool_name}/core.clj`**: Pure business logic independent of MCP
- **`src/clojure_mcp/tools/{tool_name}/tool.clj`**: MCP-specific implementation using the multimethods
- **`test/clojure_mcp/tools/{tool_name}/core_test.clj` and `tool_test.clj`**: Tests for both layers

#### Available Tools by Category
- **Code Evaluation**: `src/clojure_mcp/tools/eval/`
- **File Operations**: 
  - `src/clojure_mcp/tools/read_file/`
  - `src/clojure_mcp/tools/directory_tree/`
  - `src/clojure_mcp/tools/list_directory/`
  - `src/clojure_mcp/tools/file_write/`
  - `src/clojure_mcp/tools/grep/`
  - `src/clojure_mcp/tools/glob_files/`
- **Code Navigation and Metadata**:
  - `src/clojure_mcp/tools/namespace/`
  - `src/clojure_mcp/tools/symbol/`
- **Code Editing**:
  - `src/clojure_mcp/tools/form_edit/`
- **Project Metadata**:
  - `src/clojure_mcp/tools/project/`

### Configuration
- **`deps.edn`**: Project dependencies and aliases
- **`resources/configs/claude_desktop_config.json.example`**: Example MCP server configuration for Claude Desktop

## Dependencies

The project relies on the following key dependencies (from `deps.edn`):

```clojure
{:deps {clj-kondo/clj-kondo {:mvn/version "2024.03.13"} ;; Static analysis
        org.clojure/clojure {:mvn/version "1.11.1"}
        org.clojure/data.json {:mvn/version "2.5.1"}
        io.modelcontextprotocol.sdk/mcp {:mvn/version "0.8.1"} ;; MCP SDK
        org.slf4j/slf4j-nop {:mvn/version "2.0.3"}
        nrepl/nrepl {:mvn/version "1.3.1"} ;; nREPL client
        com.fasterxml.jackson.core/jackson-databind {:mvn/version "2.15.2"}
        rewrite-clj/rewrite-clj {:mvn/version "1.1.47"} ;; Code parsing/editing
        dev.weavejester/cljfmt {:mvn/version "0.13.1"} ;; Code formatting
        pogonos/pogonos {:mvn/version "0.2.1"} ;; Templating library
        }}
```

## Available MCP Tools

### Code Evaluation
- **`clojure_eval`**: Evaluates Clojure code in the REPL
  - Input: `{"code": "(+ 1 2)"}`
  - Output: Evaluation result and/or error messages
  - Implementation: `src/clojure_mcp/tools/eval/`

### Namespace Management
- **`current_namespace`**: Gets the active namespace
  - Implementation: `src/clojure_mcp/tools/namespace/`
- **`clojure_list_namespaces`**: Lists all available namespaces
  - Implementation: `src/clojure_mcp/tools/namespace/`
- **`clojure_list_vars_in_namespace`**: Lists vars in a namespace
  - Input: `{"namespace": "clojure.string"}`
  - Implementation: `src/clojure_mcp/tools/namespace/`

### Symbol Information
- **`symbol_completions`**: Gets completions for symbol prefixes
  - Input: `{"prefix": "map"}`
  - Implementation: `src/clojure_mcp/tools/symbol/`
- **`symbol_metadata`**: Retrieves detailed metadata for symbols
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/tools/symbol/`
- **`symbol_documentation`**: Gets docstrings and usage information
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/tools/symbol/`
- **`source_code`**: Views source code implementation
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/tools/symbol/`
- **`symbol-search`**: Searches for symbols across namespaces
  - Input: `{"search-str": "seq"}`
  - Implementation: `src/clojure_mcp/tools/symbol/`

### Form Editing Tools
- **`clojure_file_structure`**: Shows file structure with top-level forms
  - Input: `{"file_path": "src/my_namespace/core.clj", "expand": ["my-function"]}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`
- **`clojure_edit_replace_form`**: Updates top-level forms
  - Input: `{"file_path": "...", "form_name": "my-function", "form_type": "defn", "new_implementation": "..."}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`
  - Supports replacing a single form with multiple forms in one operation
- **`clojure_edit_insert_before_form`**: Inserts code before existing forms
  - Input: `{"file_path": "...", "before_form_name": "main-fn", "form_type": "defn", "new_form_str": "..."}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`
  - Supports inserting multiple forms in one operation
- **`clojure_edit_insert_after_form`**: Inserts code after existing forms
  - Input: `{"file_path": "...", "after_form_name": "helper-fn", "form_type": "defn", "new_form_str": "..."}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`
  - Supports inserting multiple forms in one operation
- **`clojure_edit_comment_block`**: Edits or updates comment blocks
  - Input: `{"file_path": "...", "comment_substring": "Example usage", "new_content": "..."}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`
- **`clojure_edit_replace_docstring`**: Updates function docstrings
  - Input: `{"file_path": "...", "form_name": "my-function", "form_type": "defn", "new_docstring": "..."}`
  - Implementation: `src/clojure_mcp/tools/form_edit/`

### File Operations
- **`file_write`**: Writes content to a file with formatting and linting
  - Input: `{"file_path": "/path/to/file.clj", "content": "(ns my.namespace)\n\n(defn my-function [x]\n  (* x 2))"}`
  - Output: Status indicating creation or update, plus a diff showing changes
  - Performs Clojure-aware linting and formatting on the content
  - Returns a detailed diff showing the changes made to the file
  - Implementation: `src/clojure_mcp/tools/file_write/`
  - Best practices:
    - Use `read_file` first to understand file contents and context
    - Use `list_directory` to verify parent directories exist for new files

### Filesystem Tools
- **`directory_tree`**: Shows a recursively indented tree view of files and directories
  - Input: `{"path": "/path/to/directory", "max_depth": 2}`
  - Output: Text representation of the directory tree with proper indentation
  - Filters out temporary files like Emacs backups and hidden files
  - Adds "..." indicators for directories truncated due to max_depth
  - Implementation: `src/clojure_mcp/tools/directory_tree/`

- **`fs_list_directory`**: Lists files and directories at a specified path
  - Input: `{"path": "/path/to/directory"}`
  - Output: Formatted directory listing with files and subdirectories
  - Implementation: `src/clojure_mcp/tools/list_directory/`

- **`fs_read_file`**: Reads file contents with optional line limits and offsets
  - Input: `{"path": "/path/to/file", "offset": 0, "limit": 2000}`
  - Output: File contents wrapped in XML tags with metadata
  - Implementation: `src/clojure_mcp/tools/read_file/`

- **`glob_files`**: Fast file pattern matching using glob patterns
  - Input: `{"path": "/path/to/directory", "pattern": "**/*.clj", "max_results": 1000}`
  - Output: JSON with filenames (sorted by modification time), count, duration, and truncation flag
  - Supports standard glob patterns like "**/*.js" (recursive) or "src/*.ts" (single directory)
  - Implementation: `src/clojure_mcp/tools/glob_files/`

- **`fs_grep`**: Fast content search tool for finding text patterns in files
  - Input: `{"path": "/path/to/directory", "pattern": "function\\s+\\w+", "include": "*.{js,ts}", "max_results": 1000}`
  - Output: JSON with matching files (sorted by modification time), count, and duration
  - Uses system grep command with extended regex support when available
  - Provides pure Java fallback implementation for cross-platform compatibility
  - Supports multiple file types through brace expansion (e.g., "*.{clj,md}")
  - Implementation: `src/clojure_mcp/tools/grep/`

### Project Tools
- **`clojure_inspect_project`**: Analyzes project structure and dependencies
  - Implementation: `src/clojure_mcp/tools/project/`

## MCP Resources

The MCP server provides several resources that can be accessed by clients:

- **`custom://project-summary`**: Serves the PROJECT_SUMMARY.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://readme`**: Serves the README.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://claude`**: Serves the CLAUDE.md file with instructions
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://project-info`**: Provides dynamic information about the project structure
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

## Architecture

### Multimethod-Based Tool System

The project implements tools using a centralized multimethod-based architecture:

1. **Core Multimethods** (defined in `tool_system.clj`):
   - `tool-name`, `tool-description`, `tool-schema`: Define metadata
   - `validate-inputs`, `execute-tool`, `format-results`: Define behavior 
   - `registration-map`: Creates MCP registration map

2. **Improved Separation of Concerns**:
   - Each tool has two main components:
     - `core.clj`: Pure business logic independent of MCP
     - `tool.clj`: MCP-specific implementation using the multimethods

3. **Standardized Error Handling**:
   - Centralized validation with clear error messages
   - Consistent response format for both success and error states

### MCP Server Layer

- Implemented in `src/clojure_mcp/core.clj`
- Handles MCP protocol communication via `io.modelcontextprotocol.sdk/mcp`
- Creates and registers tools, prompts, and resources
- Uses `StdioServerTransportProvider` for communication
- Maintains the nREPL client atom with ::nrepl-user-dir and ::allowed-directories settings

### nREPL Client Layer

- Implemented in `src/clojure_mcp/nrepl.clj`
- Manages connection and communication with nREPL
- Handles code evaluation and result formatting

### Tool Implementation Layer

- Organized in `src/clojure_mcp/tools/` directory
- Each tool follows the pattern of core.clj for logic and tool.clj for MCP integration
- Uses `rewrite-clj` for code parsing and manipulation
- Implements specific multimethods for each tool type

### Resource Layer

- Implemented in `src/clojure_mcp/resources.clj`
- Defines resources that can be served by the MCP server
- Provides utility functions for creating different types of resources

### Prompt Layer

- Implemented in `src/clojure_mcp/prompts.clj`
- Defines prompts for the MCP server
- Provides access to template-based prompts

## Form Editing Pipeline

For form editing operations, the project uses a standardized pipeline pattern:

1. **Context Map**: Operations use a context map (with ::namespaced keys) to pass data through a series of processing functions
2. **Thread-Ctx**: The `thread-ctx` function chains pipeline functions while short-circuiting on error
3. **Pipeline Components**: Individual functions perform specific operations and update the context map:
   - `load-source`: Loads file content into the context
   - `parse-source`: Parses source into a zipper
   - `find-form`: Locates the top-level form to modify
   - `edit-form`: Performs the actual edit operation
   - `format-source`: Formats code according to Clojure conventions
   - `generate-diff`: Creates a human-readable diff of changes
   - `save-file`: Writes content to file with offset tracking
   - `emacs-set-auto-revert`, `highlight-form`: Provides integration with Emacs

This pattern enables:
- Clear separation of concerns
- Consistent error handling
- Reusable components across different tools
- Easy extension with new processing steps

## Development Workflow

### Build Commands
- Run REPL with MCP server: `clojure -X:mcp` (starts on port 7888)
- Run all tests: `clojure -X:test`
- Run single test: `clojure -X:test :dirs '["test"]' :include '"repl_tools_test"'`
- Run linter: `clojure -M:lint` (checks src directory)
- Build JAR: `clojure -T:build ci`
- Install locally: `clojure -T:build install`

### Test Environment Setup
For testing tools, the project provides test utilities:
- Dynamic test fixtures (`use-fixtures`)
- Temporary directories and files
- Mock nREPL client setup
- Integration tests with real nREPL server

### Recommended REPL-Driven Development Pattern

When developing with this tool:

1. **Start with exploration**: Use namespace and symbol tools to understand available functionality
2. **Develop incrementally**: Evaluate small pieces of code to verify correctness
3. **Build up solutions**: Chain successful evaluations into complete solutions
4. **Edit with care**: Use the specialized editing tools that maintain correct syntax
5. **Verify saved code**: After editing files, re-evaluate to ensure correctness
6. **Access documentation**: Use resource URLs to access project documentation when needed
7. **File access**: Use filesystem tools to navigate and read code files when needed

## Code Style Guidelines
- **Imports**: Use `:require` with ns aliases (e.g., `[clojure.string :as string]`)
- **Naming**: Use kebab-case for vars/functions; end predicates with `?` (e.g., `is-top-level-form?`)
- **Documentation**: Provide docstrings for all public vars and functions
- **Error handling**: Use `try/catch` with general exception handling and `ex-info`/`ex-data` for contextualized errors
- **Formatting**: 2-space indentation; maintain whitespace in edited forms
- **Namespaces and File Paths**: 
  - Namespaces use dashes: `clojure-mcp.tools.file-write.core`
  - File paths use underscores: `clojure_mcp/tools/file_write/core.clj`
  - Example: Namespace `clojure-mcp.tools.file-write.core` corresponds to file path `src/clojure_mcp/tools/file_write/core.clj`
- **Testing**: Use `deftest` with descriptive names; `testing` for subsections; `is` for assertions

## MCP Tool Guidelines
- Include clear tool `:description` for LLM guidance
- Validate inputs and provide helpful error messages 
- Each tool returns a vector of strings and a boolean error flag
- Implement all required multimethods for each tool

## Extension Points

To extend this project:

1. **Add new tools**:
   - Create a new directory at `src/clojure_mcp/tools/your_tool_name/`
   - Create `core.clj` with pure business logic
   - Create `tool.clj` implementing required multimethods
   - Add tests in `test/clojure_mcp/tools/your_tool_name/`
   - Add the tool to the tool list in `src/clojure_mcp/repl_tools.clj`

2. **Add new prompts**:
   - Add to `src/clojure_mcp/prompts.clj` and include them in `get-all-prompts`

3. **Add new resources**:
   - Add to `src/clojure_mcp/resources.clj` and include them in `get-all-resources`

4. **Enhance the form editing pipeline**:
   - Add new steps in `src/clojure_mcp/tools/form_edit/pipeline.clj`

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