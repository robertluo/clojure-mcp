# Clojure MCP - LLM Assistant Guide

## Project Overview

Clojure MCP is a Model Context Protocol (MCP) server that enables LLM assistants to interact directly with a Clojure REPL environment. The core concept is to facilitate REPL-driven development where AI assistants can evaluate Clojure code in real-time, providing immediate feedback and enabling step-by-step verification of solutions.

## Key Project Files

### Core Implementation
- **`src/clojure_mcp/core.clj`**: Main entry point and server setup, creates the MCP server and registers tools
- **`src/clojure_mcp/nrepl.clj`**: Manages communication with nREPL server
- **`src/clojure_mcp/repl_tools.clj`**: Centralizes and exports all REPL tool functions
- **`src/clojure_mcp/resources.clj`**: Provides resource definitions for the MCP server

### Tool Implementations
- **`src/clojure_mcp/repl_tools/eval.clj`**: Code evaluation tools
- **`src/clojure_mcp/repl_tools/history.clj`**: Evaluation history management
- **`src/clojure_mcp/repl_tools/namespace.clj`**: Namespace exploration and management
- **`src/clojure_mcp/repl_tools/symbol.clj`**: Symbol documentation, metadata and search
- **`src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`**: Code editing tools
- **`src/clojure_mcp/repl_tools/project/inspect.clj`**: Project inspection tools

### Configuration
- **`deps.edn`**: Project dependencies and aliases
- **`.mcp.json`**: MCP server configuration

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
        }}
```

## Available MCP Tools

### Code Evaluation
- **`clojure_eval`**: Evaluates Clojure code in the REPL
  - Input: `{"expression": "(+ 1 2)"}`
  - Output: Evaluation result and/or error messages
  - Implementation: `src/clojure_mcp/repl_tools/eval.clj`

### REPL History
- **`clojure_eval_history`**: Retrieves previous evaluation results
  - Input: `{"number-to-fetch": 5}`
  - Implementation: `src/clojure_mcp/repl_tools/history.clj`

### Namespace Management
- **`current_namespace`**: Gets the active namespace
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`
- **`clojure_list_namespaces`**: Lists all available namespaces
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`
- **`clojure_list_vars_in_namespace`**: Lists vars in a namespace
  - Input: `{"namespace": "clojure.string"}`
  - Implementation: `src/clojure_mcp/repl_tools/namespace.clj`

### Symbol Information
- **`symbol_completions`**: Gets completions for symbol prefixes
  - Input: `{"prefix": "map"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol_metadata`**: Retrieves detailed metadata for symbols
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol_documentation`**: Gets docstrings and usage information
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`source_code`**: Views source code implementation
  - Input: `{"symbol": "map"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`
- **`symbol-search`**: Searches for symbols across namespaces
  - Input: `{"search-str": "seq"}`
  - Implementation: `src/clojure_mcp/repl_tools/symbol.clj`

### File Editing
- **`clojure_file_structure`**: Shows file structure with top-level forms
  - Input: `{"file_path": "src/my_namespace/core.clj", "expand": ["my-function"]}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_replace_form`**: Updates top-level forms
  - Input: `{"file_path": "...", "form_name": "my-function", "form_type": "defn", "new_implementation": "..."}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_insert_before_form`**: Inserts code before existing forms
  - Input: `{"file_path": "...", "before_form_name": "main-fn", "form_type": "defn", "new_form_str": "..."}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_insert_after_form`**: Inserts code after existing forms
  - Input: `{"file_path": "...", "after_form_name": "helper-fn", "form_type": "defn", "new_form_str": "..."}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_comment_block`**: Edits or updates comment blocks
  - Input: `{"file_path": "...", "comment_substring": "Example usage", "new_content": "..."}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`
- **`clojure_edit_replace_docstring`**: Updates function docstrings
  - Input: `{"file_path": "...", "form_name": "my-function", "form_type": "defn", "new_docstring": "..."}`
  - Implementation: `src/clojure_mcp/repl_tools/top_level_form_edit_pipeline.clj`

### Project Tools
- **`clojure_inspect_project`**: Analyzes project structure and dependencies
  - Implementation: `src/clojure_mcp/repl_tools/project/inspect.clj`

## Available MCP Resources

The MCP server provides several resources that can be accessed by clients:

- **`custom://project-summary`**: Serves the PROJECT_SUMMARY.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://readme`**: Serves the README.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://big-ideas`**: Serves the BIG_IDEAS.md file
  - MIME Type: text/markdown
  - Implementation: `src/clojure_mcp/resources.clj`

- **`custom://repl-info`**: Provides dynamic information about the REPL session
  - MIME Type: application/json
  - Content: JSON with current namespace, Clojure version, current directory, loaded libraries, etc.
  - Implementation: `src/clojure_mcp/resources.clj`

### Resource Implementation

Resources in the MCP server are implemented using:

1. **Resource Definition**:
   ```clojure
   {:url "custom://resource-name"
    :name "Human-readable Name"
    :description "Resource description"
    :mime-type "text/plain"
    :resource-fn (fn [_ _ callback] (callback ["Content as vector of strings"]))}
   ```

2. **Resource Registration**:
   - Resources are registered in the `nrepl-mcp-server` function using `add-resource`
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

## Development Workflow

1. The `nrepl-mcp-server` function in `core.clj` is the main entry point
2. It creates both an nREPL client and MCP server
3. Tools are registered with the MCP server using `add-tool`
4. Resources are registered with the MCP server using `add-resource`
5. Each tool and resource has a standard structure using continuation functions

## Recommended REPL-Driven Development Pattern

When developing with this tool:

1. **Start with exploration**: Use namespace and symbol tools to understand available functionality
2. **Develop incrementally**: Evaluate small pieces of code to verify correctness
3. **Build up solutions**: Chain successful evaluations into complete solutions
4. **Edit with care**: Use the specialized editing tools that maintain correct syntax
5. **Verify saved code**: After editing files, re-evaluate to ensure correctness
6. **Access documentation**: Use resource URLs to access project documentation when needed

## Important Implementation Notes

1. **Error Handling**: Always check for errors in evaluation results
2. **Namespace Context**: Be aware of the current namespace when evaluating code
3. **File Editing**: The file editing tools are designed to maintain correct Clojure syntax
4. **Async Tools**: All tool implementations use a callback continuation style for async results
5. **Prompts**: The system includes several built-in prompts for guiding development
6. **Resources**: Resources should return vectors of strings, which are wrapped in TextResourceContents

## Project Goals

The primary goal is to enable high-quality collaborative development between humans and AI through:

1. Immediate feedback via REPL evaluation
2. Incremental development with step-by-step verification
3. Human oversight for maintaining code quality
4. Functional programming patterns that produce more maintainable code
5. Access to contextual project information via resources

## Extension Points

If you need to extend this project:

1. New tools can be added in `src/clojure_mcp/repl_tools/`
2. Register tools in `nrepl-mcp-server` function in `core.clj`
3. New prompts can be added in `src/clojure_mcp/prompts.clj`
4. New resources can be added in `src/clojure_mcp/resources.clj`
