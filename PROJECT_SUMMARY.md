# Clojure MCP Project Summary

## Project Overview

Clojure MCP is a Model Context Protocol (MCP) server that enables AI assistants (like Claude) to interact directly with a Clojure REPL. It provides a collaborative, REPL-driven development workflow between humans and LLMs. The core philosophy is "tiny steps with high quality rich feedback" for effective development.

The project allows AI assistants to:
- Evaluate Clojure code and see immediate results
- Incrementally develop solutions with step-by-step verification
- Navigate and explore namespaces and symbols
- Edit Clojure files with proper formatting and structure-aware operations
- Access documentation and source code
- Test code directly in the REPL environment

## Key File Paths and Descriptions

### Core System Files

- `/src/clojure_mcp/core.clj`: Main entry point, sets up the MCP server and tools
- `/src/clojure_mcp/nrepl.clj`: nREPL client implementation for connecting to Clojure REPL
- `/src/clojure_mcp/tool_system.clj`: Defines the multimethod-based architecture for tools
- `/src/clojure_mcp/repl_tools.clj`: Central registry for all available tools
- `/src/clojure_mcp/prompts.clj`: Manages system prompts for AI assistants
- `/src/clojure_mcp/resources.clj`: Manages resources to be exposed to AI assistants
- `/src/clojure_mcp/linting.clj`: Code quality and formatting utilities

### Tool Implementations

- `/src/clojure_mcp/tools/eval/`: Code evaluation tools
- `/src/clojure_mcp/tools/read_file/`: File reading utilities
- `/src/clojure_mcp/tools/form_edit/`: Structure-aware Clojure code editing
- `/src/clojure_mcp/tools/file_edit/`: Basic file editing operations
- `/src/clojure_mcp/tools/unified_file_edit/`: Combined file editing capabilities
- `/src/clojure_mcp/tools/unified_read_file/`: Enhanced file reading with pattern-based code exploration
  - `tool.clj`: Main tool implementation with MCP integration
  - `pattern_core.clj`: Core pattern matching functionality for Clojure code analysis
- `/src/clojure_mcp/tools/directory_tree/`: Filesystem navigation
- `/src/clojure_mcp/tools/grep/`: Content searching in files
- `/src/clojure_mcp/tools/glob_files/`: Pattern-based file finding
- `/src/clojure_mcp/tools/namespace/`: Clojure namespace exploration
- `/src/clojure_mcp/tools/symbol/`: Symbol information and documentation
- `/src/clojure_mcp/tools/project/`: Project structure analysis
- `/src/clojure_mcp/tools/code_critique/`: Code quality feedback
- `/src/clojure_mcp/tools/think/`: Reflective thinking tool for AI assistants

### Resource Directories

- `/resources/prompts/`: System prompts for AI assistants
- `/resources/prompts/system/`: Core system prompts
- `/resources/agent/`: Agent-specific resources
- `/resources/configs/`: Configuration examples
- `/resources/logback.xml`: Logging configuration file

## Dependencies and Versions

### Core Dependencies

- `org.clojure/clojure` (1.11.1): The Clojure language
- `io.modelcontextprotocol.sdk/mcp` (0.8.1): Model Context Protocol SDK
- `nrepl/nrepl` (1.3.1): Network REPL server for Clojure
- `rewrite-clj/rewrite-clj` (1.1.47): Library for parsing and transforming Clojure code
- `dev.weavejester/cljfmt` (0.13.1): Clojure code formatting
- `clj-kondo/clj-kondo` (2024.03.13): Static analyzer and linter for Clojure
- `org.clojure/tools.logging` (1.3.0): Logging abstraction for Clojure
- `ch.qos.logback/logback-classic` (1.4.14): Logback implementation for SLF4J

### AI Integration Dependencies

- `dev.langchain4j/langchain4j` (1.0.0-beta3): Java library for LLM integration
- `dev.langchain4j/langchain4j-anthropic` (1.0.0-beta3): Anthropic-specific integration
- `pogonos/pogonos` (0.2.1): Mustache templating for prompts

## Available Tools and Examples

### Code Evaluation

```clojure
clojure_eval:
  Input: (+ 1 2)
  Output: => 3
```

### File Operations

```clojure
read_file:
  Input: {:path "/path/to/file.clj", 
          :collapsed true,
          :name_pattern "validate.*", 
          :content_pattern "try|catch",
          :include_comments false}
  Output: File contents with pattern-based collapsed view
  
edit_file:
  Input: {:file_path "/path/to/file.clj", :old_string "(defn old", :new_string "(defn new"}
  Output: Diff showing changes made
```

### Clojure-Specific Editing

```clojure
clojure_edit_replace_definition:
  Input: {:file_path "/path/to/file.clj", :form_type "defn", :form_identifier "my-func", :content "(defn my-func [x] (* x 2))"}
  Output: Diff showing syntax-aware function replacement
  
clojure_edit_insert_before_definition:
  Input: {:file_path "/path/to/file.clj", :form_type "defn", :form_identifier "my-func", :content "(def magic-multiplier 2)"}
  Output: Diff showing insertion before the specified function
  
clojure_edit_insert_after_definition:
  Input: {:file_path "/path/to/file.clj", :form_type "defn", :form_identifier "my-func", :content "(deftest my-func-test (is (= 4 (my-func 2))))"}
  Output: Diff showing insertion after the specified function

# Examples with namespace-qualified forms and defmethod

clojure_edit_replace_definition:
  Input: {:file_path "/path/to/file.clj", 
          :form_type "defmethod", 
          :form_identifier "tool-system/validate-inputs :clojure-eval", 
          :content "(defmethod tool-system/validate-inputs :clojure-eval [_ inputs]\n  (validate-clojure-eval-inputs inputs))"}
  Output: Diff showing replacement of a specific multimethod implementation
```

### Code Search and Navigation

```clojure
glob_files:
  Input: {:pattern "**/*.clj"}
  Output: List of matching file paths
  
fs_grep:
  Input: {:pattern "defn my-func"}
  Output: List of files containing the pattern
  
symbol_search:
  Input: {:search-str "map"}
  Output: List of symbol names containing "map"
```

### Project Information

```clojure
clojure_inspect_project:
  Input: {}
  Output: Detailed project structure information
```

## Collapsed View and Pattern-Based Code Exploration

The `read_file` tool provides a powerful code exploration feature through its pattern-based collapsed view:

### Key Features

1. **Collapsed View**: Shows only function signatures by default, making large files navigable
2. **Pattern Matching**: Expands functions matching specific patterns
   - `name_pattern`: Regex to match function names (e.g., "validate.*")
   - `content_pattern`: Regex to match function content (e.g., "try|catch")
3. **Comment Control**: Option to include/exclude comments with `include_comments`
4. **Markdown Formatting**: Results are formatted in markdown with:
   - File header and pattern information
   - Syntax-highlighted code blocks
   - Usage tips for further exploration

### Implementation

The implementation uses rewrite-clj to:
1. Parse Clojure code into zipper structures
2. Collect top-level forms with metadata
3. Apply regex pattern matching to function names and content
4. Generate a collapsed view with selected expansions
5. Support defmethod forms with special handling for:
   - Keyword dispatch values (e.g., `:rectangle`)
   - Vector dispatch values (e.g., `[:feet :inches]`)
   - Namespace-qualified multimethod names (e.g., `tool-system/validate-inputs`)

### Usage Examples

```clojure
;; Basic collapsed view of a file
{:path "/path/to/file.clj", :collapsed true}

;; Find all functions with 'validate' in their names
{:path "/path/to/file.clj", :name_pattern "validate"}

;; Find all error handling code
{:path "/path/to/file.clj", :content_pattern "try|catch|throw"}

;; Combined patterns - find validation functions that handle errors
{:path "/path/to/file.clj", :name_pattern "validate", :content_pattern "try|catch"}

;; Include comments in search
{:path "/path/to/file.clj", :content_pattern "TODO", :include_comments true}

;; View entire file without collapsing
{:path "/path/to/file.clj", :collapsed false}

;; Working with defmethod forms
{:path "/path/to/file.clj", :name_pattern "area :rectangle"}                   ;; Find specific dispatch value
{:path "/path/to/file.clj", :name_pattern "dispatch-with-vector \\[:feet :inches\\]"} ;; Find vector dispatch value
{:path "/path/to/file.clj", :name_pattern "tool-system/validate-inputs"}       ;; Find namespaced multimethods
```

## Architecture and Design Patterns

### Core Architecture Components

1. **MCP Server**: Entry point that exposes tools to AI assistants
2. **nREPL Client**: Connects to the Clojure REPL for code evaluation
3. **Tool System**: Extensible multimethod-based architecture for defining tools
4. **Prompt System**: Provides context and guidance to AI assistants

### Key Implementation Patterns

1. **Multimethod Dispatch**: The tool system uses multimethods for extensibility:
   - `tool-name`: Determines the name of a tool
   - `tool-description`: Provides human-readable description
   - `tool-schema`: Defines the input/output schema
   - `validate-inputs`: Validates tool inputs
   - `execute-tool`: Performs the actual operation
   - `format-results`: Formats the results for the AI

2. **Core/Tool Separation**: Each tool follows a pattern:
   - `core.clj`: Pure functionality without MCP dependencies
   - `tool.clj`: MCP integration layer using the tool system

3. **Structured Clojure Code Editing**: Uses rewrite-clj to:
   - Parse Clojure code into zipper structure
   - Perform structure-aware transformations
   - Maintain proper formatting and whitespace

4. **REPL-Driven Development**: All tools designed to support:
   - Incremental development
   - Immediate feedback
   - Step-by-step verification

5. **Pattern-Based Code Exploration**: The `read_file` tool supports:
   - Regular expression matching for function names with `name_pattern`
   - Content-based pattern matching with `content_pattern`
   - Focused code reading with collapsed view and selective expansion
   - Markdown-formatted output with usage hints

## Development Workflow Recommendations

1. **Setup and Configuration**:
   - Configure Claude Desktop with the Clojure MCP server
   - Set up file system and Git integration if needed

2. **REPL-Driven Development**:
   - Start with small, incremental steps
   - Evaluate code in the REPL to verify correctness
   - Save working code to files when verified

3. **Tool Usage Best Practices**:
   - Use `clojure_eval` for testing code snippets
   - Use `clojure_edit_*` tools for syntax-aware code editing

4. **Logging System**:
   - Uses `clojure.tools.logging` with Logback backend
   - Logs are written to `logs/clojure-mcp.log` with daily rotation
   - Configure log levels in `resources/logback.xml`
   - Server startup/shutdown and errors are logged automatically
   - Add logging in your code with:
     ```clojure
     (ns your.namespace
       (:require [clojure.tools.logging :as log]))
       
     (log/debug "Debug message")
     (log/info "Info message")
     (log/warn "Warning message")
     (log/error "Error message")
     (log/error exception "Error with exception")
     ```

5. **Project Maintenance**:
   - Run tests with `clojure -X:test`
   - Update this project summary after significant changes

6. **Pattern-Based Code Exploration**:
   - Use the enhanced `read_file` tool for efficient codebase navigation
   - Combine `name_pattern` and `content_pattern` to focus on relevant code
   - Find specific defmethod implementations using their dispatch values
   - Examples:
     - Find all validation functions: `{:name_pattern "validate.*"}`
     - Find error handling: `{:content_pattern "try|catch|throw"}`
     - Find where a specific function is used: `{:content_pattern "some-important-function"}`
     - Find a specific defmethod: `{:name_pattern "area :rectangle"}`
     - Find defmethod with vector dispatch: `{:name_pattern "dispatch-with-vector \\[:feet :inches\\]"}`
   - Markdown-formatted output includes useful tips and pattern match information

## Extension Points

1. **Adding New Tools**:
   - Create a new tool namespace in `/src/clojure_mcp/tools/`
   - Implement the required multimethods from `tool-system`
   - Register the tool in `repl_tools.clj`

2. **Enhancing Prompt System**:
   - Add new prompts in `/resources/prompts/`
   - Register them in `prompts.clj`

3. **Improving Code Editing**:
   - Extend form editing capabilities in `tools/form_edit/core.clj`
   - Add specialized tools for common editing patterns

4. **Language Model Integration**:
   - Explore langchain4j integration for more advanced AI capabilities
   - Implement feedback mechanisms for model improvements

5. **IDE Integration**:
   - Extend `/src/clojure_mcp/utils/emacs_integration.clj` for better editor support
   - Add support for VS Code or other editors

This project summary is designed to provide AI assistants with a quick understanding of the Clojure MCP project structure and capabilities, enabling more effective assistance with minimal additional context.
