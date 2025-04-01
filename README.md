# Clojure MCP - REPL-Driven Development with LLMs

A Model Context Protocol (MCP) server for Clojure that enables LLMs to interact with a Clojure REPL, promoting incremental, REPL-driven development workflows with AI assistance.

## Overview

This project implements an MCP server that connects LLMs to a Clojure nREPL, providing tools that allow LLMs to:

- Evaluate Clojure code and see immediate results
- Get documentation, metadata, and source code for symbols
- Search for functions and explore namespaces
- Edit top-level forms in Clojure files
- Access evaluation history

The goal is to enable a collaborative, REPL-driven development workflow between humans and LLMs that encourages:

- Small, incremental steps with immediate feedback
- Focused exploration through a REPL rather than large, untested code generation
- Functional programming patterns with simple data transformations
- Human guidance and oversight during the development process

## Getting Started

```bash
# Start the MCP server with nREPL connection on port 7888
clojure -X:mcp

# Run tests
clojure -X:test

# Run specific test
clojure -X:test :dirs '["test"]' :include '"repl_tools_test"'

# Run linter
clojure -M:lint
```

## Tools

The MCP server exposes the following tools to LLMs:

### Code Evaluation and Exploration

| Tool Name | Description | Arguments |
|-----------|-------------|-----------|
| `clojure_eval` | Evaluates Clojure code in the current namespace | `expression`: String containing Clojure code to evaluate |
| `current_namespace` | Returns the current active namespace | None |
| `symbol_completions` | Provides completion candidates for symbol prefixes | `prefix`: String prefix to get completions for |
| `clojure_eval_history` | Returns recently evaluated expressions | `number-to-fetch`: Number of history items to retrieve |

### Symbol Information

| Tool Name | Description | Arguments |
|-----------|-------------|-----------|
| `symbol_documentation` | Returns documentation for a symbol | `symbol`: Symbol name (e.g., "map" or "clojure.core/map") |
| `symbol_metadata` | Returns complete metadata for a symbol | `symbol`: Symbol name |
| `source_code` | Returns source code for a function | `symbol`: Symbol name |
| `symbol-search` | Searches for symbols using apropos | `search-str`: String to search for in symbol names |

### Namespace Exploration

| Tool Name | Description | Arguments |
|-----------|-------------|-----------|
| `clojure_list_namespaces` | Lists all loaded namespaces | None |
| `clojure_list_vars_in_namespace` | Lists public vars in a namespace | `namespace`: Namespace name (e.g., "clojure.string") |

### Code Editing

| Tool Name | Description | Arguments |
|-----------|-------------|-----------|
| `top_level_form_edit` | Edits a top-level form in a Clojure file | `form_name`: Name of the form to edit<br>`file_path`: Path to the file<br>`form_type`: Form type (e.g., "defn", "def", "ns")<br>`new_implementation`: String with the new form implementation |

## Example Tool Usage

### Evaluating Code
```clojure
// Tool: clojure_eval
// Args: { "expression": "(+ 1 2)" }
// Result: ["=> 3"]
```

### Getting Documentation
```clojure
// Tool: symbol_documentation
// Args: { "symbol": "map" }
// Result: ["([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls])
//          Returns a lazy sequence consisting of the result of applying f to..."]
```

### Editing a Function
```clojure
// Tool: top_level_form_edit
// Args: {
//   "form_name": "my-function",
//   "file_path": "/path/to/file.clj",
//   "form_type": "defn",
//   "new_implementation": "(defn my-function [x y]\n  (+ x y))"
// }
```

## Prompts

MCP prompts to guide LLM behavior:

- `clojure-dev-prompt` - General Clojure development guidance
- `clojure-repl-driven-prompt` - REPL-driven development workflow
- `clojure-spec-driven-modifier` - Integration with Clojure spec
- `clojure-test-driven-modifier` - Test-driven development
- `clojure-project-context-modifier` - Project-specific context
- `clj-sync-namespace` - Namespace synchronization

## Philosophy

From `BIG_IDEAS.md`:

> Tiny steps with high quality rich feedback is the recipe for the sauce.

This project explores how LLMs can integrate with a REPL workflow to create high-quality, maintainable Clojure code through incremental steps and immediate feedback, keeping human programmers in the loop for guidance and oversight.

## License

Copyright © 2025 Bruce

Distributed under the Eclipse Public License version 1.0.