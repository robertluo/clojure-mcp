# Other Tools - Deprecated but Preserved

## Overview

This directory contains tools that have been moved out of active use in the main Clojure MCP server. While these tools are **not registered in `main.clj`** and therefore not available by default, they remain fully functional with passing tests.

## Status: Deprecated but Valuable

These tools were moved here because they **found little use** in typical Clojure development workflows. However, they retain value as:

- **Reference implementations** for building new tools
- **Examples** of the tool-system architecture 
- **Specialized tools** for custom MCP servers or specific use cases
- **Learning resources** for understanding the multimethod-based tool pattern

## Available Tools

### File System Operations
- **`create_directory/`** - Create directories and nested directory structures
- **`list_directory/`** - List files and directories with formatted output
- **`move_file/`** - Move and rename files/directories

### Clojure Introspection Tools  
- **`namespace/`** - Namespace exploration and analysis
  - `current_namespace` - Get the current REPL namespace
  - `clojure_list_namespaces` - List all loaded namespaces
  - `clojure_list_vars_in_namespace` - Inspect vars in a specific namespace

- **`symbol/`** - Symbol information and documentation
  - `symbol_completions` - Get symbol completions for a prefix
  - `symbol_metadata` - Retrieve complete symbol metadata
  - `symbol_documentation` - Get symbol documentation and arglists
  - `source_code` - Retrieve source code for symbols
  - `symbol_search` - Search for symbols across all namespaces

## Usage Options

### 1. Re-activate for Main Server
To make any of these tools available in the main MCP server:

```clojure
;; In main.clj, add to imports:
[clojure-mcp.other-tools.create-directory.tool :as create-dir-tool]

;; In my-tools function, add:
(create-dir-tool/create-directory-tool nrepl-client-atom)
```

### 2. Custom MCP Server
Create a specialized MCP server using the core API:

```clojure
(ns my-custom-server
  (:require [clojure-mcp.core :as core]
            [clojure-mcp.other-tools.namespace.tool :as ns-tool]
            [clojure-mcp.other-tools.symbol.tool :as symbol-tool]))

(defn create-introspection-server []
  (let [mcp (core/mcp-server)]
    (core/add-tool mcp (ns-tool/current-namespace-tool nrepl-client-atom))
    (core/add-tool mcp (symbol-tool/symbol-search-tool nrepl-client-atom))
    mcp))
```

### 3. Direct Usage in Code
All tools can be used directly via their registration functions:

```clojure
(require '[clojure-mcp.other-tools.create-directory.tool :as create-dir])
(def tool-fn (:tool-fn (create-dir/create-directory-tool client-atom)))
```

## Architecture Reference

Each tool follows the standard pattern:
- **`core.clj`** - Pure functionality without MCP dependencies
- **`tool.clj`** - MCP integration using the tool-system multimethods
- **Corresponding tests** in `/test/clojure_mcp/other_tools/`

This makes them excellent examples for understanding how to build new tools using the multimethod-based architecture.

## Maintenance Status

- ✅ **Fully tested** - All tools have comprehensive test suites
- ✅ **Functionally complete** - Tools work as designed
- ⚠️ **Not actively maintained** - No new features planned
- ⚠️ **Deprecated from main server** - Not available by default

## Contributing

While these tools are deprecated from the main server, improvements are welcome:
- Bug fixes and maintenance updates
- Better documentation and examples
- Performance improvements
- Enhanced error handling

However, consider whether new features might be better implemented as part of the active tool set instead.
