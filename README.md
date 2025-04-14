# Clojure MCP - REPL-Driven Development with AI Assistance

A Model Context Protocol (MCP) server for Clojure that enables AI assistants (like Claude) to interact directly with a Clojure REPL, providing a collaborative, REPL-driven development workflow between humans and LLMs.

## 🚀 Overview

This project implements an MCP server that connects AI models to a Clojure nREPL, enabling a powerful REPL-driven development workflow. The driving philosophy is:

> Tiny steps with high quality rich feedback is the recipe for the sauce.

The project enables LLMs to:
- Evaluate Clojure code and see immediate results
- Incrementally develop solutions with step-by-step verification
- Navigate and explore namespaces and symbols
- Edit Clojure files with proper formatting
- Look up documentation and source code
- Test code directly in the REPL environment

### Why REPL-Driven Development with AI?

This approach enables:
- **Immediate feedback** - Validate code by running it, not just statically analyzing it
- **Incremental development** - Build solutions in small, verified steps
- **Human oversight** - Keep the programmer in the loop for guidance
- **Functional approach** - Encourage pure functions that are easier to understand and test
- **Enhanced context** - The LLM learns from each step's result, improving future steps

## 📋 Installation

### Prerequisites

- [Clojure](https://clojure.org/guides/install_clojure) (1.11 or later)
- [Java](https://openjdk.org/) (JDK 11 or later)
- [Claude Desktop](https://claude.ai/desktop) (for the best experience)
- [MCP Server for Filesystem](https://github.com/anthropics/ModelContextProtocol) (`npx -y @modelcontextprotocol/server-filesystem`)
- [MCP Server for Git](https://github.com/Anthropic-Labs/mcp-server-git) (optional, for Git integration)

### Setting up the project

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/clojure-mcp.git
   cd clojure-mcp
   ```

2. Configure Claude Desktop to use the Clojure MCP server:

   Edit your Claude Desktop config file at `~/Library/Application Support/Claude/claude_desktop_config.json`:
   
   ```json
   {
       "mcpServers": {
           "filesystem": {
               "command": "/bin/sh",
               "args": [
                   "-c",
                   "PATH=/your/bin/path:$PATH exec npx -y @modelcontextprotocol/server-filesystem /path/to/your/workspace"
                ]
           },
           "git": {
               "command": "/bin/sh",
               "args": [
                   "-c",
                   "PATH=/path/to/python:$PATH exec uvx mcp-server-git --repository /path/to/your/workspace/clojure-mcp/.git"
               ]
           },
           "clojure_connect": {
               "command": "/bin/sh",
               "args": [
                   "-c",
                   "cd /path/to/your/workspace/clojure-mcp && PATH=/your/bin/path:$PATH && clojure -X:mcp"
                ]
           }
       }
   }
   ```
   
   Replace the paths with your specific configuration:
   - `/your/bin/path` - Path to your binaries (e.g., `/Users/username/.nix-profile/bin`)
   - `/path/to/your/workspace` - Path to your workspace directory
   - `/path/to/python` - Path to your Python installation if using Git integration

3. Start the MCP servers before connecting with Claude:
   ```bash
   # Start the Clojure MCP server (in the clojure-mcp directory)
   clojure -X:mcp
   ```
   Note: The filesystem and git servers start automatically when Claude Desktop connects.

4. Launch Claude Desktop and start a new conversation.

### Using with Claude Desktop

Claude Desktop integrates with the Clojure MCP server, allowing Claude to:

1. **Evaluate Clojure code**: Claude can run code and see results immediately
2. **Access the filesystem**: Read and write Clojure files
3. **Access Git**: Interact with your Git repository (if configured)
4. **Navigate project structure**: Understand and modify your Clojure project

To connect Claude to your project:

1. Open Claude Desktop
2. Start a new conversation
3. Claude will automatically connect to the configured MCP servers
4. Ask Claude to work on your Clojure project using REPL-driven development
5. Guide the development process by providing problems to solve

## 🎮 Usage

### Starting the MCP Server

Launch the Clojure MCP server:

```bash
# Start the MCP server with nREPL connection on port 7888
clojure -X:mcp
```

Then connect to it using Claude Desktop or any other MCP-compatible interface.

### Basic Workflow

1. **Initialize Development**:
   - Ask Claude to start with REPL-driven development
   - Specify the namespace you want to work in

2. **Develop Incrementally**:
   - Work step-by-step, validating each expression
   - Claude will evaluate expressions and show results immediately
   - Guide Claude's development process as needed

3. **Complete the Solution**:
   - Once the solution works in the REPL, save it to a proper file
   - Reload the namespace and verify it still works
   - Commit your changes

### Example Sessions

#### Example 1: Fibonacci Sequence

Start a conversation with Claude and give it a problem statement:

```
I'd like to implement a function that finds the Nth Fibonacci number using Clojure. Let's use REPL-driven development to build and test this step by step.
```

Claude will then:
1. Set up a namespace and development environment
2. Experiment with small expressions to verify the approach
3. Build up to a complete solution while validating each step
4. Save the final solution to a file
5. Test the saved solution

The conversation might look like this:

```clojure
;; First, let's create a namespace for our work
(ns fibonacci.core)

;; Let's think about the Fibonacci sequence:
;; 0, 1, 1, 2, 3, 5, 8, 13, ...
;; Let's verify we understand the sequence by calculating a few terms

;; First two terms are 0 and 1
(def fib-0 0)
(def fib-1 1)

;; Calculate next terms manually
(+ fib-0 fib-1)  ;; => 1 (third term)
(+ fib-1 1)      ;; => 2 (fourth term)
(+ 1 2)          ;; => 3 (fifth term)

;; Now let's build a recursive function
(defn fib-recursive [n]
  (if (<= n 1)
    n
    (+ (fib-recursive (- n 1))
       (fib-recursive (- n 2)))))

;; Test our function
(fib-recursive 0)  ;; => 0
(fib-recursive 1)  ;; => 1
(fib-recursive 5)  ;; => 5
(fib-recursive 10) ;; => 55

;; This works, but is inefficient for large values
;; Let's create a more efficient version using memoization

(def fib-memo
  (memoize
   (fn [n]
     (if (<= n 1)
       n
       (+ (fib-memo (- n 1))
          (fib-memo (- n 2)))))))

;; Test our memoized version
(fib-memo 20)  ;; => 6765
(fib-memo 30)  ;; => 832040
```

After testing in the REPL, Claude will save the solution to a file:

```clojure
(ns fibonacci.core
  "Functions for working with Fibonacci numbers")

(defn fib-recursive
  "Calculates the Nth Fibonacci number recursively.
   Warning: Very inefficient for large values of N."
  [n]
  (if (<= n 1)
    n
    (+ (fib-recursive (- n 1))
       (fib-recursive (- n 2)))))

(def fib-memo
  "Efficient memoized version of the Fibonacci function."
  (memoize
   (fn [n]
     (if (<= n 1)
       n
       (+ (fib-memo (- n 1))
          (fib-memo (- n 2)))))))

(defn fibonacci
  "Returns the Nth Fibonacci number.
   Uses the efficient memoized implementation."
  [n]
  (when (neg? n)
    (throw (IllegalArgumentException. "Fibonacci is not defined for negative numbers")))
  (fib-memo n))
```

#### Example 2: Data Transformation Pipeline

Here's another example of developing a data processing pipeline:

```
I want to build a function that processes a collection of maps representing users, filtering out inactive users, extracting certain fields, and sorting by age.
```

Claude will work through the solution:

```clojure
;; Create a sample dataset to work with
(def users
  [{:id 1 :name "Alice" :age 28 :active true :roles [:admin :user]}
   {:id 2 :name "Bob" :age 35 :active false :roles [:user]}
   {:id 3 :name "Charlie" :age 22 :active true :roles [:user]}
   {:id 4 :name "Diana" :age 42 :active true :roles [:manager :user]}
   {:id 5 :name "Evan" :age 19 :active false :roles [:user]}])

;; First, let's filter out inactive users
(filter :active users)
;; => ({:id 1, :name "Alice", :age 28, :active true, :roles [:admin :user]}
;;     {:id 3, :name "Charlie", :age 22, :active true, :roles [:user]}
;;     {:id 4, :name "Diana", :age 42, :active true, :roles [:manager :user]})

;; Now extract just the fields we want
(map #(select-keys % [:id :name :age]) (filter :active users))
;; => ({:id 1, :name "Alice", :age 28}
;;     {:id 3, :name "Charlie", :age 22}
;;     {:id 4, :name "Diana", :age 42})

;; Sort by age
(sort-by :age (map #(select-keys % [:id :name :age]) (filter :active users)))
;; => ({:id 3, :name "Charlie", :age 22}
;;     {:id 1, :name "Alice", :age 28}
;;     {:id 4, :name "Diana", :age 42})

;; Put it all together in a function
(defn process-users [users]
  (->> users
       (filter :active)
       (map #(select-keys % [:id :name :age]))
       (sort-by :age)))

;; Test the function
(process-users users)
;; => ({:id 3, :name "Charlie", :age 22}
;;     {:id 1, :name "Alice", :age 28}
;;     {:id 4, :name "Diana", :age 42})
```

This development style demonstrates the power of the REPL-driven approach, where each step is immediately validated before moving to the next.

## 🧰 Available Tools

### Code Evaluation and Exploration

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_eval` | Evaluates Clojure code | Evaluating `(+ 1 2)` returns `=> 3` |
| `current_namespace` | Shows active namespace | Returns `"user"` |
| `symbol_completions` | Provides completions | Finding all functions starting with "map" |
| `clojure_eval_history` | Shows recent evaluations | Retrieving last 5 evaluated expressions |

### Symbol Information

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `symbol_documentation` | Gets documentation | Docs for `map` function |
| `symbol_metadata` | Gets symbol metadata | Complete metadata for `reduce` |
| `source_code` | Views source code | Source code for `filter` |
| `symbol-search` | Searches symbols | Finding all symbols containing "seq" |

### Namespace Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_list_namespaces` | Lists all namespaces | Showing available namespaces |
| `clojure_list_vars_in_namespace` | Lists namespace vars | All public vars in `clojure.string` |

### File Editing Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_file_outline` | Shows file structure | Overview of a Clojure file |
| `clojure_edit_replace_form` | Replaces a form | Updating a function definition |
| `clojure_edit_insert_before_form` | Inserts before a form | Adding a helper function |
| `clojure_edit_insert_after_form` | Inserts after a form | Adding a new function |

### Project Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_inspect_project` | Analyzes project | Shows deps, structure, etc. |

## 📜 Development Practices

### Recommended Workflow

1. **Express the problem** - Clearly state what you want to solve
2. **Develop in the REPL** - Work through solutions incrementally
3. **Validate step-by-step** - Test each expression before moving on
4. **Save to files** - When the solution is working, save it properly
5. **Reload and verify** - Make sure the saved code works

### Best Practices

- **Small steps** - Prefer many small, valid steps over a few large steps
- **Functional approach** - Use pure functions that are easier to test and reason about
- **Human guidance** - Provide feedback to keep development on track
- **Test early** - Validate ideas directly in the REPL before committing to them

## 🔧 Project Maintenance

```bash
# Run tests
clojure -X:test

# Run specific test
clojure -X:test :dirs '["test"]' :include '"repl_tools_test"'

# Run linter
clojure -M:lint
```

## 📚 Philosophy

The core philosophy of this project is that:

1. **Tiny steps with rich feedback** lead to better quality code
2. **REPL-driven development** provides the highest quality feedback loop
3. **Keeping humans in the loop** ensures discernment and maintainable code
4. **Functional programming patterns** produce more maintainable AI-generated code
5. **Collaborative development** between humans and AI can lead to higher quality than either alone

## 📋 Prompts

The project includes several prompts that guide LLM behavior:

- `clojure_dev` - General Clojure development guidelines
- `clojure-repl-driven` - REPL-driven development workflow
- `clj-spec-driven-modifier` - Spec-driven development guidelines
- `clj-test-driven-modifier` - Test-driven development workflow
- `clj-set-project-dir` - Project context setting
- `clj-sync-namespace` - Namespace synchronization

## 🔍 Troubleshooting

### Common Issues

#### Connection Problems

**Problem**: Claude cannot connect to the Clojure REPL.
**Solution**: 
- Ensure the MCP server is running with `clojure -X:mcp`
- Check the port configuration matches in both Claude Desktop config and the server
- Verify no firewall rules are blocking the connection
- Restart the MCP server and Claude Desktop

#### Namespace Not Found

**Problem**: Claude reports a namespace cannot be found.
**Solution**:
- Ensure the namespace exists in your project
- Use the correct namespace format (with hyphens, not underscores in the file path)
- Check if the namespace needs to be created first
- Verify the namespace is in the correct directory structure

#### Code Evaluation Errors

**Problem**: Clojure code evaluation fails with errors.
**Solution**:
- Examine the error message carefully
- Ask Claude to break down complex operations into smaller steps
- Check that all required namespaces are required
- Ensure variables are defined before they are used

#### File Editing Issues

**Problem**: Claude is unable to edit or create files.
**Solution**:
- Verify the filesystem MCP server is properly configured
- Check file paths are correct and within the allowed directories
- Ensure proper permissions on directories
- Try using the specialized Clojure editing tools rather than direct file editing

### Getting Help

If you encounter issues not covered here:
- Check the project's issue tracker
- Look for common nREPL or Clojure configuration issues
- Ensure you're using compatible versions of all components
- Join the community discussion on [Clojurians Slack](http://clojurians.net/) in the #mcp channel

## 🤝 Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues.

## 📝 License

Copyright © 2025

Distributed under the Eclipse Public License version 1.0.
