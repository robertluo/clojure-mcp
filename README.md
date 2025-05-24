# Clojure MCP - REPL-Driven Development with AI Assistance

> **⚠️ Alpha Software - Work in Progress**
> 
> This project is in early development and rapidly evolving. While I've found it invaluable for working with Clojure projects and it has significantly improved my development workflow, expect breaking changes, rough edges, and incomplete documentation.
> 
> **🤝 Help Wanted!** If you find this useful, please consider contributing:
> - Report bugs and issues you encounter
> - Suggest improvements or new features
> - Submit pull requests for fixes or enhancements
> - Share your configuration patterns and workflows
> - Help improve documentation and examples
> 
> Your feedback and contributions will help make this tool better for the entire Clojure community!

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

### API Keys for Agent Tools

Several powerful tools (`dispatch_agent`, `architect`, `code_critique`) require API keys from AI providers. These tools use multiple AI models to provide enhanced capabilities like autonomous search, technical planning, and interactive code review.

#### Required API Keys

To use the agent tools, you'll need API keys from one or more of these providers:

- **`GEMINI_API_KEY`** - For Google Gemini models
  - Get your API key at: https://makersuite.google.com/app/apikey
  - Used by: `dispatch_agent`, `architect`, `code_critique`

- **`OPENAI_API_KEY`** - For GPT models
  - Get your API key at: https://platform.openai.com/api-keys
  - Used by: `dispatch_agent`, `architect`, `code_critique`

- **`ANTHROPIC_API_KEY`** - For Claude models
  - Get your API key at: https://console.anthropic.com/
  - Used by: `dispatch_agent`

#### Setting Environment Variables

**Option 1: Export in your shell**
```bash
export ANTHROPIC_API_KEY="your-anthropic-api-key-here"
export OPENAI_API_KEY="your-openai-api-key-here"
export GEMINI_API_KEY="your-gemini-api-key-here"
```

**Option 2: Add to your shell profile** (`.bashrc`, `.zshrc`, etc.)
```bash
# Add these lines to your shell profile
export ANTHROPIC_API_KEY="your-anthropic-api-key-here"
export OPENAI_API_KEY="your-openai-api-key-here"
export GEMINI_API_KEY="your-gemini-api-key-here"
```

#### Configuring Claude Desktop

When setting up Claude Desktop, ensure it can access your environment variables by updating your config.

```json
{
    "mcpServers": {
        "clojure_connect": {
            "command": "/bin/sh",
            "args": [
                "-c",
                "cd /path/to/your/workspace/project && PATH=/your/bin/path:$PATH && clojure -X:mcp :port 7888"
            ],
            "env": {
                "ANTHROPIC_API_KEY": "$ANTHROPIC_API_KEY",
                "OPENAI_API_KEY": "$OPENAI_API_KEY", 
                "GEMINI_API_KEY": "$GEMINI_API_KEY"
            }
        }
    }
}
```

Personally I `source` them right in bash command:

```json
{
    "mcpServers": {
        "clojure_connect": {
            "command": "/bin/sh",
            "args": [
                "-c",
                "source ~/.api_credentials.sh && cd /path/to/your/workspace/project && PATH=/your/bin/path:$PATH && clojure -X:mcp :port 7888"
            ]
        }
    }
}
```

> **Note**: The agent tools will work with any available API key. You don't need all three - just set up the ones you have access to. The tools will automatically select from available models. For now the ANTHROPIC API is limited to the displatch_agent.

### Setting up the project

1. Clone the repository:
   ```bash
   git clone https://github.com/bhauman/clojure-mcp.git
   cd clojure-mcp
   ```

2. Configure Claude Desktop to use the Clojure MCP server:

   Edit your Claude Desktop config file at `~/Library/Application Support/Claude/claude_desktop_config.json`:
   
   ```json
   {
       "mcpServers": {
           "clojure_connect": {
               "command": "/bin/sh",
               "args": [
                   "-c",
                   "cd /path/to/your/workspace/project && PATH=/your/bin/path:$PATH && clojure -X:mcp :port 7888"
                ]
           }
       }
   }
   ```
   
   Replace the paths with your specific configuration:
   - `/your/bin/path` - Path to your binaries (e.g., `/Users/username/.nix-profile/bin`)
   - `/path/to/your/workspace/clojure-mcp` - Path to your local clojure-mcp repo checkout

4. Launch Claude Desktop and start a new conversation.

> If you change the server you must restart Claude Desktop to see the changes.

### Using with Claude Desktop

Claude Desktop integrates with the Clojure MCP server, allowing Claude to:

First you will want to start an nREPL server in the project you want to work on.

It's easier to pick a stable port number for now. Currently the port
defaults to 7888 and you would specify the `:port` either in the `deps.edn`
on in the `claude_desktop_config.json`. You could set it up like

***deps.edn***
```clojure
{
  :aliases {
    ;; this is the env that the mcp is going to execute code
    :nrepl {:extra-paths ["test"] 
            :extra-deps {nrepl/nrepl {:mvn/version "1.3.1"}
                         ch.qos.logback/logback-classic {:mvn/version "1.4.14"}}
            :main-opts ["-m" "nrepl.cmdline" "--port" "7888"]}
	:mcp   {;; required of the stdio server output gets corrupted
	        deps {org.slf4j/slf4j-nop {:mvn/version "2.0.16"}
			      clojure-mcp/clojure-mcp {:local/root "<path to clojure-mcp cloned repo>"}}
            :exec-fn clojure-mcp.main/start-mcp-server
            ;; it needs an nrepl port to talk to
            :exec-args {:port 7888}}
  }
}
```

Now you can hopefully start Claude Desktop and have access to the mcp tools.

It may take a moment to load. 

You can check if everything is hooked up by clicking the `+` in the chat area and you should see `Add from clojure_connect` in the menu.

Right next to the `+` is a settings icon, when you click on that you can which tools are available.

> If you want to design and chat about a solution you can turn the editing tools off.

#### Starting a new conversation

I click the `+` tools and I add
 * resource `PROJECT_SUMMARY.md`  - see below
 * resource `Clojure Project Info` - which introspects the repl and the project
 * resource `LLM_CODE_STYLE.md` - Which is your personal coding style instructions
 * prompt `clojure_repl_system_prompt` - instructions on how to code - cribbed a bit from Clod Code

Then start the chat.

I would start by having giving it a problem then have it design a solution for your review.

Iterate on that a bit then have it either

A. code and validate the idea in the REPL.
B. go started to file editing and then have it validate the code after file editing.

There is a bash tool so it can run tests and it can make commits for you.

Make a branch commit often so that it doesn't blow your work away with bad ideas.

## Project Summary Management

This project includes a workflow for maintaining an LLM-friendly project summary that helps assistants quickly understand the codebase structure.

### How It Works

1. **Creating the Summary**: To generate or update the PROJECT_SUMMARY.md file, use the MCP prompt `create-project-summary`. This prompt will:
   - Analyze the codebase structure
   - Document key files, dependencies, and available tools
   - Generate comprehensive documentation in a format optimized for LLM assistants

2. **Using the Summary**: When starting a new conversation with an assistant:
   - The "Project Summary" resource automatically loads PROJECT_SUMMARY.md
   - This gives the assistant immediate context about the project structure
   - The assistant can provide more accurate help without lengthy exploration

3. **Keeping It Updated**: At the end of a productive session where new features or components were added:
   - Invoke the `create-project-summary` prompt again
   - The system will update the PROJECT_SUMMARY.md with newly added functionality
   - This ensures the summary stays current with ongoing development

This workflow creates a virtuous cycle where each session builds on the accumulated knowledge of previous sessions, making the assistant increasingly effective as your project evolves.

## Learning Curve

> This tool has a learning curve. You may in practice have to remind
> the LLM to develop in the REPL.  You may also have to remind the LLM
> to use the `clojure_edit` family of tools which have linters build
> in to prevent unbalanced parens and the like.

## 🧰 Available Tools

The default tools included in `main.clj` are organized by category to support different workflows:

### Read-Only Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `LS` | Returns a recursive tree view of files and directories | Exploring project structure |
| `read_file` | Smart file reader with pattern-based exploration for Clojure files | Reading files with collapsed view, pattern matching |
| `fs_grep` | Fast content search using regular expressions | Finding files containing specific patterns |
| `glob_files` | Pattern-based file finding | Finding files by name patterns like `*.clj` |
| `think` | Log thoughts for complex reasoning and brainstorming | Planning approaches, organizing thoughts |

### Code Evaluation

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_eval` | Evaluates Clojure code in the current namespace | Testing expressions like `(+ 1 2)` |
| `bash` | Execute shell commands on the host system | Running tests, git commands, file operations |

### File Editing Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_edit` | Structure-aware editing of Clojure forms | Replacing/inserting functions, handling defmethod |
| `clojure_edit_replace_sexp` | Modify expressions within functions | Changing specific s-expressions |
| `file_edit` | Edit files by replacing text strings | Simple text replacements |
| `file_write` | Write complete files with safety checks | Creating new files, overwriting with validation |

### Agent Tools (Require API Keys)

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `dispatch_agent` | Launch agents with read-only tools for complex searches | Multi-step file exploration and analysis |
| `architect` | Technical planning and implementation guidance | System design, architecture decisions |

### Experimental Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `code_critique` | Interactive code review and improvement suggestions | Iterative code quality improvement |

### Key Tool Features

#### Smart File Reading (`read_file`)
- **Collapsed View**: Shows only function signatures for large Clojure files
- **Pattern Matching**: Use `name_pattern` to find functions by name, `content_pattern` to search content
- **defmethod Support**: Handles dispatch values like `"area :rectangle"` or vector dispatches
- **Multi-language**: Clojure files get smart features, other files show raw content

#### Structure-Aware Editing (`clojure_edit`)
- **Form-based Operations**: Target functions by type and identifier, not text matching
- **Multiple Operations**: Replace, insert_before, insert_after
- **Syntax Validation**: Built-in linting prevents unbalanced parentheses
- **defmethod Handling**: Works with qualified names and dispatch values

#### Code Evaluation (`clojure_eval`)
- **REPL Integration**: Executes in the connected nREPL session
- **Helper Functions**: Built-in namespace and symbol exploration tools
- **Multiple Expressions**: Evaluates and partitions multiple expressions
- **Namespace Support**: Optional namespace parameter for context switching

#### Agent System (`dispatch_agent`)
- **Autonomous Search**: Handles complex, multi-step exploration tasks
- **Read-only Access**: Agents have safe, limited tool access
- **Concurrent Execution**: Launch multiple agents for parallel processing
- **Detailed Results**: Returns comprehensive analysis and findings

## 🎛️ Customization

The Clojure MCP server is designed for easy customization. The codebase separates the core MCP server API from the specific implementation:

- **`src/clojure_mcp/core.clj`** - Provides the reusable API for building MCP servers
- **`src/clojure_mcp/main.clj`** - Example implementation showing how to use the API

### Using main.clj as a Template

The `main.clj` file demonstrates the recommended patterns for creating your own customized MCP server:

#### 1. Define Your Resources (`my-resources` function)

```clojure
(defn my-resources [nrepl-client-map working-dir]
  (keep identity
    [(resources/create-file-resource
       "custom://project-summary"
       "PROJECT_SUMMARY.md"
       "Project summary for LLM context"
       "text/markdown"
       (str working-dir "/PROJECT_SUMMARY.md"))
     ;; Add more resources here
     ]))
```

Resources provide context documents to the AI assistant. Common resources include:
- Project documentation (README, PROJECT_SUMMARY)
- Code style guides
- Project structure information
- Configuration files

#### 2. Define Your Prompts (`my-prompts` function)

```clojure
(defn my-prompts [working-dir]
  [{:name "my_custom_prompt"
    :description "Custom prompt for specific workflows"
    :arguments []
    :prompt-fn (prompts/simple-content-prompt-fn
                "Custom Prompt"
                "Your custom prompt content here")}])
```

Prompts provide pre-configured instructions and workflows for the AI assistant.

#### 3. Select Your Tools (`my-tools` function)

```clojure
(defn my-tools [nrepl-client-atom]
  [;; Read-only tools
   (directory-tree-tool/directory-tree-tool nrepl-client-atom)
   (unified-read-file-tool/unified-read-file-tool nrepl-client-atom)
   
   ;; Evaluation tools
   (eval-tool/eval-code nrepl-client-atom)
   
   ;; Editing tools
   (combined-edit-tool/unified-form-edit-tool nrepl-client-atom)
   (file-write-tool/file-write-tool nrepl-client-atom)
   
   ;; Add your custom tools here
   ])
```

Tools are organized by category:
- **Read-only**: File exploration, project inspection
- **Evaluation**: Code execution and testing  
- **Editing**: File modification and code generation
- **Agents**: Advanced AI-powered tools
- **Experimental**: Cutting-edge features

#### 4. Wire Everything Together (`start-mcp-server` function)

```clojure
(defn start-mcp-server [nrepl-args]
  (let [nrepl-client-map (core/create-and-start-nrepl-connection nrepl-args)
        working-dir (config/get-nrepl-user-dir nrepl-client-map)
        mcp (core/mcp-server)]
    
    (reset! core/nrepl-client-atom (assoc nrepl-client-map ::mcp-server mcp))
    
    ;; Register your customizations
    (doseq [resource (my-resources nrepl-client-map working-dir)]
      (core/add-resource mcp resource))
    (doseq [tool (my-tools core/nrepl-client-atom)]
      (core/add-tool mcp tool))
    (doseq [prompt (my-prompts working-dir)]
      (core/add-prompt mcp prompt))))
```

### Creating Your Custom Server

1. **Copy `main.clj`** as a starting point for your custom server
2. **Modify the functions** to include only the tools/resources/prompts you need
3. **Add requires** for any additional tools you want to include
4. **Update your `deps.edn`** to point to your custom server function

Example `deps.edn` configuration:
```clojure
:mcp {:exec-fn my.custom.server/start-mcp-server
      :exec-args {:port 7888}}
```

### Adding New Tools

To add tools not included in the default set:

1. **Find the tool namespace** in `src/clojure_mcp/tools/`
2. **Add the require** to your namespace
3. **Add the tool function call** to your `my-tools` function

Example:
```clojure
(:require [clojure-mcp.tools.my-new-tool.tool :as my-new-tool])

;; In my-tools function:
(my-new-tool/my-new-tool nrepl-client-atom)
```

### Tool Categories and Selection

Choose tools based on your workflow needs:

- **For exploration only**: Include read-only and evaluation tools
- **For active development**: Add editing and file manipulation tools  
- **For AI-assisted workflows**: Include agent and experimental tools
- **For testing**: Include bash tool for running tests

This modular approach lets you create focused, efficient MCP servers tailored to specific development workflows.

## ⚙️ Configuration

The Clojure MCP server supports project-specific configuration through a `.clojure-mcp/config.edn` file in your project's root directory. This configuration provides security controls and customization options for the MCP server.

### Configuration File Location

Create a `.clojure-mcp/config.edn` file in your project root:

```
your-project/
├── .clojure-mcp/
│   └── config.edn
├── src/
├── deps.edn
└── ...
```

### Configuration Options

#### `allowed-directories`
Controls which directories the MCP tools can access for security. Paths can be relative (resolved from project root) or absolute.

#### `emacs-notify` 
Boolean flag to enable Emacs integration notifications.

### Example Configuration

```edn
{:allowed-directories ["." 
                       "src" 
                       "test" 
                       "resources"
                       "dev"
                       "/absolute/path/to/shared/code"
                       "../sibling-project"]
 :emacs-notify false}
```

### Configuration Details

**Path Resolution**: 
- Relative paths (like `"src"`, `"../other-project"`) are resolved relative to your project root
- Absolute paths (like `"/home/user/shared"`) are used as-is
- The project root directory is automatically included in allowed directories

**Security**: 
- Tools validate all file operations against the allowed directories
- Attempts to access files outside allowed directories will fail with an error
- This prevents accidental access to sensitive system files
- the Bash tool doesn't respect these boundaries so be wary

**Default Behavior**:
- Without a config file, only the project directory and its subdirectories are accessible
- The nREPL working directory is automatically added to allowed directories

### Common Configuration Patterns

#### Development Setup
```edn
{:allowed-directories ["." 
                       "src" 
                       "test" 
                       "dev" 
                       "resources"
                       "docs"]
 :emacs-notify false}
```

#### Multi-Project Setup
```edn
{:allowed-directories ["."
                       "../shared-utils"
                       "../common-config"
                       "/home/user/reference-code"]
 :emacs-notify false}
```

#### Restricted Mode (Extra Security)
```edn
{:allowed-directories ["src" 
                       "test"]
 :emacs-notify false}
```

**Note**: Configuration is loaded when the MCP server starts. Restart the server after making configuration changes.

## 📜 Development Practices

### Recommended Workflow

1. **Express the problem** - Clearly state what you want to solve
2. **Develop in the REPL** - Work through solutions incrementally
3. **Validate step-by-step** - Test each expression before moving on
4. **Save to files** - When the solution is working, save it properly
5. **Reload and verify** - Make sure the saved code works

### Best Practices

- **Small steps** - Prefer many small, valid steps over a few large steps
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

## 🔍 Troubleshooting

### Common Issues

#### File Editing Issues

**Problem**: Claude is unable to edit or create files.
**Solution**:
- Verify the filesystem MCP server is properly configured
- Check file paths are correct and within the allowed directories
- Ensure proper permissions on directories
- Try using the specialized Clojure editing tools rather than direct file editing

## 📝 License

This is a private project for now. Not intended for public consumption.
