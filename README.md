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

A Model Context Protocol (MCP) server for Clojure that provides a
complete set of tools to aid in the development of Clojure projects.

## TLDR: what does this all mean for me?

With Clojure MCP alone you can turn an LLM into a powerful Clojure
REPL and coding assistant.

**LLMs excel in the Clojure REPL:** Current LLMs are unarguably
fantastic Clojure REPL assistants that perform evaluations quickly and
much more effectively than you can imagine. Ask anyone who has
experienced this and they will tell you that the LLMs are performing
much better in the Clojure REPL than they would have
imagined. Additionally, we must remember that the form and
maintainability of ephemeral code DOES NOT MATTER.

**Buttery Smooth Clojure Editing:** With current editing tools, LLMs
still struggle with the parenthesis. Clojure MCP has a different take
on editing that increases edit acceptance rates significantly. Clojure
MCP lints code coming in, fixes parenthesis if possible, uses
clj-rewrite to apply syntax aware patches, and then lints and formats
the final result. This is a powerful editing pipeline that vastly
outperforms when it comes to editing Clojure Code.

Together these two features along with a set of other Clojure aware
tools create a new and unique LLM development experience that you
probably should try at least once to understand how transformational
it is.

## 🚀 Overview

This project implements an MCP server that connects AI models to a
Clojure nREPL, and Specialized Clojure editing tools enabling a unique
Clojure develop experience.

Clojure MCP provides a superset of the tools that Claude Code uses,
so you can use it to work on Clojure **without any other tools**.  I
highly recommend using it with Claude Desktop to start.  It's
more attractive and there are **no api charges!**. Claude Desktop, also let's you
have quick access to **your own prompts** and other resources provided
by the clojure-mcp server. Having a stack of your own prompts
available in a UI menu is very convenient.

> If you use the built in Agent tools you will accumulate API charges.

## Main Features

- **Clojure REPL Connection** - which lints the eval and auto-balances parens
- **Clojure Aware editing** - Using clj-kondo, parinfer, cljfmt, and clj-rewrite
- **Optimized set of tools for Clojure Development** superset of Claude Code
- **Emacs edit highlighting** - alpha

### Why REPL-Driven Development with AI?

For Clojurists an LLM assisted REPL is the killer application.

LLMs can:
* **Iterate** on code in the REPL and when finished present the findings before adding them to your code
* **Validate** and probe your code for errors
* **Debug** your code in the REPL
* and much more

Additionally, in some LLM clients (including Claude Desktop), you can
control which tools are available to the model at any given moment so
you can easily remove the ability to edit files and restrict the model
to the REPL tool and force the use of the REPL.

## 🧠 Model Compatibility

These tools are designed to work with the latest LLM models. For the best experience with sexp editing and Clojure-specific tooling, we recommend:

- **Anthropic Claude 3.7** and **Claude 4 (sonnet or opus)** (especially **Claude 4** for best results)
- **Gemini 2.5**
- **OpenAI o4-mini** or **o3**

I highly recommend **Claude 4** if you want to see long autonomous agentic action chains ...

The pattern-based structural editing tools require high model performance, so using one of these recommended models will significantly improve your experience.

## Cohesive Clojure Toolbox

### Why These Tools Work as a Complete System

The Clojure MCP tools are intentionally designed as a **cohesive "action space"** for Clojure development, rather than a collection of independent utilities. This design approach offers several key advantages:

#### Enhanced Clojure Integration
- **Smart file editing** with automatic parenthesis balancing, linting, and formatting
- **Structure-aware operations** that understand Clojure syntax and semantics  
- **REPL-integrated development** with stateful namespace management

#### Stateful File Tracking
The tools maintain state about file read/write operations to ensure safety:
- Tracks when files were last read vs. modified externally
- Prevents editing conflicts by validating file state before modifications
- Enables multiple sequential edits after a single read operation
- Uses canonical path resolution for reliable file identification

#### Optimized Tool Interactions
When tools work together as a system, they can:
- Share context and state for more intelligent behavior
- Provide consistent interfaces and error handling
- Optimize the overall development workflow

### Using with Claude Code and Other Code Assistants

While you *can* use these tools alongside Claude Code and other code assistants with their own tooling, we recommend **trying the Clojure MCP tools independently first** to experience their full capabilities. Here's why:

**Potential Conflicts:**
- Both systems track file read/write state independently, which can cause confusion
- Overlapping tool functionality may lead to inconsistent behavior
- Mixed toolsets can dilute the optimized workflow experience

**Getting the Full Benefits:**
- Experience the curated Clojure development workflow as intended
- Understand how the tools complement each other
- Appreciate the Clojure-specific enhancements and safety features
- Develop familiarity with the integrated approach before mixing systems

Once you're comfortable with the Clojure MCP toolset, you can make informed decisions about whether to use it exclusively or integrate it with other code assistants and development tools based on your specific workflow needs.



## 📋 Installation

### Prerequisites

- [Clojure](https://clojure.org/guides/install_clojure) (1.11 or later)
- [Java](https://openjdk.org/) (JDK 11 or later)
- [Claude Desktop](https://claude.ai/download) (for the best experience)

### Setting up the ClojureMCP

Seting up ClojureMCP can be challenging as it is currently alpha and
not optimized for a quick install.

**Install Overview**

1. Setup and verify that you can start an nREPL on port `7888` in your project
2. Setup `clojure-mcp` in your `~/.clojure/deps.edn`
3. Configure `clojure-mcp` as an Mcp server in Claude Desktop or other MCP client

This setup is intended to verify that all the pieces are working
together. Specific configuration details like port numbers can be
changed later.

#### Step 1: Configure Your Target Project nREPL connection

In the Clojure project where you want AI assistance, ensure that we
can start nREPL server and ensure you can start it at specific
port `7888`.

This is what I do for my projects:

In my project's `deps.edn` I add an `:nrepl` alias:
```clojure
{
  ;; ... all the dependencies your project needs will need to be in here
  :aliases {
  ;; nREPL server for AI to connect to
  ;; include all the paths that you want available for developement
  :nrepl {:extra-paths ["test"] 
          :extra-deps {nrepl/nrepl {:mvn/version "1.3.1"}}
          :jvm-opts ["-Djdk.attach.allowAttachSelf"]						 
          :main-opts ["-m" "nrepl.cmdline" "--port" "7888"]}}}
```

**Verify** that this is working with the following command:

```bash
$ clojure -M:nrepl
```

You should see the nREPL start up at the port `7888`

**Leinigen**: If you are using Leinigen can start an nREPL server like so:

```bash
$ lein repl :headless :port 7889
```

#### Step 2: Add the  Clojure MCP server

Now we will setup `clojure-mcp` so that we can start it. The easiest
way to do that for now is to set it up as an alias in your
`~/.clojure/deps.edn` like so:

```clojure
{:aliases 
  {:mcp 
    {:deps {org.slf4j/slf4j-nop {:mvn/version "2.0.16"} ;; stdio server must have this
            com.bhauman/clojure-mcp {:git/url "https://github.com/bhauman/clojure-mcp.git"
                                     :git/tag "v0.1.1-alpha"
                                     :git/sha "0fcac09"}}
     :exec-fn clojure-mcp.main/start-mcp-server
     :exec-args {:port 7888}}}}
```

> **Finding the latest SHA**: Visit [https://github.com/bhauman/clojure-mcp/commits/main](https://github.com/bhauman/clojure-mcp/commits/main) to get the latest commit SHA, or clone the repo and run `git log --oneline -1` to see the latest commit.

**Verify**: It's very important that you verify that you can start `clojure-mcp` before moving on.

**BEFORE** starting `clojure-mcp` you **must** have an nREPL running and for this is example it must be running on port `7888`.

So in the root directory of your project start an nREPL server as demonstrated above:
```bash
$ clojure -M:nrepl
```
or
```bash
$ lein repl :headless :port 7888
```

**THEN** start `clojure-mcp`:

```bash
$ clojure -X:mcp :port 7888
```

You should see output like this:

```bash
MacBookPro:clojure-mcp bruce$ clojure -X:mcp
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/tools/list_changed"}
{"jsonrpc":"2.0","method":"notifications/resources/list_changed"}
{"jsonrpc":"2.0","method":"notifications/resources/list_changed"}
{"jsonrpc":"2.0","method":"notifications/resources/list_changed"}
{"jsonrpc":"2.0","method":"notifications/resources/list_changed"}
{"jsonrpc":"2.0","method":"notifications/resources/list_changed"}
{"jsonrpc":"2.0","method":"notifications/prompts/list_changed"}
{"jsonrpc":"2.0","method":"notifications/prompts/list_changed"}
{"jsonrpc":"2.0","method":"notifications/prompts/list_changed"}
{"jsonrpc":"2.0","method":"notifications/prompts/list_changed"}
```

It's import that you don't see any other output. This is a server that reads from standard in and outputs to standard out, so if there is any other output besides `json-rpc` this will cause problems. Extraneous output is normally caused by including `clojure-mcp` in a larger environment.  It's best restrict `clojure-mcp` to its own deps/classpaths.

If you get an error when you start 

```bash
MacBookPro:clojure-mcp bruce$ clojure -X:mcp
Execution error (ConnectException) at sun.nio.ch.Net/connect0 (Net.java:-2).
Connection refused

Full report at:
/var/folders/q2/zvxs36653cxf4v26c6262jwc0000gn/T/clojure-3031591891312608188.edn
```

This means that `clojure-mcp` could not connect to your nREPL server. 


> IMPORTANT NOTE: the mcp server can run in any directory and DOES NOT
> have to run from your project directory.  The mcp server looks to
> the nREPL connection for context.  The root directory of the project
> that is running the nREPL server becomes the root directory of all
> the mcp tool invocations. Currently the nREPL must run on the same
> machine as the MCP server as there is an assumption of a shared file
> system between the nREPL server and the MCP server.

> ANOTHER IMPORTANT NOTE: `clojure-mcp` should not run as part of your
> project and your project's dependencies should not mingle with
> clojure-mcp. It should run separately, with its own set of deps. So
> if you include it in your projects `deps.edn` it should not use
> `:extra-deps` in its alias is should always use `:deps`

#### Step 3: Configure Claude Desktop

Now that we've launched `clojure-mcp` from the commandline we need to
set it up so that it can be launched by the tools that are going to be
using it.

The common pattern for Mcp clients like Claude Desktop and others is
that they have a JSON configuration file where you give them a command
to start your Mcp server.

**This is the most challenging part of the setup.** We have to ensure
that applications launch environment has the right PATH and ENV
variables so that `clojure` can run.

Create a `~/Library/Application Support/Claude/claude_desktop_config.json` file with contents something like this:
```json
{
    "mcpServers": {
        "clojure-mcp": {
            "command": "/bin/sh",
            "args": [
                "-c",
                "export PATH=/opt/homebrew/bin:$PATH; exec clojure -X:mcp :port 7888"
            ]
        }
    }
}
```

So if you don't use nix and you use homebrew this might just work for
you. The key is that you will need to supply the PATH that will work.

Common 
  - **Homebrew (Apple Silicon)**: `/opt/homebrew/bin`
  - **Nix**: `/home/username/.nix-profile/bin` or `/nix/var/nix/profiles/default/bin`
  - **System default**: Often `/usr/bin:/usr/local/bin` works

#### Step 4: Test the Setup

1. **Start nREPL** in your target project:
   ```bash
   cd /path/to/your/project
   clojure -M:nrepl
   ```
   You should see: `nREPL server started on port 7888...`

2. **Start or Restart Claude Desktop** (required after config changes)

3. **Verify connection**: In Claude Desktop, click the `+` button in
   the chat area. You should see "Add from clojure-mcp" in the menu.

This is normally where you will start trouble shooting why Claude
Desktop won't run the `clojure` command.

I suggest you have a conversation with Gemini, Claude, ChatGpt about
how to best do this on your system.

#### Starting a new conversation

In Claude Desktop click the `+` tools and optionally add
 * resource `PROJECT_SUMMARY.md`  - (have the LLM create this) see below
 * resource `Clojure Project Info` - which introspects the nREPL connected project
 * resource `LLM_CODE_STYLE.md` - Which is your personal coding style instructions (copy the one in this repo)
 * prompt `clojure_repl_system_prompt` - instructions on how to code - cribbed a bunch from Clod Code

Then start the chat.

I would start by stating a problem and then chatting with the LLM to
interactively design a solution. You can ask Claude to "propose" a
solution to a problem.

Iterate on that a bit then have it either:

A. code and validate the idea in the REPL.

> Don't underestimate LLMs abilities to use the REPL! Current LLMs are
> absolutely fantastic at using the Clojure REPL. 

B. ask the LLM to make the changes to the source code and then have it validate the code in the REPL after file editing.

C. ask to run the tests.
D. ask to commit the changes.

> Make a branch and have the LLM commit often so that it doesn't ruin good work by going in a bad direction.

## Project Summary Management

This project includes a workflow for maintaining an LLM-friendly `PROJECT_SUMMARY.md` that helps assistants quickly understand the codebase structure.

### How It Works

1. **Creating the Summary**: To generate or update the PROJECT_SUMMARY.md file, use the MCP prompt in the `+` > `clojure-mcp` menu `create-project-summary`. This prompt will:
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

## Chat Session Summarization

The Clojure MCP server provides a pair of prompts that enable
conversation continuity across chat sessions using the `scratch_pad`
tool. This will be stored **in memory**. Things stored in the `scratch_pad`
are not persisted to disk (yet).

### How It Works

The system uses two complementary prompts:

1. **`chat-session-summarize`**: Creates a summary of the current conversation
   - Saves a detailed summary to the scratch pad
   - Captures what was done, what's being worked on, and what's next
   - Accepts an optional `chat_session_key` parameter (defaults to `"chat_session_summary"`)

2. **`chat-session-resume`**: Restores context from a previous conversation
   - Reads the PROJECT_SUMMARY.md file
   - Calls `clojure_inspect_project` for current project state
   - Retrieves the previous session summary from scratch pad
   - Provides a brief 8-line summary of where things left off
   - Accepts an optional `chat_session_key` parameter (defaults to `"chat_session_summary"`)

### Usage Workflow

**Ending a Session:**
1. At the end of a productive conversation, invoke the `chat-session-summarize` prompt
2. The assistant will store a comprehensive summary in the scratch pad
3. This summary persists across sessions thanks to the scratch pad's global state

**Starting a New Session:**
1. When continuing work, invoke the `chat-session-resume` prompt
2. The assistant will load all relevant context and provide a brief summary
3. You can then continue where you left off with full context

### Advanced Usage with Multiple Sessions

You can maintain multiple parallel conversation contexts by using custom keys:

```
# For feature development
chat-session-summarize with key "feature-auth-system"

# For bug fixing
chat-session-summarize with key "debug-memory-leak"

# Resume specific context
chat-session-resume with key "feature-auth-system"
```

This enables switching between different development contexts while maintaining the full state of each conversation thread.

### Benefits

- **Seamless Continuity**: Pick up exactly where you left off
- **Context Preservation**: Important details aren't lost between sessions
- **Multiple Contexts**: Work on different features/bugs in parallel
- **Reduced Repetition**: No need to re-explain what you're working on

The chat summarization feature complements the PROJECT_SUMMARY.md by capturing conversation-specific context and decisions that haven't yet been formalized into project documentation.

## LLM API Keys

> This is NOT required to use the Clojure MCP server.

> IMPORTANT: if you have the following API keys set in your
> environment, then ClojureMCP will make calls to them when you use
> the `dispatch_agent`,`architect` and `code_critique` tools. These
> calls will incur API charges.

There are a few MCP tools provided that are agents unto themselves and they need API keys to function.

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
        "clojure-mcp": {
            "command": "/bin/sh",
            "args": [
                "-c",
                "cd /path/to/your/mcp-server/home && PATH=/your/bin/path:$PATH && clojure -X:mcp"
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
        "clojure-mcp": {
            "command": "/bin/sh",
            "args": [
                "-c",
                "source ~/.api_credentials.sh && cd /path/to/your/mcp-server/home && PATH=/your/bin/path:$PATH && clojure -X:mcp"
            ]
        }
    }
}
```

> **Note**: The agent tools will work with any available API key. You don't need all three - just set up the ones you have access to. The tools will automatically select from available models. For now the ANTHROPIC API is limited to the displatch_agent.


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

#### Agent System (`dispatch_agent`)
- **Autonomous Search**: Handles complex, multi-step exploration tasks
- **Read-only Access**: Agents have read only tool access
- **Detailed Results**: Returns analysis and findings



## 🔧 Customization

ClojureMCP is designed to be highly customizable. During the alpha phase, creating your own custom MCP server is the primary way to configure the system for your specific needs.

You can customize:
- **Tools** - Choose which tools to include, create new ones with multimethods or simple maps
- **Prompts** - Add project-specific prompts for your workflows
- **Resources** - Expose your documentation, configuration, and project information
- **Tool Selection** - Create read-only servers, development servers, or specialized configurations

The customization approach is both easy and empowering - you're essentially building your own personalized AI development companion.

**📖 [Complete Customization Documentation](doc/README.md)**

For a quick start: **[Creating Your Own Custom MCP Server](doc/custom-mcp-server.md)** - This is where most users should begin. 

## ⚙️ Configuration

The Clojure MCP server supports minimal project-specific configuration
through a `.clojure-mcp/config.edn` file in your project's root
directory. This configuration provides security controls and
customization options for the MCP server.

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

**Prerequisites for Emacs Integration:**
- `emacsclient` must be available in your system PATH
- Emacs server must be running (start with `M-x server-start` or add `(server-start)` to your init file)
- The integration allows the MCP server to communicate with your Emacs editor for enhanced development workflows

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

## 📝 License

GNU Affero General Public License v3.0

Copyright (c) 2025 Bruce Hauman

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program. If not, see <https://www.gnu.org/licenses/>.

### License Summary

- ✅ **Use freely** for personal projects, internal business tools, and development
- ✅ **Modify and distribute** - improvements and forks are welcome  
- ✅ **Commercial use** - businesses can use this internally without restrictions
- ⚠️ **Network copyleft** - if you offer this as a service to others, you must open source your entire service stack
- 📤 **Share improvements** - modifications must be shared under the same license

This license ensures the project remains open source while preventing commercial exploitation without contribution back to the community.
