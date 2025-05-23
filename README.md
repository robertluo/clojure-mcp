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

### Code Evaluation and Exploration

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_eval` | Evaluates Clojure code | Evaluating `(+ 1 2)` returns `=> 3` |

### File Editing Tools

| Tool Name | Description | Example Usage |
|-----------|-------------|---------------|
| `clojure_edit` | Replaces a form | Updating a function definition |
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
