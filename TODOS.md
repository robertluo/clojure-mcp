## Looking forward

* integrate clj-format to format output
* edit tools return diff with context - perhaps provide dry-run
* read-summary-prompt looks up summary given a phrase
* summary chain tool takes a keyword/phrase and creates a work summary file
  - probably uses separate langchain for this.
  - until then we will query the previous summary and then combine it with the current summary.
* replace symbol tool - probably could just use a clj-rewrite walk tool -- list of files to work on
* replace-string with linting the final result
* read-form tool takes a file path and reads the single form with type and name
* fix editing comments with clj-rewrite

* implement the filsystem edit functions so that we can demphasis them and lint the output



* gptel mcp integration
* start with design session - see below
* Clojure implemented tools repl tools not exposed as MCP tools only through eval
* clojurescript/javascript piggieback
* think tool

* gptel mcp integration findings
  - MCP client connection needs work
    * has a buffer limit thats preventing loading all the
      tools, list tools seems to be cut off at 4096 bytes
	  this is remedied when I only add a coupld of tools
	* chack how the JavaAddTool works
  - gptel is working fairly well
    - must require permission to act and display the output.
    - when requiring permission for eval C-c C-c is a bit awkward
	- it would be nice it the code was displayed well
	- it would be nice if the eval part of the chat was editable
	- 
  




## general clojure MCP todos
* remove uneeded namespace
* refactor interface code that can be reused into an mcp namespace
  - consume this from specific server implementations

## Updated Notes: AI-Assisted Clojure, REPL Workflows & MCP Server

Overall Goals:

1. Develop/Refine an MCP (Machine Coding Partner) server concept for Clojure.
2. Discover and optimize REPL-oriented workflows for Clojure development assisted by AI.

### I. Experiments & Findings

**A. AI Model Exploration (Claude vs. GPT/Other)**

* **Claude:**
  * Capabilities: Impressively capable.
  * Cost: Very expensive.
  * REPL Integration:
    * Possible to integrate, but potentially harder to align with overarching prompts compared to a dedicated MCP.
    * Can validate code via REPL and perform REPL-driven tasks, but requires specific prompting and effort.
    * Finding: Similar to other AI, needs explicit instruction to show code *before* evaluation.

* **gptel Integration:**
  * Potential: Looks promising as a way to test and evaluate AI-assisted workflows.

* **General AI Code Quality (Clojure vs. JavaScript):**
  * Finding: AI-generated Clojure code quality seems comparable to AI-generated JavaScript.
  * Finding: Code often works but may lack maintainability (similar issues across languages).

* **Claude Desktop Integration:**
  * Finding: Excellent
  * often doesn't display evaluations
  * can really go on like a beast

  
**B. REPL Integration & Workflow Strategies**

* **Starting Point - No REPL Prompt:**
  * Finding: If a project directory is provided without explicit REPL instructions, the AI tends to ignore the REPL and starts creating/modifying complete files directly, even if asked politely to use the REPL.

* **Starting Point - Minimal REPL Prompt:**
  * Finding: A minimal prompt focusing on REPL-driven exploration, artifact creation, and iterative function development worked reasonably well.
    * *Example Prompt Used:*
      ```
      let's start working on Clojure library to render Clojure data like [:h1 {:class "active"} child ...] to an HTML AST
      let's explore things first via Clojure REPL driven development

      First let's make an artifact to track our work
      change into that namespace in the REPL

      This is the plan:
      * draft a simple small example function
      * Present the function in a code block
      * I will offer feedback
      * update the function with suggested changes

      Before you evaluate code, present it in a code block
      ```
	  
* **Starting Point - design session

Use a conversational LLM to hone in on an idea (I use ChatGPT 4o / o3 for this):
```
Ask me one question at a time so we can develop a thorough, step-by-step spec for this idea. Each question should build on my previous answers, and our end goal is to have a detailed specification I can hand off to a developer. Let’s do this iteratively and dig into every relevant detail. Remember, only one question at a time.

Here’s the idea:
```
<IDEA>

At the end of the brainstorm (it will come to a natural conclusion):
```
Now that we’ve wrapped up the brainstorming process, can you compile our findings into a comprehensive, developer-ready specification? Include all relevant requirements, architecture choices, data handling details, error handling strategies, and a testing plan so a developer can immediately begin implementation.
```

* **Propose/Ideate Before Evaluating:**
  * Finding: This approach works well. Switching between REPL iteration for testing/exploration and high-level discussion for improving the code design is effective.

* **Fleshing Out Code First:**
  * Finding: Providing some initial code or structure *before* engaging the AI works very well. This seems to capture intention more effectively than detailed prompts alone.

* **Working on Existing/Complex Codebases:**
  * Finding: The interactive, REPL-focused method *can* work for existing code.
  * Challenge: I/O operations are difficult, especially working with large files or performing large edits, which can be slow and cumbersome.

* **ClojureScript Evaluation:**
  * Experimented with ClojureScript eval (details/findings not specified in notes).

* **Test-Driven Prompting:**
  * Considered/planned testing a prompt focused on test-driven development (TDD).
  * Using `s/fdef` and creating empty function stubs was suggested as part of this.

**C. Prompt Engineering & Interaction Patterns**

* **Interactive Discovery:**
  * Experimented with interactively discovering prompts for Clojure code simplification/improvement.

* **Display Before Eval:**
  * Finding: Consistently necessary to instruct the AI to display code *before* evaluating it in the REPL.

* **Saving/Committing - "Allow Once":**
  * Finding: A mechanism to always require confirmation ("allow once") before saving files or making commits is needed for user control.
  * Challenge: Remembering to use this interjection point is difficult.

**D. Project & File Management**

* **Claude Project Artifacts:**
  * Initial Finding: Harmonizing AI-managed artifacts (like those in a Claude Project) with the local filesystem seems feasible ("OK").

* **Clojure Project Management Tool Concept:**
  * Idea for a tool to manage project metadata.
  * Capability: Allow replacement of `def`s (variable/function definitions) within project files.

* **`Tool Exploration (repomix):`**
  * Evaluated `repomix` (<https://github.com/yamadashy/repomix>).
  * Finding: Not particularly useful ("meh").

### II. Key Findings & Workflow Principles for AI-Assisted Clojure

* Start with Some Code: Providing initial code structure often works better than detailed prompts alone.
* Explicit REPL Guidance: AI needs clear instructions to prioritize REPL interaction, especially if a project context/directory is provided.
* Show, Then Run: Always require the AI to present code before evaluation.
* Iterative Refinement: Cycle between REPL exploration/testing and higher-level discussion/design improvement.
* Control File Operations: Implement checks (like "allow once") before the AI modifies the filesystem or commits code.
* Namespace/File Naming: AI needs to remember Clojure's convention: `my-namespace` maps to `my_namespace.clj`.
* REPL State Management: AI must remember to `require` namespaces (often with `:reload`) and switch (`in-ns`) to the correct namespace before working on its definitions.
* Large Files are Hard: Interacting with large existing files via AI is currently challenging.

### III. Future Work & Focus Areas

1. Discovering Workflows:
   * Priority: Gain more experience working *within the REPL* on an *existing* project using AI assistance.
2. Improving Interaction:
   * Develop better ways to modify AI-suggested code interactively.
3. Prompt Development:
   * Create a specific prompt for switching the AI into a "file saving mode" and syncing project state.
   * Develop and test prompts for Test-Driven Development (TDD).
4. Tooling:
   * Further investigate/develop the "Clojure Project management tool" concept.

### IV. Notes for Clojure MCP Server Development

**A. Architecture & Sandboxing**

* Consider a streaming reactor (vs. mono reactor) for streaming output results.
* Implement Java policy for sandboxing the execution environment.
  * Test if the AI can escape the sandbox.
  * Scope permissions to the working directory.

**B. Meta Features & nREPL Management**

* Create/kill nREPL servers scoped to projects/chat sessions.
* Connect to existing nREPL servers via port number.
  * Initial state: Only connection function or port search function available.
  * On connection: Add relevant tools/functions to the MCP server context.
  * On disconnect/reconnect: Kill old connection, remove old functions, add functions for the new connection.

**C. Prompt Engineering & Experiments**

* Generate tests.
* Experiment with displaying different kinds of information.
* Instruct AI to model problems with `clojure.spec`.
* Explore using `clojure.spec/instrument` and property-based testing.
* Discourage `println` for debugging; encourage REPL evaluation.
* Include a Clojure cheatsheet in the context/prompt.
* Use RL / few-shot examples to guide towards an iterative functional approach.
* Include instructions on obtaining error stack traces (`*e`) and history (`*1`, `*2`, `*3`).
* Develop a specific workflow for test generation.

**D. Debugging Strategies**

* When encountering errors, have the AI "back off" and evaluate problematic sub-expressions.

**F. Tool Ideas & Development**

* Toggling printing on/off (to force debugging via evaluation rather than printing).
* `test running tool`
* `nrepl-set-print-bytes-quota` tool.
* Context-aware `grep` tool for Clojure code.
* `long-running-eval-tool` (handles potentially long computations).
* `interrupt-tool` (to stop long-running evaluations).
* `create-new-project` tool (then connect/jack-in).
* `write-out-namespace-to-file?` tool/confirmation step.
* `create-function` tool.
* Dynamic dependency addition tool.
* Clojure-based tools callable *from* the REPL:
  * Resource/tool to list available Clojure REPL tools.
    - or list them in the eval description
  * Mechanism for pushing changes (e.g., via resource path).
  * Allows the agent to potentially create and install its own tools.

**`G. eval-tool Specific Todos`**

* Implement timeout for evaluation, followed by interrupt.

**H. ClojureScript Integration**

* Get ClojureScript evaluation working (via piggieback or direct nREPL support).

**I. Thinking Tools**

* Explore Anthropic's "Think Tool" concept: <https://www.anthropic.com/engineering/claude-think-tool>
* Consider a "Sequential Thinking" tool.

### V. Demo Ideas ("Flash Bang Demos")

* Hook up ClojureScript or JavaScript evaluation.
* Dynamically create presentation slides via REPL commands during a talk.
* Showcase concurrent use of multiple languages (e.g., Clojure + JS).
* Develop a server and frontend concurrently through REPL evaluation.

### VI. Future Tasks

* Look at using CIDER's tools/integration points.
* Add/Investigate "think tool".

### VII. Resources & Inspirations

* fast.ai (Potential tool/library?)
* "How to Solve It" by George Pólya (Book on problem-solving methodology).
* Dewey - Data Explorer/Browser: <https://github.com/phronmophobic/dewey>

### VIII. Technical Notes & Reminders

* Naming: Remember dash-case (`-`) for namespaces, underscore-case (`_`) for corresponding filenames.
* REPL Setup: Remember `(require 'my-namespace :reload)` and `(in-ns 'my-namespace)`.
* TDD Ideas: Use `clojure.spec.alpha/fdef` and potentially create empty function stubs first.


