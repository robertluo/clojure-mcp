# Today tasks


Experiments 

* switching to propose and ideate solutions before evaluating
  FINDINGS
  - this works well, switching between repl iteration and improving the code
  
* always allow once on saving files and doing commits 
  FINDINGS
  - this is needed to interject into the 
  - very hard to remember to do this

* https://github.com/yamadashy/repomix


* starting without the repl driven prompt to see behavior
  FINDINGS
  - if you provide a directory it will ignore the REPL and artifacts and just start 
    creating complete files
  - even if you politely ask to do REPL driven development it starts baning away at files
    if you provide a directory.	
  
  - This minimal prompt was working a bit.	
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

* starting with Claude Project 
  - check on harmonizing artifacts with local filesystem

* work on an existing complicated code base

3. Clojure Project management tool
  - manages data about project
  - allows var definition replacement in files
  


Eval tool output

The problem is the creation of a library for rendering Clojure data of the form [:h1 {:class "active"} children ...] into HTML.

let's use s/fdef and perhaps create some empty clojure function stubs
-----

Create a prompt to switch over to file saving mode and to sync up with a project.

Test driven prompt.

# FOCUS

1. Discovering workflows
  * Must try working on an existing project in the REPL
2. modify suggested code.







# FLASH BANG DEMOS

Hook up ClojureScript or javascript eval.

Have slides created dynamically through the repl as you give the talk.

Having different languages being used concurrently is interesting.

Develop server and frontend through evaluation.




# Future tasks

look at using ciders tools
add think tool


# Notes to guid development of my Clojure MCP server

Look at not using a mono reactor but rather a streaming reactor to give streaming output results

Toggling printing on and off. To force AI to debug via evaluation

create java policy to sandbox the java environment
  - see if it can escape the environment?
  - scope to working directory
  
Meta features:
 - create/kill nREPL servers for various projects scope repls to chat sessions
 - Connect to nREPL via a port number 
   * at first the is only the connection function or a function to search for 
     REPL ports
   * after connection add the various tools to the MCP server
   * when you connect to another server 
     - kills the current nrepl connection
	 - removes all the functions from the server
	 - and re-adds functions for the current connection
  
  
prompt experiments
 - generate tests
 - experiment with displaying different things
 - instruct it to model the problem with spec
 - see if we can use instrument and property based testing
 - prevent using println as much as possible
 - include Clojure cheatsheet
 - RL few shot examples to explaining iterative functional approach
 - prompt should include instructions on how to obtain error stacktrace *e
 - and include instr on *1 *2 *3
 - test generation workflow


When encountering a problem have the AI backoff and evaluate problemenatic sub expressions in order to debug

Have the AI look up docs and souce code in order to debug whats going on as well.

implement MCP prompts interface
- first prompt is generic clojure REPL programming prompt

Should MCP allow a program to trigger a read?

eval-history resource/tool
- eval would record expressions

linting
nrepl-set-print depth
pprint-tool
eval-pprint-composed tool
context aware grep tool for clojure

long-running-eval-tool
interrupt tool

create-new-project then jack into it

write-out-namespace-to-file?
replace-function/tool
create-function/tool 

dynamic-dependencies addition



eval-tool todo:
  * create limited results by lazy-walking with a limit 
    - remember to use (into (empty [1 2 3]) %)
  * pprint option	
  * timeout eval then interrupt
  * can give more feedback via linting
  * need to test which output style is the most helpful
  
  

create clojure based tools that can be called from the repl
 - have a resource that lists clojure repl tools
 - perhaps have a resource path pushes changes
 - interesting to note that this allows the agent to create and install tools

get ClojureScript working via piggieback or nREPL
 
Thinking tool??  https://www.anthropic.com/engineering/claude-think-tool

Sequential Thinking tool?? 


## Notes 
 
fast.ai -- tool
how to solve it -- book used
https://github.com/phronmophobic/dewey 
