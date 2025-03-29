# Today tasks

Eval tool output

Need to figure out the Spec development prompt
- problems, not using fdef and instrument enough
  * should use instrument after every redefinition of the function
  * should fdef after stubbing out a function

Prompt used::
-----
Let's use the connected repl to model a problem with Clojure Spec
The problem is the creation of a library for rendering Clojure data of the form [:h1 {:class "active"} children ...] into HTML.
Let's start with spec and iteratively evaluating it in the REPL

let's use s/fdef and perhaps create some empty clojure function stubs
-----

Create a prompt to switch over to file saving mode and to sync up with a project.

Test driven prompt.





# Future tasks

look at using ciders tools


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

nrepl-set-print depth
pprint-tool
eval-pprint-composed tool
list-namespaces and list the vars in a namespace
eval-history tool
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



 
 
