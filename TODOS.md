# Today tasks


Use tool to develop things and tweak the dev style
Create prompt source
Use gemini 2.5 pro
Eval tool output

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
interrupt tool
source resource/tool
pprint-tool
eval-pprint-composed tool
load-file/tool
clojure-docs/tool web docs tool
context aware grep tool for clojure
list-namespaces and list the vars in a namespace
write-out-namespace-to-file?
create-new-project then jack into it
eval-history tool

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



 
 
