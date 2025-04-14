# REPL Driven Development

I'd like to develop Clojure code in a REPL Driven Development style I have given you access to a Clojure REPL throught the `clojure_eval` mcp tool.

The code will be functional code where functions take args and return results.  This will be preferred over side effects. But we can use side effects as a last resort to service the larger goal.

I'm am going to supply a problem statement and I'd like you to work through the problem with me iteratively step by step.

The iterative expression doesn't have to be a complete function it can a simple sub-expression.

Each step you evaluate an expression to verify that it does what you thing it will do.

If something isn't working feel free to use the other clojure tools available.

If a function isn't found you can search for it using the `symbol_search` tool and you can also the `symbol_completions` tool to help find what you are looking for.

If you are having a hard time with something you can also lookup documentation on a function using the `symbol_documentation` tool.

You can also lookup source code with the `source_code` tool to see how a certain function is implemented.

The main thing is to work step by step to incrementally develop a solution to a problem.  This will help me see the solution you are developing and allow me to guid it's development.

# Writing out code

You have some very effective tools for editing Clojure code.

* `clojure_file_outline` - important for getting the ordered overview of the state of the file
* `clojure_edit_replace_form`
* `clojure_edit_insert_before_form`
* `clojure_edit_insert_after_form`

These tools lint code Clojure and and format it cleanly into the target files.

USE `edit_file` and `write_file` AS A LAST RESORT when the above functions are not working.

Using the `clojure_edit` tools saves development time and tokens and it really makes me happy!!! Because I can finish my work sooner. Thank you!

# Before starting

Use `clojure_inspect_project` tool to get valuable information about the project

Like project directorys, dependencies, directory structure etc.

# The overall development workflow:

1. develop and validate a solution as a function or set of functions in the REPL with `clojure_eval`
  - create a namespace
  - change into the namesapce with `(in-ns ...)`
  - if you need to start over `(remove-ns ...)`
  - create definitions and validate them incrementally
2. write the solutions to the to correct files using the specialised `clojure_edit` tools
3. require reload the namespace, and test the various functions in the REPL to verify that    they are working correctly
4. commit the changes to git.

