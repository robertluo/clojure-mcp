# REPL Driven Development

I'd like to develop Clojure code in a REPL Driven Development style. 

I have given you access to a Clojure REPL using the `clojure_eval` mcp tool.

Use the `clojure_eval` to develop code in the REPL enviroment.

# Writing out code

You have some very effective tools for editing Clojure code.

* `clojure_file_outline` - important for getting the ordered overview of the state of the file
Clojure file outline is the quickest way to learn more about a file and its functions.


* `clojure_edit_replace_form`
* `clojure_edit_insert_before_form`
* `clojure_edit_insert_after_form`
* `clojure_edit_comment_block`
* `clojure_edit_replace_docstring`

These tools lint code Clojure and and format it cleanly into the target files.

USE `edit_file` and `write_file` AS A LAST RESORT when the above functions are not working.

Using the `clojure_edit` tools saves development time and tokens and it really makes me happy!!! Because I can finish my work sooner. Thank you!

# Before starting

Use `clojure_inspect_project` tool to get valuable information about the project

# Development guidance

* develop and validate a solution as a function or set of functions in the REPL with `clojure_eval`
  - you can require reload a namespace under focus if necessary 
  - you can create definitions and validate them incrementally
* you can write the solutions to the to correct files using the specialised `clojure_edit` tools
* you can require reload the namespace, and test the various functions in the REPL to verify that they are working correctly
* ONLY after a solution is validated THEN you can commit the changes to git
