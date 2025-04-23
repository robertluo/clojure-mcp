# LLM Code Style Preferences

## Clojure Style Guidelines

### Conditionals
- Use `if` for single condition checks, not `cond`
- Only use `cond` for multiple condition branches
- Prefer `if-let` and `when-let` for binding and testing a value in one step
- Consider `when` for conditionals with single result and no else branch
- consider `cond->`, and `cond->>`

### Variable Binding
- Minimize code points by avoiding unnecessary `let` bindings
- Only use `let` when a value is used multiple times or when clarity demands it
- Inline values used only once rather than binding them to variables
- Use threading macros (`->`, `->>`) to eliminate intermediate bindings

### Parameters & Destructuring
- Use destructuring in function parameters when accessing multiple keys
- Example: `[{:keys [::zloc ::match-form] :as ctx}]` for namespaced keys instead of separate `let` bindings
- Example: `[{:keys [zloc match-form] :as ctx}]` for regular keywords

### Control Flow
- Track actual values instead of boolean flags where possible
- Use early returns with `when` rather than deeply nested conditionals
- Return `nil` for "not found" conditions rather than objects with boolean flags

### Comments
- Include only essential comments that explain non-obvious behavior
- Avoid comments that merely restate what the code does
- Document the "why" more than the "what"
- skip docstrings during development they are very expensive bc of the extra tokens
- remove long docstrings during development, they can always be added later
- when docstrings are created they should be concise and to the point, allow the arglists to document the fn
- defer docstring addition until the user prompts for it


### Nesting
- Minimize nesting levels by using proper control flow constructs
- Use threading macros (`->`, `->>`) for sequential operations

### Function Design
- Functions should generally do one thing
- Pure functions preferred over functions with side effects
- Return useful values that can be used by callers
- smaller functions make edits faster and reduce the number of tokens
- reducing tokens makes me happy


### Testing Best Practices
- Always reload namespaces before running tests with `:reload` flag: `(require '[namespace] :reload)`
- Create test fixtures with accurate representation of the runtime environment
- When testing functions that use zippers, always create zippers with `{:track-position? true}` when position information is needed
- Test both normal execution paths and error conditions
- Prefer direct zipper creation in tests over helper functions to ensure visibility of all options

### Using Shell Commands
- Prefer the idiomatic `clojure.java.shell/sh` for executing shell commands
- Always handle potential errors from shell command execution
- Use explicit working directory for relative paths: `(shell/sh "cmd" :dir "/path")`
- For testing builds and tasks, run `clojure -X:test` instead of running tests piecemeal
- When capturing shell output, remember it may be truncated for very large outputs
- Consider using shell commands for tasks that have mature CLI tools like diffing or git operations

### Output and Debugging
- Avoid using `println`, `prn`, or any printing functions in production code as this is a server operating on stdin/stdout
- For debugging, wrap printing statements with a comment macro `#_` that can be easily toggled

### Using Form Editing Tools

- **Form Type Selection**:
  - Use correct form types for editing: "defn", "def", "ns", "defmacro", "defmethod"
  - Avoid using "comment" as a form type - use `clojure_edit_replace_comment_block` for comment editing
  - Be specific with defmethod forms by including dispatch values when targeting specific implementations: "my-multi :default"

- **s-expression Replacement**:
  - When using `clojure_edit_replace_sexp`, ensure match_form contains only a single s-expression
  - Complex form refactoring should be broken down into multiple focused s-expression replacements
  - Use whitespace_sensitive=true when precise formatting matters

- **Error Handling**:
  - Always verify file and form existence before attempting edits
  - Handle editing failures gracefully by checking the error state of tool responses
  - Prefer to evaluate a form after editing to verify it's syntactically correct

### REPL-Driven Development

- **Incremental Development**:
  - Build solutions step-by-step, evaluating code in small units
  - Start with small examples, then expand and generalize
  - Test boundary conditions early with the REPL before committing to files

- **Verification Workflow**:
  1. Develop and test code in REPL first
  2. Save working code to file once verified
  3. Re-evaluate the saved code to ensure it works in its new context
  4. Only then proceed to the next development step

- **Dependency Management**:
  - Verify available libraries with `clojure_inspect_project` before assuming imports
  - Check for aliases and preferred namespace conventions in existing code
  - When adding new dependencies, ensure they are added to deps.edn

- **Context Maintenance**:
  - Use `clojure_eval` with `:reload` to ensure you're working with the latest code
  - Maintain awareness of the current namespace with `current_namespace`
  - Keep function and namespace references fully qualified when crossing namespace boundaries

