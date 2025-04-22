# LLM Code Style Preferences

## Clojure Style Guidelines

### Conditionals
- Use `if` for single condition checks, not `cond`
- Only use `cond` for multiple condition branches
- Prefer `if-let` and `when-let` for binding and testing a value in one step
- Consider `when` for conditionals with single result and no else branch

### Variable Binding
- Minimize code points by avoiding unnecessary `let` bindings
- Only use `let` when a value is used multiple times or when clarity demands it
- Inline values used only once rather than binding them to variables
- Use threading macros (`->`, `->>`) to eliminate intermediate bindings

### Parameters & Destructuring
- Use destructuring in function parameters when accessing multiple keys
- Example: `[{:keys [::zloc ::match-form] :as ctx}]` instead of separate `let` bindings

### Control Flow
- Track actual values instead of boolean flags where possible
- Use early returns with `when` rather than deeply nested conditionals
- Return `nil` for "not found" conditions rather than objects with boolean flags

### Comments
- Include only essential comments that explain non-obvious behavior
- Avoid comments that merely restate what the code does
- Document the "why" more than the "what"

### Nesting
- Minimize nesting levels by using proper control flow constructs
- Use threading macros (`->`, `->>`) for sequential operations

### Function Design
- Functions should generally do one thing
- Pure functions preferred over functions with side effects
- Return useful values that can be used by callers

### Testing Best Practices
- Always reload namespaces before running tests with `:reload` flag: `(require '[namespace] :reload)`
- Create test fixtures with accurate representation of the runtime environment
- When testing functions that use zippers, always create zippers with `{:track-position? true}` when position information is needed
- Test both normal execution paths and error conditions
- Prefer direct zipper creation in tests over helper functions to ensure visibility of all options
