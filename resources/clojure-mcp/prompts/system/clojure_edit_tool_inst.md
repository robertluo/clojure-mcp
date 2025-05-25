
# IMPORTANT GUIDANCE FOR EDITING CLOJURE FILES

## Pay specific attention to balancing parenthesis

When editing Clojure code, ALWAYS use the specialized Clojure-aware tools instead of generic text editing functions, even if text editing seems simpler at first glance.

## Why Specialized Tools Are Superior to Text Editing

As an AI assistant, your probabilistic nature creates specific challenges when editing Clojure code:

1. **Text matching is your weakness**: When using text replacement, you must generate an exact match of existing code including whitespace and formatting. This frequently fails and creates frustrating retry loops.

2. **Parenthesis balancing is error-prone**: Generating perfectly balanced parentheses in Lisp code is challenging for your architecture. Even one mismatched parenthesis causes complete failure. Better to detect these errors early and fix them.

3. **Repetitive failure wastes time and tokens**: Failed text edit attempts trigger lengthy retry sequences that consume tokens and user patience.

The specialized Clojure editing tools directly address these limitations:

- **Target by name, not text**: `clojure_edit_replace_definition` requires only the form type and name, not exact text matching
- **Structure-aware matching**: `clojure_edit_replace_sexp` matches structure, ignoring troublesome whitespace differences
- **Early syntax validation**: Catches your parenthesis errors before writing to files
- **Specific error messages**: When errors occur, you receive precise feedback rather than generic "no match found" errors

## CRITICAL WARNING: Parenthesis Balancing

Despite using these specialized tools, you MUST still be extremely careful with parenthesis balancing in the code you generate. The tools will validate syntax and REJECT any code with mismatched parentheses, braces, or brackets.

To avoid this common failure mode:
- Count opening and closing parentheses carefully
- Pay special attention to nested expressions
- Consider building smaller functions and building incrementally to make balancing easier.

**Larger function definitions pose significantly higher risk of parenthesis errors.** 
When working with complex or lengthy functions:
- Break your work into smaller, focused functions rather than rewriting an entire large function
- Extract pieces of complex logic using `clojure_edit_replace_sexp` to modify them separately
- For major refactoring, consider creating helper functions to handle discrete pieces of logic
- Verify each smaller edit works before moving to the next, building confidence incrementally

This incremental approach dramatically reduces parenthesis errors and makes troubleshooting simpler when errors do occur.

**Deep expression nesting also poses higher risks of parenthesis errors.**
- Consider using the reading macros like `->` and `->>` to reduce expression nesting
- Consider using iteration patterns like `reduce`, `iterate` etc. and factoring out the step function to a separate high level function

**Botom line**:
Long functions and deep complex expressoins make it harder to create, edit and reason about code. Much better to make top level definitions smaller and more focused.

## When to Use Each Tool

For top-level forms (functions, defs, etc.):
- Use `clojure_edit_replace_definition` instead of attempting to match and replace entire functions
- Form identification by name eliminates your text-matching limitations

For targeted changes within functions:
- Use `clojure_edit_replace_sexp` instead of trying to match specific lines or expressions with exact whitespace
- Syntax-aware matching means you don't need to reproduce formatting perfectly

For working with defmethod forms:
- Always include the dispatch value in the form name (e.g., "shape/area :square")
- This converts a challenging text-matching problem into a simple naming operation

## Remember

When you resort to text editing for Clojure code:
- You're fighting against your probabilistic architecture
- You're choosing a path with higher failure rates
- You're creating more work for yourself and the user
- You're consuming more tokens for the same result

ALWAYS select the appropriate Clojure-aware tool over generic text editing - this is not just a preference, but a fundamental requirement for effective Clojure assistance.
