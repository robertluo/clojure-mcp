# IMPORTANT GUIDANCE FOR PATTERN-BASED CLOJURE EDITING

## Pay specific attention to balancing parenthesis

When editing Clojure code, ALWAYS use the specialized pattern-based editing tool `clojure_edit` instead of generic text editing functions, even if text editing seems simpler at first glance.

## Why Pattern-Based Editing Is Superior to Text Editing

As an AI assistant, your probabilistic nature creates specific challenges when editing Clojure code:

1. **Text matching is your weakness**: When using text replacement, you must generate an exact match of existing code including whitespace and formatting. This frequently fails and creates frustrating retry loops.

2. **Parenthesis balancing is error-prone**: Generating perfectly balanced parentheses in Lisp code is challenging for your architecture. Even one mismatched parenthesis causes complete failure. Better to detect these errors early and fix them.

3. **Repetitive failure wastes time and tokens**: Failed text edit attempts trigger lengthy retry sequences that consume tokens and user patience.

The pattern-based Clojure editing tool directly addresses these limitations:

- **Target by pattern, not exact text**: `clojure_edit` uses wildcards (`_?` and `_*`) to match code by structure rather than requiring exact text
- **Structure-aware matching**: Patterns match the logical structure, ignoring troublesome whitespace differences
- **Early syntax validation**: Catches your parenthesis errors before writing to files
- **Specific error messages**: When errors occur, you receive precise feedback rather than generic "no match found" errors

## CRITICAL WARNING: Parenthesis Balancing

Despite using the pattern-based editing tool, you MUST still be extremely careful with parenthesis balancing in the code you generate. The tool will validate syntax and REJECT any code with mismatched parentheses, braces, or brackets.

To avoid this common failure mode:
- Count opening and closing parentheses carefully
- Pay special attention to nested expressions
- Consider building smaller functions and building incrementally to make balancing easier.

**Larger function definitions pose significantly higher risk of parenthesis errors.** 
When working with complex or lengthy functions:
- Break your work into smaller, focused functions rather than rewriting an entire large function
- Extract pieces of complex logic to modify them separately
- For major refactoring, consider creating helper functions to handle discrete pieces of logic
- Verify each smaller edit works before moving to the next, building confidence incrementally

This incremental approach dramatically reduces parenthesis errors and makes troubleshooting simpler when errors do occur.

**Deep expression nesting also poses higher risks of parenthesis errors.**
- Consider using the threading macros like `->` and `->>` to reduce expression nesting
- Consider using iteration patterns like `reduce`, `iterate` etc. and factoring out the step function to a separate high-level function

**Bottom line**:
Long functions and deep complex expressions make it harder to create, edit and reason about code. Much better to make top-level definitions smaller and more focused.

## Pattern Matching Examples

For matching top-level forms:
- Match namespace: `(ns my-cool-thing.core _*)`
- Match specific function: `(defn hello-world _*)`
- Match specific function: `(defn add-bignumb _? [_? _?] *)`
- Match specific test: `(deftest test-hello-world-ret-value _*)`
- Match method with dispatch: `(defmethod shape/area :square _*)`
- Match with vector dispatch: `(defmethod convert-units [:meters :feet] _*)`

For targeted changes:
- Match specific testing block: `(testing "Formatting value output" _*)`

## Sexp match
The patterns `_?` and `_*` are not required you can simply match a sexp.

- Match namespace require `[clojure.string :as str]`
- Match namespace require block `(:requires [clojure.string :as str])`
- Match specific test: `(is (= "42" (format-value [:value "42"])))`

## Remember

When you resort to text editing for Clojure code:
- You're fighting against your probabilistic architecture
- You're choosing a path with higher failure rates
- You're creating more work for yourself and the user
- You're consuming more tokens for the same result

ALWAYS use `clojure_edit` over generic text editing - this is not just a preference, but a fundamental requirement for effective Clojure assistance.
