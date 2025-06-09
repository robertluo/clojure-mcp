# Clojure MCP FAQ

## Why do I keep seeing messages like "File has been modified since last read"?

**Q: Why do I get this error when the AI tries to edit a file?**

```
File has been modified since last read: /Users/myname/myproject/src/something.clj 
Please read the WHOLE file again with collapse: false before editing.
```

**A:** This is expected behavior. Clojure MCP implements a file safety system similar to Claude Code that protects against conflicting edits.

### The File Timestamp Tracking System

The system tracks when files are read and modified to ensure AI assistants don't accidentally overwrite external changes. This prevents situations where:
- Another developer modifies a file while you're working
- Your editor auto-saves changes
- Git operations update files
- Build tools modify generated files

All editing tools automatically update the internal timestamp
after a successful edit:
- `clojure_edit` - Structure-aware Clojure form editing
- `clojure_edit_replace_sexp` - S-expression replacement
- `file_edit` - Text-based file editing
- `file_write` - File creation/overwriting

This means the AI can continue making edits to a file after using any of these tools without needing to read it again.

### Why Collapsed Reads Don't Count

When the AI uses `read_file` with `collapsed: true` (the default for Clojure files), it's only seeing a partial view of the file:
- Only function signatures are shown
- Function bodies are hidden unless they match search patterns
- Comments may be excluded

Since collapsed reads don't show the complete file content, they don't update the file's "last read" timestamp. This is intentional - the system requires the AI to see the **entire current state** of a file before allowing edits.

When you see this error message, it simply means the AI is being prompted to read the full file before making changes. This safety mechanism ensures that edits are always based on the current file state, preventing accidental loss of work and maintaining consistency across your development workflow.

## Why are there collapsed reads?

**Q: Why does the AI sometimes read files in "collapsed" mode?**

**A:** Collapsed reads are a powerful feature designed to help AI assistants work more efficiently with Clojure codebases.

### Benefits of Collapsed Reads

**1. Efficient Code Navigation**
- Large Clojure files can contain thousands of lines
- Collapsed view shows only function signatures, giving a quick overview of the file's structure
- The AI can rapidly understand what functions, definitions, and forms exist in a file

**2. Pattern-Based Exploration**
- The AI can search for specific functions using `name_pattern` (e.g., "validate.*" to find all validation functions)
- It can find code containing specific logic using `content_pattern` (e.g., "try|catch" to find error handling)
- Only matching functions are expanded, keeping the focus on relevant code

**3. Token Efficiency**
- Showing only what's needed reduces the amount of text the AI needs to process
- This allows the AI to explore more files and make connections across a codebase
- Particularly important when working with large projects

**4. Iterative Discovery**
- The AI can start with a collapsed read to understand structure
- Then use patterns to drill down into specific areas of interest
- Finally, read the full file only when edits are needed

### Example Workflow

You might see the AI:
1. Use a collapsed read to see all functions in a namespace
2. Use `name_pattern: "handle-.*"` to find all handler functions
3. Use `content_pattern: "database|db"` to find database-related code
4. Read with `collapsed: false` when it needs to edit something

This approach mirrors how human developers navigate code - we don't read every line of every file, but instead scan for structure and zoom in on what's relevant to our current task.
