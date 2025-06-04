# ClojureMCP Documentation

This directory contains documentation for creating MCP (Model Context Protocol) components using ClojureMCP.

## Documentation Files

### [Creating Tools Without ClojureMCP](creating-tools-without-clojuremcp.md)
Learn how to create tools as simple Clojure maps without depending on ClojureMCP's multimethod system. This approach allows you to create standalone tools that can be easily shared and integrated into any MCP server.

### [Creating Prompts](creating-prompts.md)
The standard guide for creating prompts in MCP. Prompts generate conversation contexts to help AI assistants understand specific tasks or workflows. This same approach works whether you're using ClojureMCP or creating standalone prompts.

### [Creating Resources](creating-resources.md)
The standard guide for creating resources in MCP. Resources provide read-only content like documentation, configuration files, or project information. This same approach works whether you're using ClojureMCP or creating standalone resources.

## Key Concepts

- **Tools**: Perform actions and computations
- **Prompts**: Generate conversation contexts for AI assistants
- **Resources**: Provide read-only content

## Quick Reference

| Component | Schema | Callback Signature |
|-----------|--------|-------------------|
| Tool | `{:name, :description, :schema, :tool-fn}` | `(callback result-vector error-boolean)` |
| Prompt | `{:name, :description, :arguments, :prompt-fn}` | `(callback {:description "...", :messages [...]})` |
| Resource | `{:url, :name, :description, :mime-type, :resource-fn}` | `(callback ["content..."])` |

## Notes

- **Tools** can be created either using ClojureMCP's multimethod system or as simple maps (see the tools documentation)
- **Prompts** and **Resources** are always created as simple maps, making them inherently portable
- All components can be tested independently without an MCP server
- String keys are used for all parameter maps passed to component functions
