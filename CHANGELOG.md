# Changelog

## [v0.1.4-alpha] - 2025-06-11

### Added
- **Ripgrep (rg) support** in grep tool with intelligent fallback hierarchy (rg > grep > Java)
- **Smart path operations** in scratch_pad tool for automatic string-to-number conversion and data structure initialization
- Tool availability caching to avoid repeated shell command checks
- Cross-platform tool detection using `--version` instead of `which`

### Changed
- **Tool rename**: `fs_grep` → `grep` for better consistency
- Enhanced scratch_pad with smart path munging for vector indices
- Improved error handling for streaming operations with null parameters

### Fixed
- Streaming errors when receiving null parameters in MCP operations
- Schema validation errors in tool operations

### Internal
- General code linting and cleanup across multiple files
- Improved documentation and README content

## [v0.1.3-alpha] - 2025-06-09

### Added
- **Configurable file timestamp tracking** via `:write-file-guard` option (`:full-read`, `:partial-read`, or `false`)
- File existence validation in `unified_read_file` tool
- FAQ.md documenting file timestamp tracking behavior
- Docker support (`:dkr-nrepl` alias)
- `plan-and-execute` prompt for structured planning workflows
- Test coverage for new features

### Changed
- Enhanced scratch_pad prompt to encourage planning usage
- Clojure files always read as text format
- nREPL aliases now bind to 0.0.0.0 for network access
- Cleaned up unused prompt definitions

### Documentation
- FAQ with table of contents explaining timestamp tracking
- Updated system prompts and README with Wiki link

### Internal
- Added configuration helper functions (`get-write-file-guard`, `write-guard?`)
- File validation utilities (`path-exists?`)