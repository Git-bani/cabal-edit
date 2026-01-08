# cabal-edit

A command-line utility for managing Cabal dependencies, inspired by cargo-edit.

## Installation

```bash
cabal install cabal-edit
```

## System Requirements

`cabal-edit` requires `openssl` and `zlib` to be installed on your system for network communication with Hackage.

### Linux (Ubuntu/Debian)
```bash
sudo apt-get install -y libssl-dev zlib1g-dev pkg-config
```

### macOS
```bash
brew install openssl zlib
```

## Quick Start

### Adding Dependencies

```bash
# Add a dependency (resolves latest version from Hackage)
cabal-edit add text

# Add with explicit version constraint
cabal-edit add aeson --version "^>= 2.0"

# Interactive Search: search Hackage and select packages
cabal-edit add -i json

# Rename/Alias a dependency (Mixins)
# Adds 'aeson' but exposes it as 'JSON' via module renaming
cabal-edit add aeson --mixin "(Data.Aeson as JSON)"

# Hide modules from a dependency
cabal-edit add base --mixin "hiding (Prelude)"

# Add to a specific section (library, executable, test-suite, benchmark)
cabal-edit add hspec --section test-suite --dev

# Add to a conditional block
cabal-edit add Win32 --if "os(windows)"

# Shorthand for flag-conditional dependencies
cabal-edit add lens --flag use-lens

# Dry run (see changes with colorized diffs)
cabal-edit add lens --dry-run
```

### Removing Dependencies

```bash
# Remove a dependency
cabal-edit rm old-package

# Interactive mode (visually select dependencies to remove)
cabal-edit rm -i

# Remove from a specific section
cabal-edit rm old-package --section executable:my-exe
```

### Listing Dependencies

```bash
# List all dependencies in the project
cabal-edit list
```

### Upgrading Dependencies

```bash
# Upgrade all dependencies to latest versions
cabal-edit upgrade

# Interactive mode (select which packages to upgrade)
cabal-edit upgrade -i

# Dry run (see colorized diff of proposed changes)
cabal-edit upgrade --dry-run

# Upgrade a specific package
cabal-edit upgrade aeson
```

### Version Management

```bash
# Set the project version
cabal-edit set-version 1.2.3.0
```

### Flag Management

```bash
# Open interactive flag dashboard (toggle flags visually)
cabal-edit flag -i

# Add a new flag
cabal-edit flag add my-feature

# Enable/Disable existing flags
cabal-edit flag enable my-feature
cabal-edit flag disable my-feature

# Remove a flag
cabal-edit flag remove my-feature
```

### Output Control

```bash
# Suppress all output (useful for scripts)
cabal-edit add aeson --quiet

# Enable verbose logging (shows debug info)
cabal-edit add aeson --verbose

# Show version information
cabal-edit --version
```

### Workspace Support

`cabal-edit` supports multi-package workspaces defined in `cabal.project`.

```bash
# Apply command to all packages in the workspace
cabal-edit -w add lens

# Upgrade everything in the workspace
cabal-edit -w upgrade

# Target specific packages in the workspace
cabal-edit -p my-pkg1 -p my-pkg2 upgrade
```

## How it Works

`cabal-edit` differentiates itself from other tools by its **lossless editing architecture**:

1.  **Parse**: The tool parses your `.cabal` file into a high-fidelity AST (Abstract Syntax Tree). Unlike standard parsers, ours captures "junk" data like comments, trailing whitespace, and specific line endings (LF vs CRLF).
2.  **Edit**: Modifying operations (like adding a dependency) are performed directly on the AST nodes.
3.  **Serialize**: The modified AST is written back to text. Because the AST is lossless, any part of the file not explicitly touched by your command remains identical down to the last byte.

This ensures that `cabal-edit` is safe to use on any codebase, no matter how "artisanal" the formatting is.

## Performance & Safety

`cabal-edit` is designed for speed and reliability in production environments.

### Benchmarks
Powered by a highly optimized AST engine, `cabal-edit` performs complex operations in microseconds:

| Operation | Time | Description |
|-----------|------|-------------|
| **Parse** | ~11.5 µs | High-fidelity AST parsing (simple files) |
| **Add** | ~9.9 µs | Inserting a dependency into the AST |
| **Remove** | ~4.1 µs | Surgically removing a dependency |
| **Update** | ~2.5 µs | Modifying a version constraint or field |

### Scalability
`cabal-edit` has been stress-tested on massive Cabal files (5,000+ dependencies).
- **Processing Time**: ~27ms for a full parse-edit-write cycle on a 5,000-dependency file.
- **Complexity**: Linear $O(N)$ scaling.

## Research & Evaluation Benchmarks

Beyond core performance, `cabal-edit` supports specialized benchmarks for evaluating deep research and LLM-based dependency management tasks.

| Benchmark | Focus | Target Metrics |
|-----------|-------|----------------|
| **SimpleQA** | Fact-based dependency queries | Accuracy, Hallucination rate |
| **BrowseComp** | Multi-package workspace analysis | Synthesis quality, Speed |

These benchmarks utilize Gemini Flash via OpenRouter to evaluate the tool's effectiveness in automated environments.

### Safety Guarantees
- **Total Functions**: The core logic is implemented using 100% total functions, eliminating runtime crashes due to partiality (no `head`, `tail`, `undefined`).
- **Exception-Free**: All IO operations and file handling are wrapped in the `Result` ADT. The library does not throw runtime exceptions, ensuring stability for callers.
- **Type Safety**: Strong internal types and smart constructors (`PackageName`, `VersionConstraint`) prevent invalid states from ever being represented.
- **Verification**: Every build passes **115+ property-based tests** (via `Hedgehog` & `QuickCheck`) ensuring semantic identity across Add/Remove cycles.

## Supported GHC Versions

`cabal-edit` is compatible with **GHC 8.10.1** and newer. It is officially tested on:

- GHC 9.6
- GHC 9.8
- GHC 9.10
- GHC 9.12

## Features

- ✅ **Industrial-Grade Core**: Powered by a lossless AST engine that captures every detail of your source file.
- ✅ **Surgical Editing**: Guaranteed byte-for-byte fidelity for all unmodified parts. Preserves all comments, indentation, and structure.
- ✅ **Smart Add**: Resolves latest versions from Hackage automatically (with offline fallback).
- ✅ **Interactive Search**: Search Hackage and select packages to add from a TUI list.
- ✅ **Renaming Support**: Add dependencies with aliases (Mixins) using `--mixin` (e.g. `hiding (Prelude)`).
- ✅ **Bulk Upgrade**: Upgrade dependencies with intelligent version resolution.
- ✅ **Flag Dashboard**: Visual TUI to toggle Cabal flags interactively.
- ✅ **Interactive Removal**: Checklist-style selection for removing multiple packages.
- ✅ **Colorized Diffs**: `git diff`-style visual previews for all dry-run operations.
- ✅ **Version Management**: Set project version from the CLI.
- ✅ **Hpack Support**: Automatically detects and edits `package.yaml` files when present.
- ✅ **Workspace Support**: Full support for `cabal.project` and multi-package setups.
- ✅ **Advanced Safety**: 
    - **Atomic Writes**: Uses temporary files and atomic moves to prevent file corruption.
    - **In-memory Verification**: Validates Cabal file syntax before committing changes.
    - **TTY Detection**: Clean output in non-terminal environments (no junk characters).

## Development

```bash
# Build
cabal build

# Test
cabal test
```

## License

MIT
