# cabal-edit

A command-line utility for managing Cabal dependencies, inspired by cargo-edit.

## Installation

```bash
cabal install cabal-edit
```

## Quick Start

### Adding Dependencies

```bash
# Add a dependency (resolves latest version from Hackage)
cabal-edit add text

# Add with explicit version constraint
cabal-edit add aeson --version "^>= 2.0"

# Add to a specific section (library, executable, test-suite, benchmark)
cabal-edit add hspec --section test-suite --dev
```

### Removing Dependencies

```bash
# Remove a dependency
cabal-edit rm old-package
```

### Upgrading Dependencies

```bash
# Upgrade all dependencies to latest versions
cabal-edit upgrade

# Dry run (see what would change)
cabal-edit upgrade --dry-run

# Upgrade a specific package
cabal-edit upgrade aeson
```

### Output Control

```bash
# Suppress all output (useful for scripts)
cabal-edit add aeson --quiet

# Enable verbose logging
cabal-edit add aeson --verbose
```

### Workspace Support

`cabal-edit` supports multi-package workspaces defined in `cabal.project`.

```bash
# Apply command to all packages in the workspace
cabal-edit -w add lens

# Upgrade everything in the workspace
cabal-edit -w upgrade
```

## Features

- ✅ **Smart Add**: Resolves latest versions from Hackage automatically.
- ✅ **Remove**: Safely removes dependencies from specific sections.
- ✅ **Upgrade**: Bulk upgrade dependencies with dry-run support.
- ✅ **Workspace Support**: Manages multiple packages via `cabal.project`.
- ✅ **Format Preservation**: Uses surgical edits to preserve comments and indentation.
- ✅ **Leading Comma Style**: Enforces leading commas for cleaner diffs.
- ✅ **Safety**: 
    - File locking prevents race conditions.
    - Automatic backups (`.bak`).
    - In-memory verification validates changes before writing to disk.

## Development

```bash
# Build
cabal build

# Test
cabal test
```

## License

MIT