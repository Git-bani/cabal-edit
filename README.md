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

# Add to a specific section (library, executable, test-suite, benchmark)
cabal-edit add hspec --section test-suite --dev

# Add to a conditional block
cabal-edit add Win32 --if "os(windows)"

# Dry run (see changes without writing to disk)
cabal-edit add lens --dry-run
```

### Removing Dependencies

```bash
# Remove a dependency
cabal-edit rm old-package

# Remove from a specific section
cabal-edit rm old-package --section executable:my-exe
```

### Upgrading Dependencies

```bash
# Upgrade all dependencies to latest versions
cabal-edit upgrade

# Interactive mode (select which packages to upgrade)
cabal-edit upgrade -i

# Dry run (see what would change)
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

## Features

- ✅ **Smart Add**: Resolves latest versions from Hackage automatically.
- ✅ **Surgical Remove**: Safely removes dependencies while preserving file structure.
- ✅ **Bulk Upgrade**: Upgrade dependencies with intelligent version resolution.
- ✅ **Flag Management**: Easily add, enable, or disable Cabal flags.
- ✅ **Version Management**: Set project version from the CLI.
- ✅ **Hpack Support**: Automatically detects and edits `package.yaml` files when present.
- ✅ **Workspace Support**: Full support for `cabal.project` and multi-package setups.
- ✅ **Format Preservation**: Preserves comments, indentation, and leading/trailing comma styles.
- ✅ **Advanced Safety**: 
    - **Atomic Writes**: Uses temporary files and atomic moves to prevent file corruption.
    - **In-memory Verification**: Validates Cabal file syntax before committing changes.
    - **TTY Detection**: Clean output in non-terminal environments (no junk characters).
    - **Dry Run Support**: Every modifying command supports `--dry-run`.

## Development

```bash
# Build
cabal build

# Test
cabal test
```

## License

MIT