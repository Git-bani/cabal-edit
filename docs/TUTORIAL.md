# cabal-edit Tutorial

## Introduction

`cabal-edit` allows you to manage your Haskell project dependencies directly from the command line, similar to `cargo-edit` for Rust or `npm` for Node.js.

## Installation

```bash
cabal install cabal-edit
```

## Basic Usage

### 1. Adding a Dependency

To add a library to your project:

```bash
cabal-edit add aeson
```

This will:
1. Fetch the latest version of `aeson` from Hackage.
2. Add it to the `build-depends` section of your `.cabal` file.
3. Preserve your existing formatting and comments.

**Adding with constraints:**

```bash
cabal-edit add text --version "^>= 1.2"
```

**Adding to a specific section:**

```bash
cabal-edit add hspec --section test-suite --dev
```

### 2. Removing a Dependency

To remove a library:

```bash
cabal-edit rm old-library
```

### 3. Upgrading Dependencies

To upgrade all dependencies to their latest versions:

```bash
cabal-edit upgrade
```

To see what would change without actually writing to the file:

```bash
cabal-edit upgrade --dry-run
```

## Output Control

For scripting or automated environments, you might want to suppress output:

```bash
cabal-edit add aeson --quiet
```

Or enable verbose logging for debugging:

```bash
cabal-edit add aeson --verbose
```

## Advanced Usage

### Workspace Support

If you have a `cabal.project` file managing multiple packages:

```bash
# Add a dependency to ALL packages in the workspace (where applicable)
cabal-edit -w add lens

# Upgrade all packages in the workspace
cabal-edit -w upgrade
```

### Configuration

You can customize `cabal-edit` via `~/.cabal-edit/config.json`.
(See API documentation for config details).
