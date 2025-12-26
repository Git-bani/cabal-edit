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

You can target specific sections like executables or test suites:

```bash
cabal-edit add hspec --section test:my-test-suite
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

## Advanced Features

### Managing Flags

Cabal flags can be managed directly:

```bash
# Add a flag
cabal-edit flag add manual-feature

# Enable it (sets default: True)
cabal-edit flag enable manual-feature
```

### Setting Project Version

Quickly update your package version:

```bash
cabal-edit set-version 1.0.0
```

### Dry Runs

If you're unsure about a change, use `--dry-run` to see a preview without modifying any files:

```bash
cabal-edit add lens --dry-run
```

### Hpack Integration

If you use `hpack` (`package.yaml`), `cabal-edit` will detect it and issue a warning. Since Hpack manages the `.cabal` file, manual changes made by `cabal-edit` might be overwritten the next time `hpack` runs.

## Workspace Support

If you have a `cabal.project` file managing multiple packages:

```bash
# Add a dependency to ALL packages in the workspace
cabal-edit -w add lens

# Target specific packages in the workspace
cabal-edit -p my-lib -p my-app upgrade
```

## Configuration

You can customize `cabal-edit` via `~/.cabal-edit/config.json`.

```json
{
  "hackage_url": "https://hackage.haskell.org",
  "leading_comma": true
}
```
