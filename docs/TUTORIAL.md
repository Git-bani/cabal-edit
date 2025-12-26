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

**Adding to conditional blocks:**

You can add a dependency directly to an `if` block:

```bash
cabal-edit add Win32 --if "os(windows)"
```

If the block doesn't exist, `cabal-edit` will create it for you at the end of the section.

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

### Interactive Upgrade

You can selectively upgrade dependencies using the interactive mode:

```bash
cabal-edit upgrade -i
```

This will present a list of all upgradeable dependencies, allowing you to toggle them using the spacebar before confirming with Enter.

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

### Hpack Support

If you use `hpack` (`package.yaml`), `cabal-edit` will automatically detect it. Instead of modifying the `.cabal` file (which would be overwritten by `hpack`), `cabal-edit` will surgically edit your `package.yaml` to add or remove dependencies, preserving your YAML formatting and comments.

A warning will still be displayed to remind you that the tool is operating on the Hpack configuration.

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
