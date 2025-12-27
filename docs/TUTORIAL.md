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

You can target specific components like executables, test suites, or benchmarks. Use the `--section` flag with the component type and name:

```bash
# Add to an executable
cabal-edit add lens --section exe:my-app

# Add to a test suite
cabal-edit add hspec --section test:unit-tests

# Add to a benchmark
cabal-edit add criterion --section bench:performance
```

**Adding to conditional blocks:**

You can add a dependency directly to an `if` block using the `--if` flag:

```bash
cabal-edit add Win32 --if "os(windows)"
```

Alternatively, use the `--flag` shorthand to add a dependency conditional on a specific Cabal flag:

```bash
cabal-edit add lens --flag use-lens
```

If the conditional block doesn't exist within the targeted section, `cabal-edit` will create it for you at the end of that section.

### 2. Removing a Dependency

To remove a library:

```bash
cabal-edit rm old-library
```

### Interactive Removal

You can visually select multiple dependencies to remove using the interactive mode:

```bash
cabal-edit rm -i
```

This opens a TUI checklist. Use **Arrow Keys** to navigate, **Space** to toggle, and **Enter** to confirm removal.

### 3. Upgrading Dependencies

To upgrade all dependencies to their latest versions on Hackage:

```bash
cabal-edit upgrade
```

### Interactive Upgrade

Selective upgrades are supported via the interactive mode:

```bash
cabal-edit upgrade -i
```

## Advanced Features

### Managing Cabal Flags

`cabal-edit` provides a dashboard and commands for managing flags in your `.cabal` file:

### Flag Dashboard

Open a visual dashboard to toggle existing flags:

```bash
cabal-edit flag -i
```

This allows you to quickly switch flag defaults between `True` and `False` without hunting through the file.

### CLI Flag Operations

```bash
# Add a new flag (default: False, manual: True)
cabal-edit flag add my-feature

# Enable an existing flag (sets default: True)
cabal-edit flag enable my-feature

# Disable an existing flag (sets default: False)
cabal-edit flag disable my-feature

# Remove a flag stanza completely
cabal-edit flag remove my-feature
```

### Dry Runs & Safety

**Dry Runs:**
Preview changes with **colorized diffs** without writing to disk by using the `--dry-run` flag. This is supported by all modifying commands (`add`, `rm`, `upgrade`, `set-version`, `flag`).

```bash
cabal-edit add lens --dry-run
```

The output will show a `git diff` style view, with removed lines in Red and added lines in Green.

**Surgical Edits:**
Unlike many tools that re-render the entire file (and lose your comments/formatting), `cabal-edit` performs "surgical" text edits. It identifies the exact lines to change and modifies them while keeping the rest of the file untouched.

**Atomic Writes:**
To prevent file corruption during power failures or crashes, `cabal-edit` writes to a temporary file first and then atomically moves it to the final destination.

### Hpack Support

If a `package.yaml` file is present, `cabal-edit` will automatically prioritize it. Commands like `add` and `rm` will modify your `package.yaml` instead of the `.cabal` file, ensuring that your changes aren't lost the next time Hpack generates the Cabal file.

## Workspace Support

Manage multiple packages in a `cabal.project` workspace:

```bash
# Add a dependency to ALL packages in the workspace
cabal-edit --workspace add lens

# Target specific packages within the workspace
cabal-edit --package my-lib --package my-app upgrade
```


## Configuration

You can customize `cabal-edit` via `~/.cabal-edit/config.json`.

```json
{
  "hackage_url": "https://hackage.haskell.org",
  "leading_comma": true
}
```
